% EEG ICA cleaning and Spectral Decomposition Step 4 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 07 FEB 2022

workspace_prep % Prepares workspace

% Preallocation ----
num_iters = size(NUM, 1); % number of participants in this batch
iter=15; % for testing purposes 
csd_switch = 1; % 1 == CSD will be computed
plot_switch = 1; % 1 == PSD plots will be saved

for iter = 1:num_iters
    
    % Creating variables ----
    this_ss = NUM(iter);
    this_ss_path = dir(fullfile(ica_outpath, strcat(num2str(this_ss), '-visual-ica.set')));
    this_ss_name = this_ss_path.name;
        
    % Loads in data using EEGLAB ----
    EEG = pop_loadset('filename',this_ss_name,'filepath', this_ss_path.folder);
    
    % Labels ICs for rejection ----
    EEG = pop_iclabel(EEG, 'default');
    EEG = pop_icflag(EEG, ...
        [NaN NaN;... % brain
        NaN NaN;... % muscle
        0.8 1;... % eye ( > 80% probability will reject components
        NaN NaN;... % heart
        NaN NaN;... % line noise
        NaN NaN;... % channel noise
        NaN NaN... % Other
        ]);
    
    % Removes artifactual ICs
    this_reject = find(EEG.reject.gcompreject);
    EEG = pop_subcomp(EEG, this_reject, 0);
    eeglab redraw
   
    if csd_switch == 1
    
    % Compute surface laplacian spatial filter ----
    % Calculating Surface Laplacian via CSD toolbox functions
    % http://psychophysiology.cpmc.columbia.edu/software/CSDtoolbox/tutorial.html#PrepareInput
    % I first saved the template channel locations from easycap as a 
    % *.ced into 'output/easycap.ced'). Then I used the CSD Toolbox
    % function to convert from *.ced to *.csd:
    % ConvertLocations('output/easycap.ced') % not run here

    % Next, I derived the spherical coordinates via CSD toolbox fx 
    % ExtractMontage() using the converted *.csd and the labels:
    M = ExtractMontage('output/easycap.csd', {EEG.chanlocs.labels}');

    % To plot the montage to ensure accuracy:
    % MapMontage(M)

    [G, H] = GetGH(M); % Calculates G and H matrices

    % Applies surface laplacian to EEG data
    % lambda left at default, 10 = cm head size, so units are microvolt/cm^2
    EEG.data = CSD(EEG.data, G, H, 1.0e-5, 10); 
    
    else
        disp('CSD skipped....');
    end
    
    % Spectral decomposition ----
    % Epoching ----
    % selecting the stimulation blocks (20 second epochs)
    blocks = {'S  1' 'S  2' 'S  3' 'S  4' 'S  5'};
    
    % preallocates arrays
    this_spectra = zeros(EEG.nbchan, EEG.srate+1, length(blocks));
    this_freqs = zeros(EEG.srate+1, 1, length(blocks));
    
    % If data were recorded in the small room, then epochs are adjusted:
    if NUM(iter, 3) == 0
        this_epoch = [4 24]; % epochs are shifted by four seconds in small room
    else
        this_epoch = [0 20]; % big room e-prime script has correct timing
    end
    
    for j = 1:length(blocks)
       
        try % This will run if the block exists
            % Selects blocks (in order)
            this_EEG = pop_epoch(EEG,blocks(j),this_epoch,'epochinfo', 'yes');
            % Spectral decomposition here
            [this_spectra(:,:,j), this_freqs(:,:,j)] = spectopo(...
                this_EEG.data(:,:), ... 
                0, ... % frames per epoch
                this_EEG.srate, ... % sampling rate
                'winsize', 2*this_EEG.srate, ... % winsize is 2 seconds
                'overlap', this_EEG.srate, ... % overlap is 1 second
                'plot','off'... % toggles plot
                );
            % Plots for troubleshooting (if needed)
            if plot_switch == 1
                figure; pop_spectopo(this_EEG,1,[],'EEG','freq',[10 12 24 25 50],'freqrange',[0 75],'electrodes','on');
                saveas(gcf, fullfile(spec_res_outpath, strcat(num2str(this_ss),'-',num2str(j),'.png')));
                close; % closes figure
            else
                % plots not saved
            end
        
        catch % if the block is missing, then the matrix is filled with NaN
            this_spectra(:,:,j) = NaN; % fills with missing values
            this_freqs(:,:,j) = NaN; % fills with missing values
     
        end
            
    end
    
    % Saving out results ----
    % combines into one variable
    % stimulation results are stored with each index being the
    % stimulation strength in order: 1, 2, 3, 4, 5
    spec_res.spectra   = this_spectra; % 3D mat of spectra
    spec_res.freqs     = this_freqs;   % 3D mat of freqs bins

    % Saving out all data ----
    spec_outname = strcat(num2str(this_ss), '-vis-spec-res.mat');
    save(fullfile(spec_res_outpath, spec_outname),'spec_res'); % saves out as matlab struct
end