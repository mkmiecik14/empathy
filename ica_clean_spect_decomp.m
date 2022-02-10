% EEG ICA cleaning and Spectral Decomposition Step 4 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 07 FEB 2022

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);
iter=1; % for testing purposes 
csd_switch = 1; % 1 == CSD will be computed

% Credit: https://github.com/dazza-codes/bioelectromagnetism/blob/master/elec_sph2cart.m
[X,Y,Z] = elec_sph2cart(theta,phi,r,degrees)

[X,Y,Z] = elec_sph2cart(0, 0, 1, 1)

for iter = 1:num_iters
    
    % Creating variables
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
    % removes artifactual ICs
    this_reject = find(EEG.reject.gcompreject);
    EEG = pop_subcomp(EEG, this_reject, 0);
    
    if csd_switch == 1
    
        % Compute surface laplacian spatial filter ----
        % Calculating Surface Laplacian via CSD toolbox functions
        % http://psychophysiology.cpmc.columbia.edu/software/CSDtoolbox/tutorial.html#PrepareInput
        chan_mont = cell(64,1); % initializes cell array

        % Fills cell array with electrode labels
        for j = 1:size(chan_mont)
            chan_mont(j) = cellstr(EEG.chanlocs(j).labels);
        end

        % Derives spherical coordinates via CSD toolbox fx ExtractMontage()
        % csd_mont = ExtractMontage('10-5-System_Mastoids_EGI129.csd', chan_mont);
        % To view: MapMontage(csd_mont)
        lab = {EEG.chanlocs.labels};
        theta = [EEG.chanlocs.theta]';
        phi = [EEG.chanlocs.sph_phi]';
        xy = [EEG.chanlocs.X; EEG.chanlocs.Y]';

        % Creates the M variable for CSD calculations
        M.lab = lab;
        M.theta = theta;
        M.phi = phi;
        M.xy = xy;
        MapMontage(M)

        [G, H] = GetGH(M); % Calculates G and H matrices

        % Applies surface laplacian to EEG data
        EEG.data = CSD(EEG.data, G, H, 1.0e-5, 10); % lambda left at default, 10 = cm head size, so units are microvolt/cm^2
    
    else
        disp('CSD skipped ...');
    end
    
    % Spectral decomposition ----
    % Epoching ----
    % selecting the stimulation blocks (20 second epochs)
    blocks = {'S  1' 'S  2' 'S  3' 'S  4' 'S  5'};
    
    % preallocates arrays
    this_spectra = zeros(EEG.nbchan, EEG.srate+1, length(blocks));
    this_freqs = zeros(EEG.srate+1, 1, length(blocks));
    
    for j = 1:length(blocks)
        this_EEG = pop_epoch(EEG, blocks(j), [0 20],'epochinfo', 'yes');
        [this_spectra(:,:,j), this_freqs(:,:,j)] = spectopo(...
            this_EEG.data(:,:), ... 
            0, ... % frames per epoch
            this_EEG.srate, ... % sampling rate
            'winsize', 2*this_EEG.srate, ... % winsize is 2 seconds
            'overlap', this_EEG.srate, ... % overlap is 1 second
            'plot','off'... % toggles plot
            ); 
        % troubleshooting plot
        figure; pop_spectopo(this_EEG,1,[],'EEG','freq',[10 12 24 25 50],'freqrange',[0 75],'electrodes','on');
        saveas(gcf, fullfile(spec_res_outpath, strcat(num2str(this_ss),'-',num2str(j),'.png')));
        close; % closes figure
        
    end
    
    % Saving out results ----
    % combines into one variable
    % stimulation results are stored differently with each index being the
    % stimulation strength in order: 1, 2, 3, 4, 5
    spec_res.spectra   = this_spectra; % 3D mat of spectra
    spec_res.freqs     = this_freqs;   % 3D mat of freqs bins

    % Saving out all data ----
    spec_outname = strcat(num2str(this_ss), '-vis-spec-res.mat');
    save(fullfile(spec_res_outpath, spec_outname),'spec_res'); % saves out as matlab struct
    
end