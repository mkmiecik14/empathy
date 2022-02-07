% EEG ICA cleaning and Spectral Decomposition Step 4 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 07 FEB 2022

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);
iter=1; % for testing purposes 

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
    
    % selecting the stimulation blocks
    EEG = pop_rmdat(EEG, {'S  1','S  2','S  3','S  4','S  5'}, [0 20], 0);
    
%     % Compute surface laplacian spatial filter ----
%     % Calculating Surface Laplacian via CSD toolbox functions
%     % http://psychophysiology.cpmc.columbia.edu/software/CSDtoolbox/tutorial.html#PrepareInput
% 
%     chan_mont = cell(32,1); % initializes cell array
% 
%     % Fills cell array with electrode labels
%     for j = 1:size(chan_mont)
%         chan_mont(j) = cellstr(EEG.chanlocs(j).labels);
%     end
% 
%     % Derives spherical coordinates via CSD toolbox fx ExtractMontage()
%     csd_mont = ExtractMontage('10-5-System_Mastoids_EGI129.csd', chan_mont);
%     % To view: MapMontage(csd_mont)
% 
%     [G, H] = GetGH(csd_mont); % Calculates G and H matrices


[spectra, freqs] = spectopo(EEG_bl.data(:,:,:), 0, EEG_bl.srate, 'winsize', 2*EEG_bl.srate, 'plot', 'off'); % winsize is 2 seconds
    
end