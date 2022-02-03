% EEG ICA Step 3 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 03 FEB 2022

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);
iter=1; % for testing purposes

for iter = 1:num_iters
    
    % Creating variables
    this_ss = NUM(iter);
    this_ss_path = dir(fullfile(prepro_outpath, strcat(num2str(this_ss), '-visual-prepro.set')));
    this_ss_name = this_ss_path.name;
        
    % Loads in raw data using EEGLAB ----
    EEG = pop_loadset('filename',this_ss_name,'filepath', this_ss_path.folder);
    
    % If there were any bad channels identified during inspection, they are
    % imported here:
    interpchans = str2num(RAW{iter+1,2}); % gathers bad channels from excel
    
    % Interpolates bad channels if bad channels were identified
    if sum(size(interpchans, 2)) > 0 % checks to see if there are bad channels
            disp('Bad channel(s) detected...interpolating channels...');
            EEG = pop_interp(EEG, interpchans, 'spherical');
    else
            disp('No bad channels detected...')
    end
    
    % ICA decomposition ----
    % compute rank
    EEG = pop_runica(EEG, 'icatype', 'runica', 'extended',1,'interrupt','on');
    
    
    
end

