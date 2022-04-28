% EEG Preprocessing Pipeline Step 1 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 03 FEB 2022

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);
iter = 1; % for testing purposes only

for iter = 1:num_iters
    
    % Creating variables ----
    this_ss = NUM(iter);
    this_ss_path = dir(fullfile(data_dir, num2str(this_ss), 'v*.vhdr'));
    this_ss_name = this_ss_path.name;
        
    % Loads in raw data using loadbv() from BrainVision plugin ----
    EEG = pop_loadbv(this_ss_path.folder, this_ss_name, [], []);
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname',this_ss_name,'gui','off');
    eeglab redraw
    
    % Checks to see if participant has correct number of channels ----
    if EEG.nbchan < 64
        disp("This participant has < 64 channels");
    end
    
    % Deleting EOG and Photo channels ----
    EEG = pop_select( EEG, 'nochannel',{'EOG','Photo'});
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
    
    % Importing channel locations ----
    % uses the template file that came with the easycaps
    chanlocs = loadbvef(chan_loc_path); % without reference
    EEG.chanlocs = chanlocs(2:65); % does not include ground (GND)

    % Removing DC offset ----
    % subtracts the mean signal from each electrode
    EEG = pop_rmbase(EEG, [],[]);

    % Re-referencing ----
    % Averaged mastoid reference
    EEG = pop_reref(EEG, [31 63] ,'keepref', 'off');
   
    % Downsampling to 256 Hz ----
    EEG = pop_resample(EEG, 256);

    % Highpass filter at 1Hz (-6dB @ 1Hz, 425 point highpass, 2Hz transition band width)
    EEG = pop_eegfiltnew(EEG,'locutoff',2,'plotfreqz',0);

    % Cleanline - Removing electrical line noise @ 60 Hz
    EEG = pop_cleanline(EEG, 'bandwidth',2,'chanlist',[1:EEG.nbchan],...
         'computepower',1,'linefreqs',60,'normSpectrum',0,'p',0.01,'pad',...
         2,'plotfigures',0,'scanforlines',1,'sigtype','Channels',...
         'tau',100,'verb',1,'winsize',4,'winstep',1);
    
    % renames dataset
    dataset_name = strcat('visual-', num2str(this_ss), '-prepro');
    EEG = pop_editset(EEG, 'setname', dataset_name, 'run', []);
     
    % Saves out data for visual inspection
    outname = strcat(num2str(this_ss),'-visual-prepro.set'); % save out subject name
    EEG = pop_saveset( EEG, 'filename', outname, 'filepath' ,prepro_outpath);
   
    eeglab redraw % redraws to GUI for convenience
    
end
