% EEG Preprocessing Pipeline Step 1 - Visual Task
% This script has the additional ability to check if the recording was
% recorded with a channel 10 reference instead of the usual referenceless
% recording
% Matt Kmiecik
% Started 11 MAY 2022

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
    
    % Deleting EOG and Photo channels ----
    EEG = pop_select( EEG, 'nochannel',{'EOG','Photo'});
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
   
    % Importing channel locations ----
    % uses the template file that came with the easycaps
    chanlocs = loadbvef(chan_loc_path); % without reference
    EEG.chanlocs = chanlocs(horzcat(2:10,12:65)); % does not include ground (GND)
    
    % Adds channel 10
    EEG=pop_chanedit(EEG, 'append',63,'changefield',{64,'labels','10'});
    
    % Averaged mastoid reference and puts channel 10 back in
    EEG = pop_reref(EEG, [30 62], 'refloc', ...
        struct('labels',{'10'},'sph_radius',{[1]},'sph_theta',{[16]},...
        'sph_phi',{[44]},'theta',{[-16]},'radius',{[0.255555555555556]},...
        'X',{[0.691473796429464]},'Y',{[0.198276920499274]},'Z',{[0.694658370458997]},'ref',{''},...
        'type',{''},'urchan',{[]},'datachan',{0},...
        'sph_theta_besa',{[]},'sph_phi_besa',{[]}));
    
    % reorders channel locations so that chan 10 is in the correct place
    chanlocs_reordered = EEG.chanlocs(horzcat(1:9, 62, 10:61));
    EEG.chanlocs = chanlocs_reordered;
    
    % Removing DC offset ----
    % subtracts the mean signal from each electrode
    EEG = pop_rmbase(EEG, [],[]);
   
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
