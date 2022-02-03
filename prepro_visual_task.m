% EEG Preprocessing Pipeline Step 1 - Visual Task
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 03 FEB 2022

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);
%iter = 1; % for testing purposes only

for iter = 1:num_iters
    
    % Creating variables
    this_ss = NUM(iter);
    this_ss_path = dir(fullfile(data_dir, num2str(this_ss), 'v*.vhdr'));
    this_ss_name = this_ss_path.name;
        
    % Loads in raw data using loadbv() from BrainVision plugin ----
    EEG = pop_loadbv(this_ss_path.folder, this_ss_name, [], []);
    [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname',this_ss_name,'gui','off');
    eeglab redraw
    
    % Checks to see if participant has correct number of channels
    if EEG.nbchan < 64
        disp("This participant has < 64 channels");
    end
    
    % Re-referencing ----
    
    % ! UPDATE THIS CODE AND ALSO EDIT THE NAME BEFORE SAVE OUT ! %
    
    % Per: https://sccn.ucsd.edu/wiki/I.4:_Preprocessing_Tools
    % First step is to compute common average reference:
%     EEG = eeg_checkset( EEG );
%     EEG = pop_reref( EEG, [],'refloc',struct('labels',{'TP9'},'sph_radius',{1},'sph_theta',{108},'sph_phi',{-23},'theta',{-108},'radius',{0.6278},'X',{-0.2845},'Y',{0.8755},'Z',{-0.3907},'type',{''},'ref',{''},'urchan',{[]},'datachan',{0}));
%     [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off'); 
% 
%     % Next compute linked average mastoid
%     EEG = pop_reref( EEG, [20 34] ,'keepref','on');
%     [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off'); 

    % Deleting EOG and Photo
    EEG = pop_select( EEG, 'nochannel',{'EOG','Photo'});
    [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
        
    % Deleting Channel 10 for consistency
    % This is because channel 10 is digitized but is not in the raw data
    % because it serves as a reference
    %EEG = pop_select( EEG, 'nochannel',{'Ch10'});
    %[ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
        
    % Importing channel locations
    % usetemplate-dig
    EEG=pop_chanedit(EEG, 'load', ...
        {dig_file,...
        'filetype','autodetect'},'changefield',{4,'datachan',0},...
        'changefield',{37,'datachan',0});
    [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

    % Removing DC offset by subtracting the mean signal from each electrode
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
     
     % Saves out data for visual inspection
     outname = strcat(num2str(this_ss),'-visual-prepro.set'); % save out subject name
     EEG = pop_saveset( EEG, 'filename', outname, 'filepath' ,prepro_outpath);
end
