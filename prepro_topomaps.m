% EEG Preprocessing Pipeline Step 1
% Sarah Darnell, Kaela Harber, & Matt Kmiecik
% Started 5 OCTOBER 2021

workspace_prep % Prepares workspace

% Preprocessing ----
num_iters = size(NUM, 1);

for iter = 1:num_iters
    
    try

        %iter = 2; % for testing purposes only

        % Creating variables
        this_ss = NUM(iter);
        this_ss_path = dir(fullfile(data_dir, num2str(this_ss), 'v*.vhdr'));
        this_ss_name = this_ss_path.name;
        % If there were any bad channels identified during inspection, they are
        % imported here:
        interpchans = str2num(RAW{iter+1,2}); % gathers bad channels from excel
         
        % Loads in raw data using loadbv() from BrainVision plugin ----
        EEG = pop_loadbv(this_ss_path.folder, this_ss_name, [], []);
        [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 0,'setname',this_ss_name,'gui','off');
        eeglab redraw

        if EEG.nbchan < 34
            % Configuring channel locations ----
            % TP9 is the online electrode (channel 10) but is not included in recording
            % chanlocs = loadbvef('C:\Users\pains\Desktop\matt-eeglab\data\elecs\matts-32-chan.bvef'); % without TP9
            chanlocs = loadbvef(fullfile(data_dir, 'digitizer', 'matts-32-chan-TP9.bvef')); % with TP9

            % Enters the channel locations for all except TP9
            EEG.chanlocs = chanlocs(1:33); % All but TP9

            % Adds TP9 as channel, sets it as reference, and sets its location manually
            EEG=pop_chanedit(EEG, 'append',33,'changefield',{34 'labels' 'TP9'},'changefield',{34 'sph_radius' '1'},'changefield',{34 'sph_theta' '108'},'changefield',{34 'sph_phi' '-23'},'changefield',{34 'Z' '-0.3907'},'changefield',{34 'Y' '0.8755'},'changefield',{34 'X' '-0.2845'},'changefield',{34 'radius' '0.6278'},'changefield',{34 'theta' '-108'},'setref',{'1:31' 'TP9'});
            [ALLEEG, EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

        end % END IF STATEMENT HERE

        % Deleting EOG and Photo
        EEG = pop_select( EEG, 'nochannel',{'EOG','Photo'});
        [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
        
        % Deleting Channel 10 for consistency
        EEG = pop_select( EEG, 'nochannel',{'Ch10'});
        [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
        
        % Importing channel locations
        % usetemplate-dig
        EEG=pop_chanedit(EEG, 'load',{'C:\\Projects\\empathy\\data\\digitizer\\template-dig.elp', ...
            'filetype','autodetect'},'changefield',{4,'datachan',0},'changefield',{37,'datachan',0},...
            'changefield',{14,'datachan',0});
        [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

        % Removing DC offset by subtracting the mean signal from each electrode
        EEG = pop_rmbase(EEG, [],[]);

        % Downsampling to 256 Hz ----
        EEG = pop_resample(EEG, 256);

        % Highpass filter at 1Hz (-6dB @ 1Hz, 425 point highpass, 2Hz transition band width)
        EEG = pop_eegfiltnew(EEG,'locutoff',2,'plotfreqz',0);

        % Cleanline - Removing electrical line noise @ 60 Hz
        EEG = pop_cleanline(EEG, 'bandwidth',2,'chanlist',[1:EEG.nbchan] ,'computepower',1,'linefreqs',60,'normSpectrum',0,'p',0.01,'pad',2,'plotfigures',0,'scanforlines',1,'sigtype','Channels','tau',100,'verb',1,'winsize',4,'winstep',1);

        % Extracting S5 block
        EEG = pop_epoch( EEG, {  'S  5'  }, [0  20], 'newname', 'visual_2_0_12219 resampled epochs', 'epochinfo', 'yes');
        [ALLEEG EEG CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off'); 
        EEG = eeg_checkset( EEG );

        % Interpolates bad channels if bad channels were identified
        if sum(size(interpchans, 2)) > 0 % checks to see if there are bad channels
            disp('Bad channel(s) detected...interpolating channels...');
            EEG = pop_select( EEG, 'nochannel', interpchans);
            [ALLEEG, EEG, CURRENTSET] = pop_newset(ALLEEG, EEG, 1,'overwrite','on','gui','off');
        else
            disp('No bad channels detected...')
        end

        % Topo maps ----
        % Generates a black to white to orange colormap
        spooky_colors = colMapGen([255 165 0], [0 0 0], 10, 'midCol',[255 255 255])/255;

        % Computes the power spectrum
        [spectra, freqs] = spectopo(EEG.data, 5120, EEG.srate, ...
            'winsize', 2*EEG.srate,'plot','off'); % winsize is 2 seconds
        figure; 
        topoplot(spectra(:, 51), EEG.chanlocs, ... % isolates the 25Hz signal
            'electrodes', 'off', 'shading', 'interp', 'whitebk', 'on', ...
            'colormap', spooky_colors, ...
            'noplot', 'off'); % remove this part if you want to plot (or set to off)
        %colorbar; % uncomment to see the scale

        % Saving out topomaps
        halloween_dir = fullfile(output_dir, 'halloween-topomaps\'); % halloween dir
        if ~exist(halloween_dir, 'dir') % create dir only if it does not exist
               mkdir(halloween_dir);
        end

        saveas(gcf, fullfile(halloween_dir, strcat('id-', num2str(this_ss), '.png'))); % saves as png
        close; % closes figure if printed
    
    catch ME
        % Saves out last error message at *.mat file
        save(fullfile(output_dir, 'halloween-topomaps', strcat('me-', num2str(this_ss),'.mat')), 'ME')
    end
    
end
