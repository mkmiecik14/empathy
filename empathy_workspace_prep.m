% Workspace Preparation
% This script is meant to be run at the beginning of each script in this
% project to prepare MATLAB with paths and other code that is redundant in
% each script
%
% Matt Kmiecik
%
% Started 28 Jan 2020
% Began modification 24 Feb 2021

% Directory paths ----
% Update these to reflect your machine
proj_path = 'M:\empathy\public\';
eeglab_path = 'C:\Users\Matthew\Documents\MATLAB\eeglab2019_1\';
eeg_data_path = fullfile(proj_path, 'data\eeg\');
elp_data_path = fullfile(proj_path, 'data\elps\');

% Adding paths for MATLAB to gain access to functions ----
addpath(eeglab_path);       % EEGLAB
% add more as necessary

% Sets working directory if not done already
cd(proj_path);

% Starts EEGLAB ----
[ALLEEG, EEG, CURRENTSET] = eeglab; % ensure that it has been added to path

% Loads in participant information ----
% [NUM,TXT,RAW] = xlsread('C:\Users\pains\Desktop\matt-eeglab\data\0-global-vars\vis-subj-info.xlsx');

% Creating directories (if they don't exist) ----

% preprocessing output
% prepro_outpath = fullfile(proj_path, 'output/1-prepro/'); 
% if ~exist(prepro_outpath, 'dir')   % checks if directory does not exist
%     mkdir(prepro_outpath)          % creates it if it doesn't
% end
