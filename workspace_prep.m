% Workspace Preparation
% This script is meant to be run at the beginning of each script in this
% project to prepare MATLAB with paths and other code that is redundant in
% each script
%
% Matt Kmiecik
%
% Started 5 OCTOBER 2021
%

% Sets working directory ----
main_dir = 'C:\Projects\empathy\'; % creating the main dir
cd(main_dir); % setting the working directory
data_dir = fullfile(main_dir, 'data\'); % creating the data folder

% Creates directories necessary for this project
output_dir = fullfile(main_dir, 'output\'); % creating the output dir
mkdir(output_dir); % making the output dir

% Starts EEGLAB ----
[ALLEEG, EEG, CURRENTSET, ALLCOM] = eeglab;

% Loads in participant information ----
[NUM,TXT,RAW] = xlsread('data/id-notes.xlsx');