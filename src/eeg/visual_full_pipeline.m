% Full Visual Pipeline
% Matt Kmiecik
% Started 28 APRIL 2022

% Purpose: to run the entire visual EEG processing pipeline on the
% participants in batch. This runs each step back to back.
% I'd recommend using this if a participant needs to be re-run through the
% pipleine after making some changes to settings.

% Step 1 - preprocessing
prepro_visual_task
clear % clears workspace after finished

% Step 2 - spherical interpolation and ICA decomposition
ica_visual_task
clear % clears workspace after finished

% Step 3 - artifact correction, surface Laplacian filter, and spectral
% decomposition
spect_decomp_visual_task
clear % clears workspace after finished