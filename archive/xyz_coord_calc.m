% Script to calculate X Y Z coordinates using EasyCap theta and phi
% Matt Kmiecik
% 10 FEB 2022

% Prepares workspace ----
workspace_prep

% Loads in excel file with theta and phi from Easycap ----
[eNUM,eTXT,eRAW] = xlsread('data/easycap_elec_positions.xlsx');

% Converts phi and theta to Cartesian coordinates ----
% Credit: https://github.com/dazza-codes/bioelectromagnetism/blob/master/elec_sph2cart.m
radius = 1;
X = size(eNUM, 1); Y = X; Z = X;
for i = 1:size(eNUM, 1)
    this_theta = eNUM(i, 3); % finds this electrode theta
    this_phi = eNUM(i, 4); % finds this electrode phi
    [X(i),Y(i),Z(i)] = elec_sph2cart(this_theta, this_phi, radius, 1); % computes xyz
end

% Saves out XYZ coordinates for this montage ----
easycap_xyz = [X; Y; Z]';
save('output/easycap_xyz.mat', 'easycap_xyz');
