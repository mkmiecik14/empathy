% Script to calculate X Y Z coordinates using EasyCap theta and phi
% Matt Kmiecik
% 10 FEB 2022

% read in excel file with the theta and phi

% Credit: https://github.com/dazza-codes/bioelectromagnetism/blob/master/elec_sph2cart.m
[X,Y,Z] = elec_sph2cart(theta,phi,r,degrees)

% use radius of 1
[X,Y,Z] = elec_sph2cart(0, 0, 1, 1)