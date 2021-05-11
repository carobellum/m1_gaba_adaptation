% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% Script name:  results_robust_recalculation.m
% 
% Author:       Caroline Nettekoven, 2021
% Contact:      nettekoven-enquiries@web.de 
% 
% Description:  Script to re-calculate main correlation results of the 
%               ultra-high field (7T) MRS project ingestigating M1 GABA
%               and functional connectivity in visuomotor adaptation in
%               humans with the Robust Correlations Toolbox [1]
% 
%               Robust correlation methods were selected for their
%               suitability to the given data structure according to
%               criteria described in [1].
%               
% 
%               1. Pernet, C. R., Wilcox, R. & Rousselet, G. A. Robust
%               correlation analyses: False positive and power validation
%               using a new open source matlab toolbox. Front. Psychol. 3,
%               1–18 (2013).
% 
%               Toolbox available at:
%               https://sourceforge.net/projects/robustcorrtool/files/
% 
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
% ----------------------------------------------------------------------
% ----------------------------------------------------------------------
% Dependencies
addpath(genpath('/Users/CN/Documents/Projects/Joystick_M1_MRS/Analysis/m1_manuscript_code/Corr_toolbox_v2'))

% ----------------------------------------------------------------------
% Load data
load("/Users/CN/Documents/Projects/Joystick_M1_MRS/Analysis/m1_gaba_adaptation/data_rotation.mat")


% ----------------------------------------------------------------------
% Calculate robust correlation
% ++ Toggle selection to calculate correlation for different data pairs ++

% Raw data
correlation_results = robust_correlation(data.GABA,data.Retention)
% correlation_results = robust_correlation(data.Retention,data.M1_Cerebellar_ConnChange)
% correlation_results = robust_correlation(data.GABA,data.M1_Cerebellar_ConnChange)

% Data controlling for GM fraction
% correlation_results = robust_correlation(data.GABA_GMreg,data.Retention_GMreg)
% correlation_results = robust_correlation(data.GABA_GMreg,data.M1_Cerebellar_CC_GMreg)


% ----------------------------------------------------------------------
% Extract results from correlation_results structure for legibility

fnames=fieldnames(correlation_results)
for f = 1:length(fnames);
    fname=fnames{f};
    e = correlation_results.(fname);
    fnames_e=fieldnames(e)
    for f_e = 1:length(fnames_e);
        fname_e=fnames_e{f_e};
        r = e.(fname_e);
        new_fieldname=sprintf('%s_%s',fname, fname_e);
       
        if isstruct(r) & ~(fname(1:7)=='outlier');
           fnames_e_skip = fieldnames(r);
           for f_e_skip = 1:length(fnames_e_skip);
               fname_e_skip = fnames_e_skip{f_e_skip};
               r = r.(fname_e_skip);
               new_fieldname=sprintf('%s_%s_%s',fname, fname_e, fname_e_skip);
               results.(new_fieldname) = r;
           end
        else
            results.(new_fieldname) = r;
        end
        
    end
end

results
