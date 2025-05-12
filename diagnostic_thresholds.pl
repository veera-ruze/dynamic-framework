
:- module(diagnostic_thresholds, [diagnostic_threshold/4]).
:- dynamic diagnostic_threshold/4.

% Diabetes diagnostic thresholds
diagnostic_threshold(normal_glucose, fasting_glucose, 0.0, 6.0).  % Normal range
diagnostic_threshold(prediabetes, fasting_glucose, 6.1, 6.9).      % Prediabetes range
diagnostic_threshold(diabetes, fasting_glucose, 7.0, inf).         % Diabetes range
diagnostic_threshold(normal_hba1c, hba1c, 0.0, 5.6).               % Normal HbA1c
diagnostic_threshold(prediabetes, hba1c, 5.7, 6.4).                % Prediabetes HbA1c
diagnostic_threshold(diabetes, hba1c, 6.5, inf).                   % Diabetes HbA1c
