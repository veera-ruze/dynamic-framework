

:- module(medical_diagnosis, [
    handle_diabetes_risk/0,
    diabetes_risk_assessment/1,
    evaluate_threshold/3,
    diagnose_disease/2,
    normalize_measurement_type/2,
    fetch_measurements/4,
    calculate_bmi/3  % Add this line to export the predicate!
]).

:- dynamic yes/1, no/1.
:- dynamic patient/7.
:- dynamic symptom/2.
:- dynamic icd_threshold/4.


normalize_measurement_type(fasting_glucose, fasting_glucose).  
normalize_measurement_type('fasting glucose', fasting_glucose).  
normalize_measurement_type(hba1c, hba1c).
normalize_measurement_type('HbA1c', hba1c).
normalize_measurement_type('hba1c', hba1c).
normalize_measurement_type(Other, Other).   

evaluate_threshold(Value, MeasurementType, Category) :-
    format('DEBUG: Checking thresholds for ~w (value: ~2f)~n', [MeasurementType, Value]),
    
    (   diagnostic_threshold(Category, MeasurementType, Min, Max),
        format('DEBUG: Comparing ~2f with range (~2f - ~w)~n', [Value, Min, Max]),
        
        % Handle infinite upper threshold
        (Max == inf -> Value >= Min ; Value >= Min, Value =< Max)
    ->  format('DEBUG: Matched category: ~w~n', [Category])
    ;   write('ERROR: No matching threshold found!~n'), nl, fail
    ).




check_bmi_data(Patient, anthropometry(Weight, Height, BMI, _), FinalBMI) :-  
    (   number(BMI), BMI > 0 -> FinalBMI = BMI
    ;   write('No BMI data found. Enter weight (kg): '), read(Weight),  
        write('Enter height (cm): '), read(Height),  
        calculate_bmi(Weight, Height, FinalBMI),  
        update_patient_bmi(Patient, Weight, Height, BMI)
    ).


check_bmi_and_display(Patient) :-
    measurement(Patient, bmi, BMI),
    categorize_bmi(BMI, Category),
    format('~w has a BMI of ~2f, classified as ~w.~n', [Patient, BMI, Category]).

categorize_bmi(BMI, underweight) :- BMI < 18.5.
categorize_bmi(BMI, normal_weight) :- BMI >= 18.5, BMI < 24.9.
categorize_bmi(BMI, overweight) :- BMI >= 25, BMI < 29.9.
categorize_bmi(BMI, obese) :- BMI >= 30.

handle_diabetes_risk :-  
    write('Enter patient name: '), read(UserInput),

    % Ensure input is an atom
    (   atom(UserInput) -> Patient = UserInput
    ;   string(UserInput) -> atom_string(Patient, UserInput)
    ;   write('ERROR: Invalid name format! Please enter an atom or string.'), nl, fail
    ),

    % Debug: Show available patients
    findall(N, patients:patient(N, _, _, _, _, _, _), PatientList),
    format('DEBUG: Patients in database: ~w~n', [PatientList]),
    
    % Query the correct scope
    (   patients:patient(Patient, Age, Gender, _, _, FG, HbA1c) ->  
        format('DEBUG: Patient found: ~q (Age: ~d, Gender: ~w)~n', [Patient, Age, Gender]),
        format('Using diabetes data: FG = ~2f mmol/L, HbA1c = ~2f%~n', [FG, HbA1c]),
        diabetes_risk_assessment(Patient)
    ;   
        write('ERROR: Patient not found in the database!~n'), nl, fail
    ).



diabetes_data_exists(FG, HbA1c) :- 
    number(FG), FG > 0, 
    number(HbA1c), HbA1c > 0.


update_patient_bmi(Patient, Weight, Height, BMI) :-  
    patient(Patient, Age, Gender, MedicalHistory, _, FG, HbA1c),  % Retrieve full patient/7 data
    get_time(Timestamp),
    retract(patient(Patient, Age, Gender, MedicalHistory, _, FG, HbA1c)),  
    assertz(patient(Patient, Age, Gender, MedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c)),  
    save_patients_to_file.




% Use structured approach to diagnose risk
diabetes_risk_assessment(Patient) :-
    format('DEBUG: Running diabetes_risk_assessment for ~w~n', [Patient]),

    % Retrieve patient’s FG and HbA1c
    (   patients:patient(Patient, _, _, _, _, FG, HbA1c) ->  
        format('DEBUG: Retrieved FG = ~2f, HbA1c = ~2f~n', [FG, HbA1c])
    ;   write('ERROR: Could not retrieve FG and HbA1c!~n'), nl, fail
    ),

    % Debug: Print what is being passed into evaluate_threshold/3
    format('DEBUG: Calling evaluate_threshold(~2f, fasting_glucose, Category)~n', [FG]),

    % Compare FG against thresholds
    (   evaluate_threshold(FG, fasting_glucose, FGCategory) -> 
        format('DEBUG: FG category: ~w~n', [FGCategory])
    ;   write('ERROR: FG threshold evaluation failed!~n'), nl, fail
    ),

    % Compare HbA1c against thresholds
    (   evaluate_threshold(HbA1c, hba1c, HbA1cCategory) -> 
        format('DEBUG: HbA1c category: ~w~n', [HbA1cCategory])
    ;   write('ERROR: HbA1c threshold evaluation failed!~n'), nl, fail
    ),

    % Determine final risk
    format('DEBUG: Final diabetes risk assessment complete for ~w~n', [Patient]).




fetch_measurements(Patient, FG, HbA1c, BMI) :-
    patient(Patient, _, _, _, anthropometry(_, _, BMI, _), FG, HbA1c).  % Use patient/7



calculate_bmi(Weight, Height, BMI) :-
    Height > 0,  % Prevent division by zero
    HeightMeters is Height / 100,  % Convert height to meters
    BMI is Weight / (HeightMeters * HeightMeters).

evaluate_diabetes_risk(Patient, FGCategory, HbA1cCategory, BMICategory) :-  
    (   FGCategory == diabetes ; HbA1cCategory == diabetes ->  
        format('~w is at HIGH diabetes risk.~n', [Patient])  
    ;   FGCategory == prediabetes ; HbA1cCategory == prediabetes ->  
        format('~w is at MODERATE diabetes risk (prediabetes).~n', [Patient])  
    ;   BMICategory == obese ->  
        format('~w is at an increased risk due to high BMI.~n', [Patient])  
    ;   FGCategory == unknown, HbA1cCategory == unknown, BMICategory == unknown ->  
        format('ERROR: Unable to classify diabetes risk for ~w due to missing or incorrect data!~n', [Patient]), fail  
    ;   format('~w is at LOW diabetes risk (normal levels).~n', [Patient])  
    ).



categorize_threshold(Value, fasting_glucose, normal) :- Value =< 6.0.
categorize_threshold(Value, fasting_glucose, prediabetes) :- Value > 6.0, Value < 7.0.
categorize_threshold(Value, fasting_glucose, diabetes) :- Value >= 7.0.

categorize_threshold(Value, hba1c, normal) :- Value =< 5.6.
categorize_threshold(Value, hba1c, prediabetes) :- Value > 5.6, Value =< 6.4.
categorize_threshold(Value, hba1c, diabetes) :- Value > 6.4.

categorize_threshold(Value, bmi, underweight) :- Value < 18.5.
categorize_threshold(Value, bmi, normal) :- Value >= 18.5, Value < 24.9.
categorize_threshold(Value, bmi, overweight) :- Value >= 25, Value < 29.9.
categorize_threshold(Value, bmi, obese) :- Value >= 30.

    
sync_diagnostic_thresholds :-
    unload_previous_thresholds,  % Ensure old definitions are cleared
    load_thresholds,  % Reload the correct thresholds
    selected_threshold(User),
    format('Using ~w thresholds.~n', [User]).


diagnose_disease(Patient, Symptoms) :-
    write('Evaluating disease based on symptoms...'), nl,
    findall(
        Disease-MatchingSymptoms,
        (   disease_symptom(Disease, DiseaseSymptoms),
            intersection(DiseaseSymptoms, Symptoms, MatchingSymptoms),
            MatchingSymptoms \= []
        ),
        MatchedDiseases
    ),
    (   MatchedDiseases \= [] ->
        format('~w may have the following condition(s):~n', [Patient]),
        forall(member(Disease-Match, MatchedDiseases),
               format('- ~w (Matching symptoms: ~w)~n', [Disease, Match]))
    ;   write('No known disease matches the provided symptoms.'), nl
    ).
