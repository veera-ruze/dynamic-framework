:- dynamic patient/7.


% patient(Name, Age, Gender, MedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c).
% anthropometry format: anthropometry(weight, height, bmi, last_checked).

add_patient(Name, Age, Gender, MedicalHistory, Weight, Height, BMI, Timestamp, FG, HbA1c) :-
    (   var(MedicalHistory) 
    ->  ValidMedicalHistory = []  % If undefined, default to empty list
    ;   (   is_list(MedicalHistory)  
        ->  ValidMedicalHistory = MedicalHistory  % Keep as list
        ;   ValidMedicalHistory = [MedicalHistory]  % Convert single atom to list
        )
    ),
    assertz(patient(Name, Age, Gender, ValidMedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c)),
    save_patients_to_file.



add_new_patient :-  
    write('Enter patient name: '), read(Name),
    write('Enter patient age: '), read(Age),
    write('Enter patient gender (male/female): '), read(Gender),
    write('Enter medical history as a list (e.g., [diabetes, hypertension]): '), read(MedicalHistory),
    collect_anthropometry(Name, Weight, Height, BMI, Timestamp),
    write('Enter fasting glucose (mmol/L): '), read(FG),  % Collect diabetes data
    write('Enter HbA1c (%): '), read(HbA1c),
    assertz(patient(Name, Age, Gender, MedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c)),
    save_patients_to_file,
    write('Patient added successfully!'), nl.



update_medical_history(Name, Age, Gender, Anthropometry, FG, HbA1c) :-
    write('Enter updated medical history as a list (e.g., [diabetes, hypertension]): '), read(NewHistory),
    (   is_list(NewHistory) -> ValidNewHistory = NewHistory
    ;   ValidNewHistory = [NewHistory]  % Convert atom to list if necessary
    ),
    retract(patient(Name, Age, Gender, _, Anthropometry, FG, HbA1c)),  
    assertz(patient(Name, Age, Gender, ValidNewHistory, Anthropometry, FG, HbA1c)),
    save_patients_to_file,
    write('Medical history updated successfully.'), nl.


update_anthropometry(Name, Age, Gender, MedicalHistory, FG, HbA1c) :-
    collect_anthropometry(Name, Weight, Height, BMI, Timestamp),
    retract(patient(Name, Age, Gender, MedicalHistory, _, FG, HbA1c)),  
    assertz(patient(Name, Age, Gender, MedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c)),  
    save_patients_to_file,
    format('Anthropometric data updated successfully for ~w. Timestamp: ~w~n', [Name, Timestamp]).



% Centralized anthropometry collection
collect_anthropometry(Name, Weight, Height, BMI, Timestamp) :-
    write('Enter weight (in kg, or 0 if not available): '), read(Weight),
    write('Enter height (in cm, or 0 if not available): '), read(Height),
    get_time(Timestamp),  % Record the current time
    (   Weight > 0, Height > 0 ->
        calculate_bmi(Weight, Height, BMI),
        format('BMI calculated: ~2f~n', [BMI])
    ;   Weight = 0, Height = 0, BMI = 0,  % Default values if input is invalid
        write('Invalid weight or height entered. Setting defaults to 0.'), nl
    ),
    format('Anthropometric data recorded for ~w. Timestamp: ~w~n', [Name, Timestamp]).


check_anthropometry(Name, anthropometry(Weight, Height, BMI, Timestamp)) :-
    (   Weight =:= 0 ; Height =:= 0 ->
        write('Missing or invalid anthropometric data detected for '), write(Name), nl,
        prompt_missing_data(Name)
    ;   write('Anthropometric data for '), write(Name), write(' is complete: '),
        format('Weight = ~2f kg, Height = ~2f cm, BMI = ~2f. Last updated: ~w~n', [Weight, Height, BMI, Timestamp]),
        write('Do you want to update this data? (yes/no): '), read(Response),
        (   Response == yes -> update_anthropometry(Name)
        ;   write('No changes made to the anthropometric data.'), nl
        )
    ).


prompt_and_update_diabetes_data(Patient) :-
    (   diabetes_data(Patient, FG, HbA1c) ->  
        format('Existing diabetes data: FG = ~2f mmol/L, HbA1c = ~2f%%~n', [FG, HbA1c]),  
        write('Diabetes data is static and cannot be changed.\n')
    ;   collect_new_diabetes_data(Patient)  % Only collects if missing
    ).



collect_new_diabetes_data(Patient) :-  
    (   diabetes_data(Patient, _, _) ->  
        write('Error: Diabetes data is static and cannot be changed.\n'), fail  
    ;   write('Enter fasting glucose (mmol/L): '), read(FG),  
        write('Enter HbA1c (%): '), read(HbA1c),  
        assertz(diabetes_data(Patient, FG, HbA1c)),  % Store data only once  
        save_patients_to_file,  
        format('Diabetes data recorded for ~w: FG = ~2f mmol/L, HbA1c = ~2f%%.\n', [Patient, FG, HbA1c])
    ).



update_patient_with_diabetes_data(Patient) :-
    diabetes_data(Patient, FG, HbA1c),  % Retrieve diabetes data
    (   retract(patient(Patient, Age, Gender, MedicalHistory, Anthropometry)) ->  
        assertz(patient(Patient, Age, Gender, [diabetes_data(FG, HbA1c) | MedicalHistory], Anthropometry)),  
        save_patients_to_file,
        format('Patient record updated with diabetes data: FG = ~2f mmol/L, HbA1c = ~2f%%.\n', [FG, HbA1c])
    ;   write('Error: Failed to update diabetes data in patient record.\n')
    ).




prompt_missing_data(Name, Age, Gender, MedicalHistory) :-
    % Retrieve current data
    patient(Name, Age, Gender, MedicalHistory, anthropometry(OldWeight, OldHeight, _, _)),
    % Prompt for new data
    write('Enter weight (in kg, or 0 to keep current): '), read(NewWeight),
    write('Enter height (in cm, or 0 to keep current): '), read(NewHeight),
    get_time(Timestamp),  % Record the current time
    % Validate and update data
    (   (NewWeight > 0 -> FinalWeight = NewWeight; FinalWeight = OldWeight),
        (NewHeight > 0 -> FinalHeight = NewHeight; FinalHeight = OldHeight),
        calculate_bmi(FinalWeight, FinalHeight, BMI),
        % Update patient record
        retract(patient(Name, Age, Gender, MedicalHistory, _)),
        assertz(patient(Name, Age, Gender, MedicalHistory, anthropometry(FinalWeight, FinalHeight, BMI, Timestamp))),
        save_patients_to_file,
        format('Anthropometric data updated successfully for ~w. Timestamp: ~w~n', [Name, Timestamp])
    ;   write('Invalid input. No changes were made.'), nl
    ).


% Retrieve a patient record
get_patient(Name, Age, Gender, _, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c) :-
    patient(Name, Age, Gender, _, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c).


% Remove a patient record
remove_patient(Name) :-
    retractall(patient(Name, _, _, _, _, _, _)).  


list_all_patients :-
    findall((Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c),
            patient(Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c),
            Patients),
    (   Patients \= [] ->
        write('Listing all patients:'), nl,
        forall(member((Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c), Patients),
               (   (is_list(MedicalHistory) -> MH = MedicalHistory ; MH = [MedicalHistory]),  % Convert single atoms to lists
                   format('Name: ~w, Age: ~d, Gender: ~w, Medical History: ~w, Anthropometry: ~w, FG: ~2f, HbA1c: ~2f~n',
                          [Name, Age, Gender, MH, Anthropometry, FG, HbA1c])
               )
        )
    ;   write('No patients found in the database.'), nl
    ).



remove_patient_action :-
    write('Enter the name of the patient to remove: '), read(Name),
    (   retractall(patient(Name, _, _, _, _)) ->
        save_patients_to_file,
        write('Patient removed successfully.'), nl
    ;   write('Patient not found.'), nl
    ).


write_patients_to_file(File) :-
    open(File, write, Stream),
    findall(patient(Name, Age, Gender, MedicalHistory, Anthropometry), 
            patient(Name, Age, Gender, MedicalHistory, Anthropometry), Patients),
    write_patients(Patients, Stream),
    close(Stream).

write_patients([], _).
write_patients([patient(Name, Age, Gender, MedicalHistory, anthropometry(Weight, Height, BMI, Timestamp), FG, HbA1c) | Tail], Stream) :-
    format(Stream, 'patient(~q, ~d, ~w, ~w, anthropometry(~w, ~w, ~w, ~w), ~2f, ~2f).~n',
           [Name, Age, Gender, MedicalHistory, Weight, Height, BMI, Timestamp, FG, HbA1c]),
    write_patients(Tail, Stream).


load_patients(File) :-
    (   exists_file(File) ->
        write('DEBUG: Loading patients from file...'), nl,
        open(File, read, Stream),
        repeat,
        read(Stream, Term),
        (   Term == end_of_file -> close(Stream)
        ;   assertz(Term), fail
        ),
        listing(patient)  % Show loaded patients
    ;   write('ERROR: Patient database file not found!~n'), fail
    ).



update_patient_record :-  
    write('Enter the name of the patient to update: '), read(CurrentName),
    (   patient(CurrentName, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c) ->  
        write('What would you like to update?'), nl,
        write('1. Medical history'), nl,
        write('2. Anthropometry'), nl,
        write('3. Diabetes Data (Fasting Glucose & HbA1c)'), nl,  
        read(UpdateChoice),
        (   UpdateChoice == 1 -> update_medical_history(CurrentName, Age, Gender, Anthropometry, FG, HbA1c);
            UpdateChoice == 2 -> update_anthropometry(CurrentName, Age, Gender, MedicalHistory, FG, HbA1c);
            UpdateChoice == 3 -> update_diabetes_data(CurrentName, Age, Gender, MedicalHistory, Anthropometry);  
            write('Invalid choice. Returning to patient management menu.'), nl
        )
    ;   write('Error: Patient not found in the database.'), nl
    ).


update_diabetes_data(Name, Age, Gender, MedicalHistory, Anthropometry) :-  
    write('Enter updated fasting glucose (mmol/L): '), read(FG),
    write('Enter updated HbA1c (%): '), read(HbA1c),
    
    % Ensure values are instantiated before updating
    number(FG), number(HbA1c),

    retract(patient(Name, Age, Gender, MedicalHistory, Anthropometry, _, _)),
    assertz(patient(Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c)),  
    save_patients_to_file,
    write('Diabetes data updated successfully!'), nl.



update_personal_details_interactive(CurrentName, CurrentAge, Gender, MedicalHistory, Anthropometry) :-
    write('What personal detail would you like to update?'), nl,
    write('1. Name'), nl,
    write('2. Age'), nl,
    read(DetailChoice),
    (   DetailChoice == 1 ->
            write('Enter the updated name: '), read(NewName),
            retract(patient(CurrentName, CurrentAge, Gender, MedicalHistory, Anthropometry)),
            assertz(patient(NewName, CurrentAge, Gender, MedicalHistory, Anthropometry)),
            save_patients_to_file,
            write('Name updated successfully!'), nl;
        DetailChoice == 2 ->
            write('Enter the updated age: '), read(NewAge),
            retract(patient(CurrentName, CurrentAge, Gender, MedicalHistory, Anthropometry)),
            assertz(patient(CurrentName, NewAge, Gender, MedicalHistory, Anthropometry)),
            save_patients_to_file,
            write('Age updated successfully!'), nl;
        write('Invalid choice. Returning to patient update menu.'), nl
    ).



extract_diabetes_data(MedicalHistory, FG, HbA1c) :-
    (   member(diabetes_data(FG, HbA1c), MedicalHistory),
        number(FG), FG > 0, number(HbA1c), HbA1c > 0
    ->  true  % Successfully extracted
    ;   FG = -1, HbA1c = -1,  % Default invalid values to indicate missing data
        write('Warning: No valid diabetes data found in MedicalHistory!'), nl
    ).


% Validate and fix the MedicalHistory field
validate_medical_history(MedicalHistory, ValidMedicalHistory) :-
    maplist(validate_diabetes_data, MedicalHistory, ValidMedicalHistory).

% Validate diabetes_data and replace end_of_file with default values
validate_diabetes_data(diabetes_data(end_of_file, end_of_file), diabetes_data(0, 0)).
validate_diabetes_data(Term, Term).  % Keep all other terms unchanged




update_patient_symptoms(Name, NewSymptoms) :-
    patient(Name, Age, Gender, OldMedicalHistory, Anthropometry, FG, HbA1c),
    (   is_list(OldMedicalHistory) -> ValidOldHistory = OldMedicalHistory
    ;   ValidOldHistory = [OldMedicalHistory]  % Convert single atom to list if needed
    ),
    append(ValidOldHistory, NewSymptoms, UpdatedMedicalHistory),
    list_to_set(UpdatedMedicalHistory, DeduplicatedMedicalHistory),  % Remove duplicates
    retract(patient(Name, Age, Gender, OldMedicalHistory, Anthropometry, FG, HbA1c)),
    assertz(patient(Name, Age, Gender, DeduplicatedMedicalHistory, Anthropometry, FG, HbA1c)),
    save_patients_to_file.



save_patients_to_file :-  
    write('Saving updated patients to the database file...'), nl,  
    open('patients_db.txt', write, Stream),  

    findall(patient(Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c),  
            patient(Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c),  
            Patients),  
    forall(member(patient(Name, Age, Gender, MedicalHistory, Anthropometry, FG, HbA1c), Patients),  
           (   (is_list(MedicalHistory) -> MH = MedicalHistory ; MH = [MedicalHistory]),  
               format(Stream, 'patient(~q, ~d, ~w, ~q, ~q, ~2f, ~2f).~n',  
                      [Name, Age, Gender, MH, Anthropometry, FG, HbA1c])
           )
    ),

    close(Stream),  
    write('All patients and diabetes data saved successfully!'), nl.  