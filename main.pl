init_thresholds :-
    (   exists_file('c:/users/veera/documents/prolog/thresholds.pl') ->
        consult('c:/users/veera/documents/prolog/thresholds.pl'),
        (   listing(diagnostic_threshold/4) ->
            write('DEBUG: Diagnostic thresholds loaded successfully!~n')
        ;   write('ERROR: No valid diagnostic thresholds found in thresholds.pl!~n'), fail
        )
    ;   write('ERROR: Thresholds file not found!~n'), fail
    ).



close_all_streams :-
    forall(current_input(S), catch(close(S), _, true)),  
    forall(current_output(S), catch(close(S), _, true)).
% Load the patient file

:- consult('chronic_disease.pl').
:- consult('patients.pl').
:- use_module(medical_diagnosis,[handle_diabetes_risk/0, calculate_bmi/3, diabetes_risk_assessment/1, fetch_measurements/4]).
:- consult('fetch_thresholds.pl').
:- consult('collect_symptoms.pl').

:- retractall(diagnostic_threshold(_, _, _, _)).  
:- use_module(guideline_import).

:- dynamic threshold/4.
:- dynamic selected_threshold/1.

% Maps each threshold source to the corresponding file
threshold_source(hospital_admin, 'thresholds/thresholds_hospital_admin.pl').
threshold_source(diabetes_response, 'thresholds/thresholds_diabetes_response.pl').
threshold_source(wellness, 'thresholds/thresholds_wellness.pl').
threshold_source(patient, 'thresholds/thresholds_patient.pl').



setup_system :-
    write('Initializing system...'), nl,
    (   selected_threshold(_) ->
        format('DEBUG: Selected threshold found: ~w~n', [selected_threshold])
    ;   write('No threshold selected. Prompting user...'), nl,
        select_threshold_source  % Force user to pick a threshold
    ),
    !.


select_threshold_source :-
    write('Select threshold source (hospital_admin / diabetes_response / wellness / patient): '), nl,
    read(UserChoice),
    (   threshold_source(UserChoice, _) ->
        retractall(selected_threshold(_)),  % Remove any previous selection
        assertz(selected_threshold(UserChoice)),  
        format('Selected threshold source: ~w~n', [UserChoice]),
        validate_and_copy_if_valid  % Validate before loading
    ;   write('Invalid choice! Please enter a valid option.'), nl,
        select_threshold_source  % Prompt again until valid input is received
    ).


% Proceed only if validation is successful
validate_and_copy_if_valid :-
    selected_threshold(User),
    threshold_source(User, FilePath),
    format('Validating and copying thresholds from: ~w~n', [FilePath]),

    (   exists_file(FilePath) ->
        format('DEBUG: File exists. Copying contents to "thresholds.pl"...~n'),
        copy_threshold_file(FilePath, 'c:/users/veera/documents/prolog/thresholds.pl'),
        format('DEBUG: Thresholds copied successfully!~n')

        % Skip extraction and saving after copying
    ;   format('ERROR: Threshold file "~w" not found! Please select another threshold.~n', [FilePath]),
        select_threshold_source  % Force user to pick again
    ).



% Proceed with threshold selection after validation
proceed_with_threshold_selection :-
    write('Select threshold source (hospital_admin / diabetes_response / wellness / patient): '), nl,
    read(User),
    (   threshold_source(User, File) ->
        retractall(selected_threshold(_)),
        assertz(selected_threshold(User)),
        load_thresholds(File)
    ;   write('Invalid source! Please restart and select a valid one.'), nl, fail
    ).

debug_message(Message) :-
    format('DEBUG: ~w~n', [Message]).

:- initialization(debug_message('Initializing system...'), now).
:- initialization(setup_system, now).
:- initialization(debug_message('Initializing thresholds...'), now).
:- initialization(init_thresholds, now).
:- initialization(debug_message('Loading patients...'), now).
:- initialization(load_patients('patients_db.txt'), now).
:- initialization(debug_message('Starting main menu...'), now).
:- initialization(go).


init_system :-
    format('DEBUG: Initializing system...~n'),
    selected_threshold(Source),
    threshold_source(Source, File),
    load_thresholds(File),  % Ensure thresholds are loaded and saved
    consult('thresholds.pl'),  % Load thresholds statically
    format('DEBUG: Thresholds successfully initialized and loaded from thresholds.pl!~n').

:- dynamic patient/7.
:- dynamic yes/1, no/1, measurement/3. % Format: measurement(Patient, MeasurementType, Value).
:- dynamic threshold/3.  % Format: threshold(Condition, Measurement, Value).
:- dynamic icd_threshold/4.


go :-
    write('Welcome to the Chronic Disease Management System!'), nl,
    write('Choose an option:'), nl,
    write('1. Manage patients'), nl,
    write('2. List chronic disease symptoms'), nl,
    write('3. Collect symptoms for a specific patient and update'), nl,
    write('4. Evaluate diabetes risk'), nl,
    write('5. Evaluate a disease (when patient has symptoms collected)'), nl,
    write('6. Validate diagnostic thresholds'), nl,
    write('7. Upload a guideline file for criteria extraction'), nl,
    write('8. Exit'), nl,
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :- handle_patient_operations, go.
handle_choice(2) :- list_chronic_disease_symptoms, go.
handle_choice(3) :- handle_collect_symptoms, go.
handle_choice(4) :- handle_diabetes_risk, go. 
handle_choice(5) :- handle_disease_evaluation, go.
handle_choice(6) :-  
    selected_threshold(User),  
    format('Validating thresholds for ~w...~n', [User]),
    validate_loaded_thresholds, 
    go.

handle_choice(7) :- handle_pdf_extraction, go.
handle_choice(8) :- handle_exit.
handle_choice(_) :- write('Invalid choice, try again!'), nl, go.


handle_patient_operations :-
    write('Patient Management Menu:'), nl,
    write('1. List all patients'), nl,
    write('2. Add a new patient'), nl,
    write('3. Update a patient record'), nl,
    write('4. Remove a patient'), nl,
    write('5. Go back to the main menu'), nl,
    read(Choice),
    (   Choice == 1 -> list_all_patients;
        Choice == 2 -> add_new_patient;
        Choice == 3 -> update_patient_record;
        Choice == 4 -> remove_patient_action;
        Choice == 5 -> true;
        write('Invalid choice. Try again.'), nl, handle_patient_operations
    ).


handle_collect_symptoms :-
    write('Enter patient name: '), read(UserInput),
    atom_string(Name, UserInput),
    (   patient(Name, Age, Gender, _, Anthropometry, FG, HbA1c) ->  
        format('Patient found: ~w (Age: ~d, Gender: ~w)~n', [Name, Age, Gender]),
        format('Existing diabetes data: FG = ~2f mmol/L, HbA1c = ~2f%%.~n', [FG, HbA1c]),
        format('Anthropometry Data: ~w~n', [Anthropometry]),

        % Collect numeric symptoms properly by iterating over known symptoms
        forall(numeric_symptom(Symptom, Description),
               collect_numeric_input(Name, Symptom, Description)),  

        % Collect boolean symptoms
        forall(boolean_symptom(Symptom, Description),
               collect_boolean_input(Name, Symptom, Description)),

        % Retrieve collected symptoms
        findall(Symptom, symptom(Name, Symptom), NewSymptoms),

        % Update medical history
        update_patient_symptoms(Name, NewSymptoms),

        % Save updated patient record
        save_patients_to_file,
        write('Symptoms updated successfully!\n')
    ;   
        write('Error: Patient not found in the database!\n')
    ).




collect_and_update_symptoms(Name, _, _, MedicalHistory, _) :-
    % Collect symptoms dynamically
    collect_symptoms(Name),

    % Update patient record directly with combined and deduplicated symptoms
    retract(patient(Name, _, _, MedicalHistory, _)),
    findall(Symptom, symptom(Name, Symptom), Symptoms),
    append(MedicalHistory, Symptoms, CombinedSymptoms),
    list_to_set(CombinedSymptoms, DeduplicatedSymptoms),  % Remove duplicates
    assertz(patient(Name, _, _, DeduplicatedSymptoms, _)),

    % Save updates to file
    save_patients_to_file,
    write('Symptoms collected and database updated successfully.'), nl.


ensure_files_exist([]).
ensure_files_exist([File|Rest]) :-
    (   exists_file(File)
    ->  true
    ;   format('Error: File ~w not found. Initialization aborted.~n', [File]),
        fail
    ),
    ensure_files_exist(Rest).


sync_symptoms_to_measurements(Patient) :-
    (   symptom(Patient, fasting_glucose(FG)) ->
        retractall(measurement(Patient, fasting_glucose, _)),
        assertz(measurement(Patient, fasting_glucose, FG))
    ;   true
    ),
    (   symptom(Patient, hba1c(HbA1c)) ->
        retractall(measurement(Patient, hba1c, _)),
        assertz(measurement(Patient, hba1c, HbA1c))
    ;   true
    ).



% Check and display BMI classification
check_bmi_and_display(Patient) :-
    patient(Patient, _, _, _, anthropometry(Weight, Height, _, _)),
    (   Weight > 0, Height > 0 ->
        calculate_bmi(Weight, Height, BMI),
        categorize_bmi(BMI, Category),
        format('~w has a BMI of ~2f, classified as ~w.~n', [Patient, BMI, Category])
    ;   write('BMI evaluation skipped due to missing weight or height.'), nl
    ).


handle_disease_evaluation :-
    write('Enter patient name: '), read(Name),
    (   patient(Name, _, _, MedicalHistory, _, _, _) ->  
        findall(Symptom, symptom(Name, Symptom), CollectedSymptoms),
        append(MedicalHistory, CollectedSymptoms, AllSymptoms),
        list_to_set(AllSymptoms, UniqueSymptoms),  % Remove duplicates
        medical_diagnosis:diagnose_disease(Name, UniqueSymptoms)  % Explicitly reference the module
    ;   write('Error: Patient not found in the database!'), nl
    ).



% Add a patient, including anthropometric data
add_patient_procedure :-
    write('Enter patient name (enclose in quotes if it contains spaces): '), read(Name),
    write('Enter patient age: '), read(Age),
    write('Enter patient gender (male/female): '), read(Gender),
    write('Enter initial medical history (as a list, e.g., [diabetes]): '), read(MedicalHistory),
    % Collect anthropometric data
    collect_anthropometry(Name, Weight, Height, BMI, Timestamp),
    % Add patient to the database
    (   add_patient(Name, Age, Gender, MedicalHistory, Weight, Height, BMI, Timestamp)
    ->  write('Patient added successfully!'), nl,
        save_patients_to_file  % Save the updated database to the file
    ;   write('Failed to add patient. Please try again.'), nl
    ).

remove_patient_procedure :-
    write('Enter patient name (enclose in quotes if it contains spaces): '), read(Name),
    (   remove_patient(Name)
    ->  write('Patient removed successfully!'), nl,
        save_patients_to_file  % Save the updated database to the file
    ;   write('Failed to remove patient. Ensure the patient exists.'), nl
    ).

handle_anthropometry_check :-
    write('Enter patient name: '), read(Name),
    % Check if the patient exists in the database
    (   patient(Name, Age, Gender, MedicalHistory, anthropometry(Weight, Height, _, _)) ->
        (   (Weight =:= 0 ; Height =:= 0)  % Check for missing data
        ->  write('Missing anthropometric data detected for '), write(Name), nl,
            prompt_missing_data(Name, Age, Gender, MedicalHistory)
        ;   write('Anthropometric data for '), write(Name), write(' is complete.'), nl
        )
    ;   % If the patient does not exist, notify the user
        write('Error: Patient not found in the database!'), nl
    ).



handle_pdf_extraction :-
    write('Would you like to extract thresholds from a new guideline PDF? (yes/no): '), nl,
    read(UserResponse),
    (   UserResponse == yes ->
        write('Enter the path to the guideline PDF: '), read(PDFPath),
        prompt_for_pdf_extraction(PDFPath)  % Extract from user-provided PDF
    ;   UserResponse == no ->
        write('Using existing thresholds if available...'), nl,
        (   exists_file('./thresholds.pl') ->
            write('Threshold file found. Loading thresholds...'), nl,
            retractall(icd_threshold(_, _, _, _)),  % Clear existing thresholds
            load_icd_thresholds('./thresholds.pl'),  % Load thresholds
            (   validate_loaded_thresholds ->  % Explicit module reference
                write('Thresholds successfully loaded and validated.'), nl,
                list_icd_thresholds  % Correct reference
            ;   write('Warning: Threshold validation failed! Consider uploading a new guideline PDF.'), nl
            )
        ;   write('Error: No valid thresholds found. Please upload a new guideline PDF.'), nl,
            write('Enter the path to the guideline PDF: '), read(PDFPath),
            prompt_for_pdf_extraction(PDFPath)  % Upload a new PDF if no valid file exists
        )
    ).


prompt_for_pdf_extraction(PDFPath) :-
    write('Extracting thresholds from the provided PDF...'), nl,
    PythonPath = 'python',  % Adjust if using a virtual environment
    ScriptPath = './pdfextracter.py',
    OutputPath = './thresholds.pl',
    format(atom(Command), '"~w" "~w" "~w" "~w"', [PythonPath, ScriptPath, PDFPath, OutputPath]),
    write('Executing command: '), write(Command), nl,
    shell(Command, ExitCode),
    (   ExitCode =:= 0 ->
        write('Thresholds extracted successfully to file: '), write(OutputPath), nl,
        retractall(icd_threshold(_, _, _, _)),  % Clear existing thresholds
        load_icd_thresholds(OutputPath),  % Reload thresholds from extracted file
        (   validate_loaded_thresholds ->
            write('Thresholds successfully loaded and validated after extraction.'), nl,
            list_icd_thresholds  % Display the extracted thresholds
        ;   write('Error: Unable to validate thresholds after extraction. Please check the guideline file.'), nl
        )
    ;   write('Error: Extraction failed. Ensure the PDF path is correct and dependencies are installed.'), nl
    ).


handle_exit :-
    write('Goodbye!'), nl, halt.





