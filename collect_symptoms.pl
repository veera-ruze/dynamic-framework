% Dynamic predicate for symptoms
:- dynamic symptom/2, measurement/3.
:- dynamic diagnostic_threshold/4.


% Symptom descriptions for numeric and yes/no inputs

numeric_symptom(fasting_glucose, 'Fasting Glucose (mmol/L)').
numeric_symptom(hba1c, 'HbA1c (Glycated Hemoglobin) in percentage').


boolean_symptom(headache, 'Do you have a headache?').
boolean_symptom(runny_nose, 'Do you have a runny nose?').
boolean_symptom(cough, 'Do you have a cough?').
boolean_symptom(body_ache, 'Do you have body aches?').
boolean_symptom(chills, 'Do you have chills?').
boolean_symptom(sore_throat, 'Do you have a sore throat?').
boolean_symptom(sneezing, 'Do you have sneezing?').
boolean_symptom(swollen_glands, 'Do you have swollen glands?').


collect_symptoms(Patient) :-
    write('Collecting symptoms for: '), write(Patient), nl,

    % Iterate over numeric symptoms
    forall(numeric_symptom(Symptom, Description),
           collect_numeric_input(Patient, Symptom, Description)),

    % Iterate over boolean symptoms
    forall(boolean_symptom(Symptom, Description),
           collect_boolean_input(Patient, Symptom, Description)),

    write('Symptom collection completed for: '), write(Patient), nl.


% Ensure the patient exists before collecting numeric input
collect_numeric_input(Patient, Symptom, Description) :-
    (   patient(Patient, _, _, _, _, _, _)  % Check if patient exists
    ->  format('~w, please enter your ~w (numeric value required): ', [Patient, Description]),
        catch(read(Value), _, fail),  % Catch errors
        (   number(Value), Value >= 0
        ->  assertz(measurement(Patient, Symptom, Value)),
            format('Recorded: ~w = ~2f~n', [Symptom, Value])
        ;   write('Invalid input. Please enter a valid number.'), nl,
            collect_numeric_input(Patient, Symptom, Description)
        )
    ;   format('Error: Patient ~w not found! Please enter a valid patient name.~n', [Patient]), fail
    ).



% Ensure the patient exists before collecting boolean symptoms
collect_boolean_input(Patient, Symptom, Description) :-
    (   patient(Patient, _, _, _, _, _, _)  % Check if patient exists
    ->  format('~w, ~w (yes/no): ', [Patient, Description]),
        read(Response),
        (   Response == yes -> assertz(symptom(Patient, Symptom));
            Response == no -> true;  % Skip for "no"
            write('Invalid input. Please enter yes or no.'), nl,
            collect_boolean_input(Patient, Symptom, Description)
        )
    ;   format('Error: Patient ~w not found! Please enter a valid patient name.~n', [Patient]), fail
    ).


% Clear previous data (optional utility)
undo_symptom_collection :-
    retractall(symptom(_, _)),
    retractall(measurement(_, _, _)),
    write('All previous symptom data cleared.'), nl.


% Example static symptom definitions
disease_symptom(diabetes, [excessive_thirst, frequent_urination, fatigue, blurred_vision, elevated_glucose]).
disease_symptom(hypertension, [headache, dizziness, shortness_of_breath, chest_pain]).
disease_symptom(arthritis, [joint_pain, stiffness, swelling, reduced_mobility]).
disease_symptom(asthma, [shortness_of_breath, wheezing, chest_tightness, coughing]).
disease_symptom(copd, [shortness_of_breath, chronic_cough, wheezing, fatigue]).
disease_symptom(heart_disease, [chest_pain, shortness_of_breath, fatigue, dizziness]).
disease_symptom(kidney_disease, [swelling, fatigue, changes_in_urine, nausea]).
disease_symptom(liver_disease, [jaundice, abdominal_pain, fatigue, swelling]).

list_chronic_disease_symptoms :-
    write('Listing symptoms for chronic diseases:'), nl,
    forall(disease_symptom(Disease, Symptoms),
           (   format('~w: ~w~n', [Disease, Symptoms]))
          ).

collect_chronic_disease_symptoms(Patient, Disease) :-
    disease_symptom(Disease, Symptoms),
    write('Collecting symptoms for '), write(Disease), nl,
    forall(member(Symptom, Symptoms),
           (   format('Do you have ~w? (yes/no): ', [Symptom]),
               read(Response),
               (Response == yes -> assertz(has_symptom(Patient, Symptom)); true)
           )),
    write('Symptom collection for '), write(Disease), write(' complete.'), nl.

% Updated collect_all_chronic_symptoms
collect_all_chronic_symptoms(Patient) :-
    write('Collecting symptoms for all chronic diseases...'), nl,
    forall(
        (symptom_description(Symptom, Description)),
        (   ask_symptom(Patient, Symptom, Description)
        )
    ),
    write('All symptoms recorded for patient.'), nl.

    % Ask for a specific symptom
ask_symptom(Patient, Symptom, Description) :-
    format('~w, do you ~w (yes/no)? ', [Patient, Description]),
    read(Response),
    (   Response == yes -> assertz(symptom(Patient, Symptom));
        Response == no -> true;  % No assertion for "no"
        write('Invalid input. Please enter yes or no.'), nl, ask_symptom(Patient, Symptom, Description)
    ).

collect_symptoms_from_user(Patient, [Symptom | RemainingSymptoms], CollectedSymptoms) :-
    format('Do you have ~w? (yes/no): ', [Symptom]),
    attempt_read_boolean(Response, 3),
    (   Response == yes
    ->  assertz(symptom(Patient, Symptom)), 
        NewCollectedSymptoms = [Symptom | CollectedSymptoms]
    ;   NewCollectedSymptoms = CollectedSymptoms
    ),
    collect_symptoms_from_user(Patient, RemainingSymptoms, NewCollectedSymptoms).

attempt_read_boolean(Value, Attempts) :-
    (   Attempts > 0
    ->  read(Response),
        (   Response == yes ; Response == no
        ->  Value = Response
        ;   NewAttempts is Attempts - 1,
            write('Invalid response. Please enter "yes" or "no".'), nl,
            attempt_read_boolean(Value, NewAttempts)
        )
    ;   Value = no
    ).