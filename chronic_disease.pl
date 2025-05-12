% Chronic diseases
chronic_disease(diabetes).
chronic_disease(hypertension).
chronic_disease(arthritis).
chronic_disease(asthma).
chronic_disease(copd).  % Chronic Obstructive Pulmonary Disease
chronic_disease(heart_disease).
chronic_disease(kidney_disease).
chronic_disease(liver_disease).

list_chronic_diseases :-
    findall(Name, chronic_disease(Name), Diseases),
    write('Chronic diseases: '), nl,
    write_list(Diseases).

patients_with_chronic_diseases :-
    findall((Patient, Disease),
            (   patient(Patient, _, _, History),
                member(Disease, History),
                chronic_disease(Disease)
            ),
            Matches),
    write('Patients with chronic diseases: '), nl,
    write_list(Matches).