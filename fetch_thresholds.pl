
:- use_module(medical_diagnosis, [normalize_measurement_type/2]).

get_threshold_file(default, 'thresholds/thresholds_default.pl').
get_threshold_file(hospital_admin, 'thresholds/thresholds_hospital_administrator.pl').
get_threshold_file(diabetes_response, 'thresholds/thresholds_diabetes_response.pl').
get_threshold_file(wellness, 'thresholds/thresholds_wellness.pl').
get_threshold_file(patient, 'thresholds/thresholds_patient.pl').


load_thresholds(FilePath) :-
    format('DEBUG: Resolving file path: ~w~n', [FilePath]),
    (   exists_file(FilePath) ->
        format('DEBUG: File exists. Extracting thresholds...~n'),
        extract_thresholds(FilePath, Thresholds),
        (   Thresholds \= [] ->
            format('DEBUG: Writing thresholds to static file...~n'),
            save_thresholds_to_file('c:/users/veera/documents/prolog/thresholds.pl', Thresholds),
            format('DEBUG: Thresholds saved successfully!~n')
        ;   format('ERROR: No thresholds found in ~w!~n', [FilePath]), fail
        )
    ;   format('ERROR: Threshold file ~w not found!~n', [FilePath]), fail
    ).



copy_threshold_file(SourceFile, DestinationFile) :-
    setup_call_cleanup(
        open(SourceFile, read, InStream, [encoding(utf8)]),
        setup_call_cleanup(
            open(DestinationFile, write, OutStream, [encoding(utf8)]),
            (   repeat,
                read_line_to_string(InStream, Line),
                (   Line == end_of_file -> !
                ;   writeln(OutStream, Line), fail
                )
            ),
            close(OutStream)
        ),
        close(InStream)
    ),
    format('Thresholds copied from ~w to ~w.~n', [SourceFile, DestinationFile]).





write_thresholds(Stream, []) :- 
    format(Stream, '/* ERROR: No thresholds available */~n', []).
write_thresholds(Stream, [threshold(Category, MeasurementType, Min, Max) | Rest]) :-
    format(Stream, 'threshold(~q, ~q, ~2f, ~2f).~n', [Category, MeasurementType, Min, Max]),
    write_thresholds(Stream, Rest).  % Recursively write all thresholds.



extract_thresholds(FilePath, Thresholds) :-
    setup_call_cleanup(
        open(FilePath, read, Stream, [encoding(utf8)]),
        (   read_all_terms(Stream, Thresholds),
            (   Thresholds = [] ->
                format('ERROR: No thresholds extracted from ~w!~n', [FilePath]), fail
            ;   format('DEBUG: Extracted thresholds: ~w~n', [Thresholds])
            )
        ),
        close(Stream)
    ).


read_all_terms(Stream, []) :-
    at_end_of_stream(Stream), !.

read_all_terms(Stream, [Term | Terms]) :-
    read(Stream, Term),
    (   Term == end_of_file -> Terms = []
    ;   read_all_terms(Stream, Terms)
    ).


import_thresholds :-
    findall(threshold(C, M, Min, Max), threshold(C, M, Min, Max), Thresholds),
    forall(
        member(threshold(C, M, Min, Max), Thresholds),
        (
            normalize_measurement_type(M, NormalizedM),
            retractall(diagnostic_threshold(C, NormalizedM, _, _)),
            assertz(diagnostic_threshold(C, NormalizedM, Min, Max))
        )
    ),
    save_thresholds_to_file('c:/users/veera/documents/prolog/thresholds.pl', Thresholds).



convert_thresholds_to_numbers :-
    findall((C, T, MinStr, MaxStr), diagnostic_threshold(C, T, MinStr, MaxStr), Thresholds),
    format('DEBUG: Found ~w thresholds in memory.~n', [Thresholds]),
    forall(
        member((C, T, MinStr, MaxStr), Thresholds),
        (   
            format('DEBUG: Processing ~w (~w, ~w)~n', [T, MinStr, MaxStr]),
            (   MinStr == inf -> Min = 1.0e300  % Replace `inf` with a large number
            ;   atom(MinStr) -> atom_number(MinStr, Min) 
            ;   Min = MinStr
            ),
            (   MaxStr == inf -> Max = 1.0e300  % Replace `inf` with a large number
            ;   atom(MaxStr) -> atom_number(MaxStr, Max)
            ;   Max = MaxStr
            ),
            format('DEBUG: Converted ~w -> (~w, ~w)~n', [T, Min, Max]),
            retract(diagnostic_threshold(C, T, MinStr, MaxStr)),
            assertz(diagnostic_threshold(C, T, Min, Max))
        )
    ).



evaluate_threshold(Value, MeasurementType, Category) :-
    threshold(Category, MeasurementType, Min, Max),
    number(Min), number(Max),
    Value >= Min, Value =< Max.


list_loaded_thresholds :-
   findall((Category, Measurement, Min, Max), diagnostic_threshold(Category, Measurement, Min, Max), Thresholds),
   (   Thresholds \= [] 
   ->  forall(member((C, M, Min, Max), Thresholds),
              format('Threshold Loaded: Category: ~w, Measurement: "~w", Min: ~2f, Max: ~2f~n', 
                     [C, M, Min, Max]))  
   ;   write('Error: No diagnostic thresholds were loaded!'), nl
   ).




% Load ICD thresholds from the generated Prolog file
load_icd_thresholds(FilePath) :-
    (   exists_file(FilePath) ->
        write('Loading ICD thresholds from: '), write(FilePath), nl,
        retractall(icd_threshold(_, _, _, _)),  % Clear old thresholds
        open(FilePath, read, Stream, [encoding(utf8)]),
        repeat,
        read(Stream, Term),  % Read terms from file
        (   Term == end_of_file ->
            close(Stream),
            !
        ;   assertz(Term),  % Store loaded threshold
            fail
        )
    ;   write('Error: File not found.'), nl, fail
    ).


% Process extracted ICD threshold lines
process_icd_threshold_line(Line) :-
    term_string(Term, Line),
    (   Term = icd_threshold(Code, Description, Measurement, Value),
        valid_icd_threshold(Code, Description, Measurement, Value) ->
        assertz(icd_threshold(Code, Description, Measurement, Value)),
        format('Loaded ICD threshold: Code: ~w, Desc: ~s, Measurement: ~s, Value: ~2f~n',
               [Code, Description, Measurement, Value])
    ;   format('Invalid ICD threshold line skipped: ~w~n', [Line])
    ).


% Validate types for each component of the threshold
valid_threshold(_, _, Measurement, _) :-
    member(Measurement, [fasting_glucose, hba1c]).  % Only allow valid ICD terms

% Validate ICD threshold entries
valid_icd_threshold(Code, Description, Measurement, Value) :-
    atom(Code),  % Ensure Code is an atom
    atom(Description),  % Ensure Description is an atom
    atom(Measurement),  % Ensure Measurement is an atom
    number(Value),  % Ensure Value is a number
    member(Measurement, [fasting_glucose, hba1c]).  % Only allow valid ICD terms

validate_loaded_thresholds :-
    findall((Code, Desc, Meas, Val), icd_threshold(Code, Desc, Meas, Val), Thresholds),
    (   Thresholds \= [] ->
        forall(member((Code, Desc, Meas, Val), Thresholds),
               (   format('Valid threshold: Code = ~w, Desc = ~s, Meas = ~s, Val = ~2f~n',
                          [Code, Desc, Meas, Val])
               ))
    ;   write('No thresholds loaded. Check extraction process.'), nl
    ).



list_icd_thresholds :-
    (   exists_file('thresholds.pl') ->  % Check if thresholds file exists
        load_icd_thresholds('thresholds.pl')  % Load thresholds from file
    ;   write('Error: thresholds.pl not found. No ICD thresholds loaded.'), nl,
        fail
    ),
    % Now, retrieve the thresholds from memory
    findall((Code, Desc, Meas, Val), icd_threshold(Code, Desc, Meas, Val), Thresholds),
    (   Thresholds \= [] ->  % If thresholds exist, print them
        forall(member((Code, Desc, Meas, Val), Thresholds),
               format('Code: ~w, Description: ~s, Measurement: ~s, Value: ~2f~n', [Code, Desc, Meas, Val]))
    ;   write('No ICD thresholds loaded.'), nl
    ).



validate_mappings :-
    findall(Mapping, mapping(_, Mapping), Mappings),
    forall(member(Mapping, Mappings),
        format('Valid mapping: ~w~n', [Mapping])
    ).



% Add or update a mapping dynamically
add_mapping(Key, Value) :-
    retractall(mapping(Key, _)),  % Remove any existing mapping for the Key
    assertz(mapping(Key, Value)),
    format('Mapping added: "~w" -> ~w~n', [Key, Value]).

% Remove an existing mapping
remove_mapping(Key) :-
    retractall(mapping(Key, _)),
    format('Mapping for "~w" removed successfully.~n', [Key]).

% List all mappings
list_mappings :-
    findall((Key, Value), mapping(Key, Value), Mappings),
    (   Mappings \= [] ->
        write('Current mappings:'), nl,
        forall(member((Key, Value), Mappings),
               format('~w -> ~w~n', [Key, Value]))
    ;   write('No mappings found.'), nl
    ).

