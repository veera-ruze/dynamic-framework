:- module(guideline_import, [import_guideline/1]).  % Define the module at the very beginning

:- dynamic threshold/3, imported_criteria/2.       % Declare dynamic predicates
:- dynamic diagnostic_threshold/4.

% Map guideline terms to Prolog-friendly terms
map_guideline_to_prolog('fasting_glucose', fasting_glucose).
map_guideline_to_prolog('HbA1c', hba1c).


% Import a guideline and validate thresholds
import_guideline(FilePath) :-
    exists_file(FilePath),
    load_thresholds_from_file(FilePath),
    validate_thresholds,
    write('Guideline thresholds imported and validated successfully!'), nl.

% Process each line of the guideline
process_guideline_line(Line) :-
    split_string(Line, ",", "", [Condition, Measurement, ValueStr]),
    map_guideline_to_prolog(Measurement, MappedMeasurement), % Ensure proper mapping
    atom_string(CondAtom, Condition),
    catch(number_string(Value, ValueStr), _, fail), % Safely convert value to number
    assertz(threshold(CondAtom, MappedMeasurement, Value)), % Add to threshold
    assertz(imported_criteria(CondAtom, MappedMeasurement)), % Track imports
    format('Imported: ~w, ~w = ~2f~n', [CondAtom, MappedMeasurement, Value]).



% List imported criteria
list_imported_criteria :-
    findall((Condition, Measurement), imported_criteria(Condition, Measurement), Criteria),
    (   Criteria \= [] ->
        write('Imported criteria:'), nl,
        forall(member((Cond, Meas), Criteria),
               format('Condition: ~w, Measurement: ~w~n', [Cond, Meas]))
    ;   write('No criteria have been imported yet.'), nl
    ).
