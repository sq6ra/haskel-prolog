:-ensure_loaded(library(clpfd)).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

samelength([], []).
samelength([_|L1], [_|L2]) :-
	samelength(L1, L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% predicate name : changed_row/3
%% this predicate changes all '_' in a list into a 
%% varable with empty value. 

changed_row([], Accumulator, Accumulator).
changed_row([E|Es], Accumulator, New_row) :-
	( E = '_' ->
		append(Accumulator,[Slot], NewAcc)
	  ; append(Accumulator,[E], NewAcc) 
	 ),
	changed_row(Es, NewAcc, New_row).

%% predicate name : changed_puzzle/3
%% this predicate changes a puzzle into a new puzzle
%% which has its '_' substitudes to a variable without 
%% value; Accumulator should be a [], list of list

changed_puzzle([], Accumulator, Accumulator).
changed_puzzle([Row|Rows], Accumulator, New_puzzle) :-
	changed_row(Row, [], New_row),
	append(Accumulator, [New_row],  NewAcc), 
	changed_puzzle(Rows, NewAcc, New_puzzle).

%% predicate name : find_slots_per_Row/3
%% this predicate returns the slots in a list
%% Slot is defined as two continious variable
%% slot is stored in a list ; Accumulator should
%% be a [], it is a list of list

find_slots_per_Row([],[], Accumulator, Accumulator).
find_slots_per_Row([], SlotStack, Accumulator, Slots_per_row) :-
	length(SlotStack, Len),
	(  Len =< 1 ->
	     find_slots_per_Row([], [], Accumulator, Slots_per_row)
	   ; append(Accumulator, [SlotStack], NewAcc),
	     find_slots_per_Row([], [], NewAcc, Slots_per_row)
	). 
find_slots_per_Row([E|Elems], SlotStack, Accumulator, Slots_per_row) :-
	length(SlotStack , Len),
	(	nonvar(E) -> 
		  (E \= '#' -> append(SlotStack, [E], NewStack), 
		  	find_slots_per_Row(Elems, NewStack, Accumulator, Slots_per_row)
		  	;
		  	( Len >= 2 -> append(Accumulator, [SlotStack], NewAcc),
		      find_slots_per_Row(Elems, [], NewAcc, Slots_per_row)
		      ;
		      find_slots_per_Row(Elems,[],Accumulator,Slots_per_row) 
		    )
		  )
		; 
		  append(SlotStack, [E], NewStack),
		  find_slots_per_Row(Elems, NewStack, Accumulator, Slots_per_row)
	).

%% predicate name : find_half_Slots/3
%% call the predicate 'find_slots_per_Row'
%% find all slots in changed puzzle

find_half_Slots([], Accumulator, Accumulator).
find_half_Slots([R|Rows], Accumulator, Half_Slots) :-
	find_slots_per_Row(R, [], [], Slots_per_row),
	append(Slots_per_row, Accumulator, NewAcc),
	find_half_Slots(Rows, NewAcc, Half_Slots).

%% predicate name : find_all_Slots/3
%% call predicate 'find_all_Slots'
%% find all Slots in a changed puzzle 

find_all_Slots([], Accumulator, Accumulator).
find_all_Slots(Hor_puzzle, Accumulator, All_Slots) :-
	find_half_Slots(Hor_puzzle, [], Hor_Slots),
	append(Hor_Slots, Accumulator, NewAcc),
	transpose(Hor_puzzle, Ver_puzzle),
	find_half_Slots(Ver_puzzle, [], Ver_Slots),
	append(Ver_Slots, NewAcc, Fin_Acc),
	find_all_Slots([], Fin_Acc, All_Slots).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reduce_list([], _, []).
reduce_list([Head|Tail], A, B) :-
    check_elems(Head, A),
    reduce_list(Tail, A, B).
reduce_list([Head|A], Tail, [Head|B]) :- reduce_list(A, Tail, B).

%% to find out if a element in a list or not
%% input : A element, A list 
%% True is the element in list, otherwise 

check_elems(Elem, [Head|_]) :- Elem == Head.
check_elems(Elem, [_|Tail]) :- check_elems(Elem, Tail).

%% predicate name : filter_wordslist/3
%% this predicate together with sorted words
%% return a list sorted by word frequency 

filter_wordslist([], _, []).
filter_wordslist([Head|Tail], WordList, Filterd_Wordslist) :-
    sorted_words(Head, WordList, Filterd_Words),
    append([Filterd_Words], Filterd_Wordslist_tmp, Filterd_Wordslist),
    filter_wordslist(Tail, WordList, Filterd_Wordslist_tmp).

sorted_words(_, [], []).
sorted_words(Word, [Head|Tail], Filterd_Words) :-
    length(Word, L1),
    length(Head, L2),
    (   L1 = L2
    ->  pare(Word, Head, Result),
        (   Result = true
        ->  append([Head], Filterd_Words1, Filterd_Words)
        ;   append([], Filterd_Words1, Filterd_Words)
        ),
        sorted_words(Word, Tail, Filterd_Words1)
    ;   sorted_words(Word, Tail, Filterd_Words)
    ).
%% predicate pare\2
%% pare up two inputed words, if succuess
%% the argument of 'Result' is true;
%% called by the sorted_words\2 

pare([], [], true).
pare([Head0|Rest0], [Head1|Rest1], Result) :-
    (   nonvar(Head0), nonvar(Head1), Head0 = Head1
    ->  pare(Rest0, Rest1, Result);
        var(Head0)
    ->  pare(Rest0, Rest1, Result);  
        Result = false
    ).

%% predicate name : fill/2
%% this predicate fill the word into a word list 

fill(Word, [Word]).
fill(Word, [W|Ws]) :-
    Word = W ; fill(Word, Ws).

%% predicate name index_wordslist/2
%% this predicate take 2 argus and 
%% return the list with index

index_wordslist(Filterd_Wordslist, N) :-
    list_by_length(Filterd_Wordslist, List),
    min_list(List, Mth),
    nth1(N, List, Mth).

%% a predicate to input a list of list A
%% and return a length list for each list in A 

list_by_length([],[]).
list_by_length([Head|Tail], List) :-
    length(Head, L),
    append([L], List_tmp, List),
    list_by_length(Tail, List_tmp).


%% predicate name : find_solution/3
%% return a solution of inputed slots and WordList
%% called by solve_puzzle

find_solution([],[]).
find_solution(Slot_word, WordList) :-
    filter_wordslist(Slot_word, WordList, Filterd_Wordslist),
    index_wordslist(Filterd_Wordslist, N),
    nth1(N, Slot_word, Word, Rest_words),
    nth1(N, Filterd_Wordslist, Filterd_Words),
    fill(Word, Filterd_Words),
    reduce_list(Slot_word, [Word], Rest_words),
    reduce_list(WordList, [Word], Rest_WordList),
    find_solution(Rest_words, Rest_WordList).

%% predicate name : solve_puzzle/3
%% should hold when Puzzle is a solved version of Puzzle0, with the
%% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
%% should be lists of lists of characters (single-character atoms), one
%% list per puzzle row.  WordList is also a list of lists of
%% characters, one list per word.

solve_puzzle(Puzzle, Wordlist, Solved) :-
	changed_puzzle(Puzzle,[], New_puzzle),
	find_all_Slots(New_puzzle,[], All_Slots), 
	find_solution(All_Slots, Wordlist),
	Solved = New_puzzle.

