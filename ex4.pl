:- module('ex4',
        [author/2,
         genre/2,
         book/4
        ]).

/*
 * **********************************************
 * Printing result depth
 *
 * You can enlarge it, if needed.
 * **********************************************
 */

maximum_printing_depth(100).
:- current_prolog_flag(toplevel_print_options, A),
   (select(max_depth(_), A, B), ! ; A = B),
   maximum_printing_depth(MPD),
   set_prolog_flag(toplevel_print_options, [max_depth(MPD)|B]).



author(a, asimov).
author(h, herbert).
author(m, morris).
author(t, tolkien).

genre(s, science).
genre(l, literature).
genre(sf, science_fiction).
genre(f, fantasy).

book(inside_the_atom, a, s, s(s(s(s(s(zero)))))).
book(asimov_guide_to_shakespeare, a, l, s(s(s(s(zero))))).
book(i_robot, a, sf, s(s(s(zero)))).
book(dune, h, sf, s(s(s(s(s(zero)))))).
book(the_well_at_the_worlds_end, m, f, s(s(s(s(zero))))).
book(the_hobbit, t, f, s(s(s(zero)))).
book(the_lord_of_the_rings, t, f, s(s(s(s(s(s(zero))))))).

% Helper to compare Church numerals
greater_than(zero, zero) :- false.
greater_than(s(_), zero).
greater_than(s(X), s(Y)) :- greater_than(X, Y).

% max_list(List, Max) implementation
max_list([], false).
max_list([X], X).
max_list([X|Xs], Max) :-
    max_list(Xs, TempMax),
    (greater_than(X, TempMax) -> Max = X ; Max = TempMax).

% author_of_genre(GenreName, AuthorName)
author_of_genre(GenreName, AuthorName) :-
    genre(GenreId, GenreName),
    author(AuthorId, AuthorName),
    book(_, AuthorId, GenreId, _).

% longest_book(AuthorName, BookName)
longest_book(AuthorName, BookName) :-
    author(AuthorId, AuthorName),
    findall((Length, Name),
            book(Name, AuthorId, _, Length),
            Pairs),
    extract_lengths(Pairs, Lengths),
    max_list(Lengths, Max),
    member((Max, BookName), Pairs).

extract_lengths([], []).
extract_lengths([(Len, _)|Rest], [Len|Others]) :-
    extract_lengths(Rest, Others).

/** <examples>

?- max_list([s(zero), s(s(zero)), zero], Max).
?- author_of_genre(fantasy, Author).
?- longest_book(tolkien, Book).

*/