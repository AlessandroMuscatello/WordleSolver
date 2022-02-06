:-["wordList.txt"].

%Let you know the number and the list of possible word given the known conditions
%combinations(-N, -List, +Clcp, +Clnp, Ncl)
combinations(N, List, Clcp, Clnp, Ncl):-
    findall(W, wordle(W, Clcp, Clnp, Ncl), List),
    length(List, N).

%Provides a word that satisfies the conditions
%wordle(-Word, +Clcp, +Clnp, +Ncl)
wordle(Word, Clcp, Clnp, Ncl):-
    word(Word),
    atom_chars(Word, L),
    length(L, 5),
    clnp(L, Clnp),
    clcp(L, Clcp),
    ncl(L, Ncl).

%CorrectLetterNotPosition
clnp(Word, Clnp):-
    clnp_(Word, Word, 1, Clnp).

clnp_([Lett], Word, 5, [List]):-
    \+member(Lett, List),
    forall(member(L, List), member(L, Word)).
clnp_([Lett|Rest], Word, N, [L1|C]):-
    \+member(Lett, L1),
    forall(member(L, L1), member(L, Word)),
    M is N+1,
    clnp_(Rest, Word, M, C).

%CorrectLetterCorrectPosition
clcp(Word, Clcp):-
    clcp_(Word, 1, Clcp).

clcp_([Lett], 5, [L1]):-
    L1 == [] ; (member(L, L1), Lett == L).
clcp_([Lett|Rest], N, [L1|C]):-
    ((L1 == []) ; (member(L, L1), Lett == L)),
    M is N+1,
    clcp_(Rest, M, C).

ncl([], _).
ncl([Lett|Rest], Ncl):-
    \+member(Lett, Ncl),
    ncl(Rest, Ncl).
