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

%CorrectLetterNotPosition - Checks if the Word has the letter in the Clnp list of letters but not in the position where are listed.
%Basically check the yellow letters of Wordle.
%Ex:    Clnp = [[],[],[],[],[]],   Word = "audio" -> SATISFIED
%       Clnp = [[],[a],[i],[],[]], Word = "audio" -> SATISFIED
%       Clnp = [[],[a],[a,i],[],[]] Word = "audio" -> SATISFIED
%       Clnp = [[a],[],[o],[],[]], Word = "audio" -> NOT SATISFIED
        
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

%CorrectLetterCorrectPosition - Checks if the Word has the correct letter in the correct position.
%Basically checks the green letters of Wordle.
%Ex:    Clcp = [[],[],[],[],[]], Word = "audio -> SATISFIED
%       Clcp = [[a],[],[],[i],[]], Word = "audio" -> SATISFIED
%       Clcp = [[a],[d],[],[],[]], Word = "audio" -> NOT SATISFIED

clcp(Word, Clcp):-
    clcp_(Word, 1, Clcp).

clcp_([Lett], 5, [L1]):-
    L1 == [] ; (member(L, L1), Lett == L).
clcp_([Lett|Rest], N, [L1|C]):-
    ((L1 == []) ; (member(L, L1), Lett == L)),
    M is N+1,
    clcp_(Rest, M, C).

%NotCorrectLetters - Checks if the Word has the wrong letters
%Basically checks the red letters of Wordle
%Ex:    Ncl = [],       Word = "audio" -> SATISFIED
%       Ncl = [b,c],    Word = "audio" -> SATISFIED
%       Ncl = [a],      Word = "audio" -> NOT SATISFIED

ncl([], _).
ncl([Lett|Rest], Ncl):-
    \+member(Lett, Ncl),
    ncl(Rest, Ncl).
