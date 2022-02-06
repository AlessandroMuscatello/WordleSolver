# WordleSolver
My implementation of a Wordle (https://www.powerlanguage.co.uk/wordle/) solver in ProLog

## How it works
Using SWI-Prolog I created this algorithm that return a word or a list of words that are possible solutions of the Wordle game. I used a complete list of english words (more than 50000) and the algoritm looks for words that are 5 letters long and comply with the constraints.

## How to use it
There are two main predicates:

### **wordle(-Word, +Clcp, +Clnp, +Npl)**
 
- `Word` Is a possible solution of the game that complies with the constraints.
 
- `Clcp` (Correct Letter Correct Position) - List of lists containing the known letters in the correct position.
 ```
  Ex: [[],[i],[],[],[]]   means that the solution must have the letter 'i' in the second position
 ```
- `Clnp` (Correct Letter Not Position) - List of lists containing the known letters but in the wrong position.
 ```
  Ex: [[],[],[s,i],[],[]]   means that the solution must have the letter 's' AND the letter 'i' both not in the third position
 ```
- `Npl` (Not Present Letters) - List of letters that are not present in the solution word.
 ```
  Ex: [l,m,e]   means that the letters 'l', 'm', 'e' are not present in the solution word
 ```
 
 ### **combinations(-N, -List, +Clcp, +Clnp, +Npl)**
- `N` Number of possible word solutions that comply with the actual constraints
-`List` List of possible solutions
-`Clcp` (Correct Letter Correct Position) - List of lists containing the known letters in the correct position.
 ```
  Ex: [[],[i],[],[],[]]   means that the solution must have the letter 'i' in the second position
 ```
- `Clnp` (Correct Letter Not Position) - List of lists containing the known letters but in the wrong position.
 ```
  Ex: [[],[],[s,i],[],[]]   means that the solution must have the letter 's' AND the letter 'i' both not in the third position
 ```
- `Npl` (Not Present Letters) - List of letters that are not present in the solution word.
 ```
  Ex: [l,m,e]   means that the letters 'l', 'm', 'e' are not present in the solution word
 ```

## Example on a game
1. Start your game on wordle using a word of your choice (I personally like to use the word "audio")

![](README%20images/wordle1.jpg)

2. Based on the game response on the first word use the Prolog program to predict the next word.
  
  The green letters will be inserted in the 'Clcp' list, the yellow letters in the 'Clnp' list and the grey letters in the 'Npl' list.
  
  We will use the predicate ***combinations()*** like this:
 ```
  ?- combinations(N, L, [[],[],[],[],[]], [[],[],[],[i],[]], [a,u,d,o]).
  N = 501,
  L = [beige, being, bible, biker, bikes, biles, bilge, bills, billy|...].
 ```
3. At this point we need to choose one of the 501 words remaining in the list. We will use 'bikes'

![](README%20images/wordle2.jpg)

4. We are not lucky and we still dont't get any of the green letters... Let's reuse the ***combinations()*** with the new informations
 ```
  ?- combinations(N, L, [[],[],[],[],[]], [[],[i],[k],[i],[s]], [a,u,d,o,b,e]).
  N = 14,
  L = [frisk, shirk, skill, skimp, skirl, skirt, slick, slink, smirk|...].
 ```
We reduced a lot of words! (From 501 to 14).
5. With a bit of Prolog skils we can list completely the 14 words remaining:
 ```
 frisk 
 shirk 
 skill 
 skimp 
 skirl 
 skirt 
 slick 
 slink 
 smirk 
 snick 
 spiky 
 stick 
 stink  
 whisk
 ```
6. Now we need to use a mixture of skill and luck... I choose for the next word 'shirk' a word that begins with 's' and has 'h' in second position, in that way if the 's' is green I will know that 'frisk' and 'whisk' will be excluded but if 's' il green, the solution was one of them, and then the answer of Wordle will disjoin them with the colour of the 'h' (Green means that the solution is 'whisk', otherwise 'frisk' is the solution).

![](README%20images/wordle3.jpg)

7. Good! Again with ***combinations()***
 ```
  ?- combinations(N, L, [[s],[],[i],[],[]], [[],[i],[k],[i],[s]], [a,u,d,o,b,e,h,r]).
  N = 8,
  L = [skill, skimp, slick, slink, snick, spiky, stick, stink].
 ```
 This time we will choose 'skimp', this should skim most of the remaining word (I'm working for an automated process of choosing the next moove)
 
![](README%20images/wordle4.jpg)

8. Finally
```
?-  combinations(N, L, [[s],[k],[i],[],[]], [[],[i],[k],[i],[s]], [a,u,d,o,b,e,h,r,m,p]).
N = 1,
L = [skill].
```
Let's try 'skill' which should be the correct answer
![](README%20images/wordle5.jpg)
  
  
