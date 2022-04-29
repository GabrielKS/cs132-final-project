
Original grammar:
```
S -> E
E -> T | E+T
T -> id | (E)
```

Augmented and numbered grammar:
```
(0) E' -> E
(1) E -> E+T
(2) E -> T
(3) T -> (E)
(4) T -> id
```

These are taken directly from the handout:

Johnson, M., & Zelenski, J. (2008). CS143 Handout 08: Bottom-Up Parsing. Stanford University. https://suif.stanford.edu/dragonbook/lecture-notes/Stanford-CS143/08-Bottom-Up-Parsing.pdf
