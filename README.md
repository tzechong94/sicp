# SICP

## Applicative order vs normal order

- Fully expand then reduce is called normal order
- Operate then substitute with abstracted function is applicative order
- Lisp uses applicative order evaluation. Additional efficiency obtained from avoiding multiple evaluation of expressions.
- Normal order eval becomes much more complex

## conditional predicates
<, =, >, and, or, not

## Linear Recursion and Iteration

fermat's little theorem
- if n is a prime number and a is any positive integer less than n, then a raised to the nth power is congruent to a modulo n

congruent modulo n if they both have the same remainder when divided by n.

## Data abstraction

- structure programs that are to use compound data objects so that they operate on 'abstract data'
- seelctors and constructors to implement abstract data in terms of the concrete representation
- concrete data representation is defined independent of the programs that use the data

### Pairs

- use cons. cons takes two arguments, returns a compound data object that contains two arguments as parts. given a pair, we can extract the parts using the primitive procedure car and cdr. 

- cons provide primitive glue to construct compound data objects
- box and pointer notation

