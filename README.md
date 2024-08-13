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

#### 8 queen problem

```js
const N = 8;

function isSafe(board, row, col) {
  // check if there is a queen in the same row to the left
  for (let x = 0; x < col; x++) {
    if (board[row][x] == 1) {
      return false;
    }
  }

  // check if there is a queen in the upper left diagonal
  for (let x = row, y = col; x >= 0 && y >= 0; x--, y--) {
    if (board[x][y] == 1) {
      return false;
    }
  }

  for (let x = row, y = col; x < N && y >= 0; x++, y--) {
    if (board[x][y] == 1) {
      return false;
    }
  }

  return true;
}

function solveNQueens(board, col) {
  if (col == N) {
    for (let i = 0; i < N; i++) {
      console.log(board[i].join(" "));
    }
    console.log("\n");
    return true;
  }

  for (let i = 0; i < N; i++) {
    if (isSafe(board, i, col)) {
      board[i][col] = 1;
      if (solveNQueens(board, col + 1)) {
        return true;
      }
      board[i][col] = 0;
    }
  }

  return false;
}

let board = Array(N)
  .fill()
  .map(() => Array(N).fill(0));
```
