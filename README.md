# functions

This library provides higher-order functions for functional programming in Common Lisp. It includes utilities for function composition, currying, partial application, and argument manipulation.

## Functions

### `binary-compose-left`
Combines two functions such that the result of the second function is passed as the left argument to the first function.

#### Example:
```lisp
CL-USER> (funcall (binary-compose-left #'* #'1+) 3 4)
16
;; Explanation: (1+ 3) = 4, then (* 4 4) = 16
```

### `binary-compose-right`
Combines two functions such that the result of the second function is passed as the right argument to the first function.

#### Example:
```lisp
CL-USER> (funcall (binary-compose-right #'* #'1+) 3 4)
12
;; Explanation: (1+ 4) = 5, then (* 3 5) = 15
```

### `compose2`
Combines two functions such that the result of the second function is passed as the argument to the first function.

#### Example:
```lisp
CL-USER> (funcall (compose2 #'sqrt #'1+) 8)
3
;; Explanation: (1+ 8) = 9, then (sqrt 9) = 3
```

### `compose`
Combines multiple functions such that the result of each function is passed as the argument to the next function.

#### Example:
```lisp
CL-USER> (funcall (compose #'sqrt #'1+ #'1-) 9)
3
;; Explanation: (1- 9) = 8, (1+ 8) = 9, then (sqrt 9) = 3
```

### `curry-left`
Curries a binary function by fixing its left argument.

#### Example:
```lisp
CL-USER> (funcall (funcall (curry-left #'expt) 2) 3)
8
;; Explanation: Fixing the left argument as 2, computes (expt 2 3) = 8
```

### `curry-right`
Curries a binary function by fixing its right argument.

#### Example:
```lisp
CL-USER> (funcall (funcall (curry-right #'expt) 3) 2)
9
;; Explanation: Fixing the right argument as 3, computes (expt 2 3) = 9
```

### `partial-apply-left`
Partially applies a binary function by fixing its left argument.

#### Example:
```lisp
CL-USER> (funcall (partial-apply-left #'mod 10) 3)
1
;; Explanation: Fixing the left argument as 10, computes (mod 10 3) = 1
```

### `partial-apply-right`
Partially applies a binary function by fixing its right argument.

#### Example:
```lisp
CL-USER> (funcall (partial-apply-right #'mod 3) 10)
1
;; Explanation: Fixing the right argument as 3, computes (mod 10 3) = 1
```

### `swap-arguments`
Swaps the arguments of a binary function.

#### Example:
```lisp
CL-USER> (funcall (swap-arguments #'/) 2 10)
5
;; Explanation: Swapping arguments computes (/ 10 2) = 5
```
