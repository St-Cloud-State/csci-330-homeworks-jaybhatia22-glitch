(defvar *tokens* '())
(defvar *position* 0)  ; To keep track of the current position in the token list

(defun match (expected)
  "Matches the current token if it is the expected token."
  (if (and *tokens* (eq (car *tokens*) expected))
      (progn
        (incf *position)  ; Increment position when a match is found
        (setf *tokens* (cdr *tokens*)))
      (error "Syntax error at position ~A: Expected ~A but found ~A"
             *position* expected (car *tokens*))))

(defun lookahead ()
  "Returns the next token without consuming it."
  (if *tokens* (car *tokens*) nil))

(defun parse-G ()
  (let ((tok (lookahead)))
    (if (member tok '(x y z w))
        (match tok)
        (error "Syntax error at position ~A in G: Expected one of '(x y z w)' but found ~A"
               *position* tok))))

(defun parse-E-prime ()
  "Handles the recursive part of the E non-terminal."
  (when (eq (lookahead) 'o)
    (match 'o)
    (parse-G)
    (parse-E-prime)))

(defun parse-E ()
  "Parses the E non-terminal."
  (if (member (lookahead) '(x y z w))
      (progn
        (parse-G)
        (parse-E-prime))
      (error "Syntax error at position ~A in E: Expected 'x', 'y', 'z', or 'w' but found ~A"
             *position* (lookahead))))

(defun parse-L-prime ()
  "Handles the recursive part of L."
  (let ((tok (lookahead)))
    (cond
      ((eq tok 's) (match 's) (parse-L-prime))
      ((eq tok 'b) (match 'b))
      (t (error "Syntax error at position ~A in L-prime: Unexpected token ~A"
               *position* tok)))))

(defun parse-L ()
  "Parses the L non-terminal."
  (if (eq (lookahead) 's)
      (progn
        (match 's)
        (parse-L-prime))
      (error "Syntax error at position ~A in L: Expected 's' but found ~A"
             *position* (lookahead))))

(defun parse-S ()
  "Parses the S non-terminal."
  (let ((tok (lookahead)))
    (cond
      ((eq tok 's) (match 's))
      ((eq tok 'd) (match 'd) (parse-L-prime))
      (t (error "Syntax error at position ~A in S: Expected 's' or 'd' but found ~A"
               *position* tok)))))

(defun parse-I ()
  "Parses the I non-terminal."
  (match 'i)
  (parse-E)
  (parse-S)
  (when (eq (lookahead) 'e)
    (match 'e)
    (parse-S)))

(defun parse (tokens)
  "Entry point for parsing."
  (setf *tokens* tokens)
  (setf *position* 0)  ; Reset position before parsing
  (parse-I)
  (if *tokens*
      (error "Syntax error at position ~A: Extra tokens found: ~A"
             *position* *tokens*)
      "Parsing successful!"))

;; Example usage:
(parse '(i x o y o w d s s b e s))  ;; Example valid input
;; (parse '(i x o y o w d s s b e x)) ;; Example invalid input
