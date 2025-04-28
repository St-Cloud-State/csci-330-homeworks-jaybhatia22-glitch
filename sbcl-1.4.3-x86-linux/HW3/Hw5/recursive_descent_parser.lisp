(defvar *tokens* '())

(defun match (expected)
  "Matches the current token if it is the expected token."
  (if (and *tokens* (eq (car *tokens*) expected))
      (setf *tokens* (cdr *tokens*))
      (error "Syntax error: Expected ~A but found ~A" expected (car *tokens*))))

(defun lookahead ()
  "Returns the next token without consuming it."
  (if *tokens* (car *tokens*) nil))

(defun parse-G ()
  (let ((tok (lookahead)))
    (if (member tok '(x y z w))
        (match tok)
        (error "Syntax error in G"))))

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
      (error "Syntax error in E")))

(defun parse-L-prime ()
  "Handles the recursive part of L."
  (let ((tok (lookahead)))
    (cond
      ((eq tok 's) (match 's) (parse-L-prime))
      ((eq tok 'b) (match 'b)))))

(defun parse-L ()
  "Parses the L non-terminal."
  (if (eq (lookahead) 's)
      (progn
        (match 's)
        (parse-L-prime))
      (error "Syntax error in L")))

(defun parse-S ()
  "Parses the S non-terminal."
  (let ((tok (lookahead)))
    (cond
      ((eq tok 's) (match 's))
      ((eq tok 'd) (match 'd) (parse-L-prime))
      (t (error "Syntax error in S")))))

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
  (parse-I)
  (if *tokens*
      (error "Syntax error: Extra tokens found")
      "Parsing successful!"))

;; Example usage:
(parse '(i x o y o w d s s b e s))  ;; Example valid input
;; (parse '(i x o y o w d s s b e x)) ;; Example invalid input

