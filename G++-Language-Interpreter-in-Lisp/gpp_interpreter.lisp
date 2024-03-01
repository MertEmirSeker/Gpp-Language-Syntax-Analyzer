; Symbols and Operators
(setq operators '("+" "-" "/" "*" "(" ")" ","))

; Corresponding list of symbols for operators
(setq ops '(OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA))

; Keywords
(setq Keywords '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "def" "for" "if" "exit" "load" "display" "true" "false"))
(setq KWS '(KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE))

; Function to find the greatest common divisor of two numbers
(defun greatest-common-divisor (a b)
  "Finds the greatest common divisor of two numbers."
  (if (= b 0)
      a
      (greatest-common-divisor b (mod a b))))

; Function to simplify a fraction
(defun simplify-fraction (frac)
  "Simplifies a fraction."
  (let ((n (first frac))
        (d (second frac)))
    (let ((gcd (greatest-common-divisor (abs n) (abs d))))
      (list (/ n gcd) (/ d gcd)))))

; Function to perform an operation on two fractions
(defun operate-fractions (op frac1 frac2)
  "Performs an operation on two fractions."
  (cond
   ((equal op 'OP_PLUS) (add-fractions frac1 frac2))
   ((equal op 'OP_MINUS) (subtract-fractions frac1 frac2))
   ((equal op 'OP_MULT) (multiply-fractions frac1 frac2))
   ((equal op 'OP_DIV) (divide-fractions frac1 frac2))))

; Function to add two fractions
(defun add-fractions (frac1 frac2)
  (let ((n1 (first frac1)) (d1 (second frac1))
        (n2 (first frac2)) (d2 (second frac2)))
    (if (= d1 d2)
        (list (+ n1 n2) d1)
      (let ((common-denominator (* d1 d2))
            (num1 (* n1 d2))
            (num2 (* n2 d1)))
        (simplify-fraction (list (+ num1 num2) common-denominator))))))

; Function to subtract two fractions
(defun subtract-fractions (frac1 frac2)
  (let ((n1 (first frac1)) (d1 (second frac1))
        (n2 (first frac2)) (d2 (second frac2)))
    (if (= d1 d2)
        (list (- n1 n2) d1)
      (let ((common-denominator (* d1 d2))
            (num1 (* n1 d2))
            (num2 (* n2 d1)))
        (simplify-fraction (list (- num1 num2) common-denominator))))))

; Function to multiply two fractions
(defun multiply-fractions (frac1 frac2)
  (let ((n1 (first frac1)) (d1 (second frac1))
        (n2 (first frac2)) (d2 (second frac2)))
    (simplify-fraction (list (* n1 n2) (* d1 d2)))))

; Function to divide two fractions
(defun divide-fractions (frac1 frac2)
  (let ((n1 (first frac1)) (d1 (second frac1))
        (n2 (first frac2)) (d2 (second frac2)))
    (if (= n2 0) (error "Division by zero")
        (simplify-fraction (list (* n1 d2) (* d1 n2))))))

; Function to parse a fraction from a string in the format "n/d"
(defun parse-fraction (str)
  "Parses a fraction string in the format 'n/d'."
  (let ((b-pos (position #\b str :test #'char-equal)))
    (if b-pos
        (let ((numerator (parse-integer (subseq str 0 b-pos)))
              (denominator (parse-integer (subseq str (1+ b-pos)))))
          (simplify-fraction (list numerator denominator)))
      (list (parse-integer str) 1))))

; Function to convert an operator string to a symbol
(defun convert-op-to-symbol (op)
  "Converts an operator string to a symbol."
  (cond
   ((string= op "+") 'OP_PLUS)
   ((string= op "-") 'OP_MINUS)
   ((string= op "*") 'OP_MULT)
   ((string= op "/") 'OP_DIV)))

; Hash table to store function definitions
(defvar *function-definitions* (make-hash-table :test 'equal))

; Function to parse a function definition and store it in the hash table
(defun parse-function-definition (tokens)
  "Parses a function definition and returns function name, parameters, and body."
  (when (and (listp tokens)
             (equal (car tokens) 'def))  ; Is it a list starting with `def`?
    (let* ((function-name (cadr tokens))                   ; Get the function name
           (rest-tokens (cddr tokens))                    ; Tokens after the function name
           (parameters (loop while (and (consp rest-tokens) (not (consp (car rest-tokens))))
                             collect (car rest-tokens)
                             do (setq rest-tokens (cdr rest-tokens)))) ; Collect parameters
           (body (first rest-tokens)))                     ; Get the body as a single list
      (setf (gethash function-name *function-definitions*)
            (list parameters body))
      ; Print the parsed information
      (format t "#function ")function-name)))

; Function to evaluate a function call
(defun eval-function-call (function-name args)
  (let ((function-def (gethash function-name *function-definitions*)))
    (when function-def
      (let* ((parameters (first function-def))
             (body (second function-def))
             (arg-values (mapcar #'eval-expression args)))  ; Evaluate arguments
        (if (= (length parameters) (length arg-values))
            (let ((param-subst (pairlis parameters arg-values)))  ; Match parameters with argument values
              (eval-expression (substitute-parameters body param-subst)))  ; Evaluate the body
          (error "Incorrect number of arguments for function ~a" function-name))))))

; Function to substitute parameters in the body of a function
(defun substitute-parameters (body substitutions)
  (if (atom body)
      (or (cdr (assoc body substitutions)) body)  ; Replace the parameter with the corresponding argument value
      (mapcar (lambda (x) (substitute-parameters x substitutions)) body)))  ; Recurse for list elements

; Function to evaluate an expression
(defun eval-expression (exp)
  ;; Handle `$EXP` expressions
  (cond
    ((numberp exp) (list exp 1))
    ((symbolp exp) (parse-fraction (symbol-name exp)))
    ((and (listp exp) (equal (car exp) 'def))
     (parse-function-definition exp))
    ((and (listp exp) (symbolp (car exp)))
     (let ((op (car exp)) (args (cdr exp)))
       (if (gethash op *function-definitions*)
           (eval-function-call op args)
         (let ((arg-values (mapcar #'eval-expression args)))
           (cond
             ((equal op '+) (apply #'add-fractions arg-values))
             ((equal op '-) (apply #'subtract-fractions arg-values))
             ((equal op '*) (apply #'multiply-fractions arg-values))
             ((equal op '/) (apply #'divide-fractions arg-values))
             (t (error "Unknown operation")))))))
    (t exp)))

; Function to evaluate G++ expressions
(defun gpp-eval (input)
  (handler-case
      (let ((exp (read-from-string input)))
        (let ((result (eval-expression exp)))
          (if (listp result)
              (format nil "~ab~a" (car result) (cadr result))
            result)))
    (error () (format nil "Syntax error!"))))

; Main loop of the program
(defun gppinterpreter ()
  ;; Input loop
  (format t "Welcome to the G++ Language Interpreter in Lisp.~%")
  (format t "Would you like to read from a file? (yes/no): ")
  (let ((response (read-line)))
    (cond
     ((string-equal response "yes")
      (format t "Enter the file name (Hint: input.g++): ")
      (let ((file-name (read-line)))
        (when (probe-file file-name)
          (with-open-file (stream file-name)
            (loop for line = (read-line stream nil nil)
                  while line do
                    (if (string-equal line "(exit)")
                        (return (format t "Exiting the G++ Language Lexer in Lisp. Goodbye!!!~%"))
                        (format t "~a~%" (gpp-eval line))))))))
     (t
      (format t "Entering interactive mode. Type (exit) to exit. Please enter a code:~%")
      (loop
        (format t "> ")
        (let ((input (read-line nil nil)))
          (cond
           ((or (null input) (string-equal input "(exit)"))
            (return (format t "Exiting the G++ Language Lexer in Lisp. Goodbye!!!~%")))
           (t (format t "~a~%" (gpp-eval input))))))))))




; Start the program
(gppinterpreter)
