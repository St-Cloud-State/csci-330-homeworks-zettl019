;; Helper: Match token
(defun match (token tokens)
  (if (and tokens (string= token (car tokens)))
      (cdr tokens)
      (error "Expected ~A but found ~A" token (if tokens (car tokens) "end of input"))))

;; Non-terminal parser for G:
;; G -> x | y | z | w
(defun parse-G (tokens)
  (if (and tokens (member (car tokens) '("x" "y" "z" "w") :test #'string=))
      (cons (list 'G (car tokens)) (cdr tokens))
      (error "Expected x, y, z, or w but found ~A" (if tokens (car tokens) "end of input"))))

;; Non-terminal parser for E′:
;; E′ -> o G E′ | ε
(defun parse-Eprime (tokens)
  (if (and tokens (string= (car tokens) "o"))
      (let ((tokens (match "o" tokens)))
        (destructuring-bind (g-result . tokens) (parse-G tokens)
          (destructuring-bind (eprime-result . tokens) (parse-Eprime tokens)
            (cons (list 'Eprime "o" g-result eprime-result) tokens))))
      (cons 'epsilon tokens)))

;; Non-terminal parser for E:
;; E -> G E′
(defun parse-E (tokens)
  (destructuring-bind (g-result . tokens) (parse-G tokens)
    (destructuring-bind (eprime-result . tokens) (parse-Eprime tokens)
      (cons (list 'E g-result eprime-result) tokens))))

;; Non-terminal parser for S:
;; S -> s | d L b
(defun parse-S (tokens)
  (cond
    ((and tokens (string= (car tokens) "s"))
     (cons (list 'S "s") (cdr tokens)))
    ((and tokens (string= (car tokens) "d"))
     (let ((tokens (match "d" tokens)))
       (destructuring-bind (l-result . tokens) (parse-L tokens)
         (let ((tokens (match "b" tokens)))
           (cons (list 'S "d" l-result "b") tokens)))))
    (t (error "Expected s or d but found ~A" (if tokens (car tokens) "end of input")))))

;; Non-terminal parser for L′:
;; L′ -> s L′ | ε
(defun parse-Lprime (tokens)
  (if (and tokens (string= (car tokens) "s"))
      (let ((tokens (match "s" tokens)))
        (destructuring-bind (lprime-result . tokens) (parse-Lprime tokens)
          (cons (list 'Lprime "s" lprime-result) tokens)))
      (cons 'epsilon tokens)))

;; Non-terminal parser for L:
;; L -> s L′
(defun parse-L (tokens)
  (let ((tokens (match "s" tokens)))
    (destructuring-bind (lprime-result . tokens) (parse-Lprime tokens)
      (cons (list 'L "s" lprime-result) tokens))))

;; Non-terminal parser for I:
;; I -> i E S | i E S e S
(defun parse-I (tokens)
  (let ((tokens (match "i" tokens)))
    (destructuring-bind (e-result . tokens) (parse-E tokens)
      (destructuring-bind (s-result . tokens) (parse-S tokens)
        (if (and tokens (string= (car tokens) "e"))
            (let ((tokens (match "e" tokens)))
              (destructuring-bind (s2-result . tokens) (parse-S tokens)
                (cons (list 'I "i" e-result s-result "e" s2-result) tokens)))
            (cons (list 'I "i" e-result s-result) tokens))))))

;; Main parser: converts an input string into a list of tokens and then parses.
(defun parse-input (input-string)
  (let* ((tokens (loop for i from 0 below (length input-string)
                       collect (subseq input-string i (1+ i))))
         (result (parse-I tokens)))
    (if (cdr result)
        (error "Unconsumed tokens: ~A" (cdr result))
        (car result))))
