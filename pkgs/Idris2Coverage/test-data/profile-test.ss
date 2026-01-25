#!/opt/homebrew/bin/chez --script

;; Test to see profile-dump-list output format
(compile-profile #t)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (test)
  (display (factorial 5))
  (newline))

(test)

;; Dump profile data in list format
(let ((profile-data (profile-dump-list)))
  (display "=== Profile Data ===\n")
  (pretty-print profile-data)
  (newline))
