(module batmon *

(import (scheme)
        (chicken base)
        (chicken condition)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken pathname)
        (chicken process-context)
        (chicken time)
        (chicken time posix))
(import srfi-1)

(define max-db-items
  (make-parameter 50000))

(define base-data-dir
  (make-parameter
   (make-pathname (list (get-environment-variable "HOME") ".cache")
                  "batmon-data")))

(define (list-battery-dirs)
  (glob "/sys/class/power_supply/BAT*"))

(define (read-file file)
  (with-input-from-file file read-line))

(define (read-battery-data battery-dir)
  (let ((capacity
         (string->number (read-file (make-pathname battery-dir "capacity"))))
        (status (read-file (make-pathname battery-dir "status"))))
    (list (current-seconds)
          capacity
          (string->symbol (string (string-ref status 0))))))

(define (update-db! battery-dir sample)
  ;; sample => (<seconds> <capacity> <char>), where <char> is a
  ;; one-letter symbol:
  ;; * C => Charging
  ;; * D => Discharging
  ;; * U => Unknown
  (let* ((battery (pathname-file battery-dir))
         (db-file (make-pathname (base-data-dir) battery "db"))
         (db-data (handle-exceptions exn
                    '()
                    (with-input-from-file db-file read-list)))
         (num-db-items (length db-data)))
    (if (< num-db-items (sub1 (max-db-items)))
        (with-output-to-file db-file
          (lambda ()
            (write sample)
            (newline))
          append:)
        (with-output-to-file db-file
          (lambda ()
            (for-each (lambda (item)
                        (write item)
                        (newline))
                      (append (take-right db-data (sub1 (max-db-items)))
                              (list sample))))))))

) ;; end module
