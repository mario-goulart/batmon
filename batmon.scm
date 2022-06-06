(module batmon ()

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

(define max-db-items 50000)

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

(define (mainloop poll-interval)
  (create-directory (base-data-dir) 'parents)
  (let loop ()
    (let ((battery-dirs (list-battery-dirs)))
      (for-each
       (lambda (battery-dir)
         (let* ((battery (pathname-file battery-dir))
                (db-file (make-pathname (base-data-dir) battery "db"))
                (db-data (handle-exceptions exn
                           '()
                           (with-input-from-file db-file read-list)))
                (num-db-items (length db-data))
                (sample (read-battery-data battery-dir)))
           (if (< num-db-items (sub1 max-db-items))
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
                             (append (take-right db-data (sub1 max-db-items))
                                     (list sample))))))))
       battery-dirs))
    (sleep poll-interval)
    (loop)))

(define (die! fmt . args)
  (apply fprintf (cons (current-error-port)
                       (cons (string-append fmt "\n")
                             args)))
  (exit 1))

(define (usage #!optional exit-code)
  (let* ((port (if (and exit-code (not (zero? exit-code)))
                   (current-error-port)
                   (current-output-port)))
         (prog (pathname-strip-directory (program-name)))
         (msg #<#EOF
Usage: #prog [<options>]

<options>:
--data-dir|-d <dir>
  Directory where collected battery data will be written to.
  Default: ~~/.cache/batmon-data

--poll-interval|-p <seconds>
  Number of seconds between two consecutive sample collections.
  Default: 120 (two minutes)

EOF
))
    (fprintf port msg)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((poll-interval 120))
    (let loop ((args (command-line-arguments)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((or (string=? arg "--data-dir") (string=? arg "-d"))
                 (when (null? (cdr args))
                   (die! "--data-dir requires an argument"))
                 (base-data-dir (cadr args))
                 (loop (cddr args)))
                ((or (string=? arg "--poll-interval") (string=? arg "-p"))
                 (when (null? (cdr args))
                   (die! "--poll-interval requires an argument"))
                 (let ((interval (cadr args)))
                   (set! poll-interval (string->number interval))
                   (unless (and (integer? poll-interval)
                                (positive? poll-interval))
                     (die! "--poll-interval: invalid argument: ~a" interval))
                   (loop (cddr args))))
                (else
                 (die! "Invalid parameter: ~a" arg))))))
    (mainloop poll-interval)))

) ;; end module
