(module batmon-prog ()

(import (scheme)
        (chicken base)
        (chicken file)
        (chicken format)
        (chicken pathname)
        (chicken process-context))
(import batmon)

(define (mainloop)
  (let loop ()
    (let ((battery-dirs (list-battery-dirs)))
      (for-each
       (lambda (battery-dir)
         (update-db! battery-dir (read-battery-data battery-dir)))
       battery-dirs))
    (sleep (poll-interval))
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
                 (poll-interval (string->number interval))
                 (unless (and (integer? (poll-interval))
                              (positive? (poll-interval)))
                   (die! "--poll-interval: invalid argument: ~a" interval))
                 (loop (cddr args))))
              (else
               (die! "Invalid parameter: ~a" arg))))))
  (mainloop))

) ;; end module
