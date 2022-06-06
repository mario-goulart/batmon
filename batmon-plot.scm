(module batmon ()

(import (scheme)
        (chicken base)
        (chicken condition)
        (chicken file)
        (chicken format)
        (chicken io)
        (chicken pathname)
        (chicken process)
        (chicken process-context)
        (chicken time)
        (chicken time posix))
(import srfi-1 srfi-13)

(define db-item-time car)
(define db-item-capacity cadr)
(define db-item-status caddr)

(define (db-item-charging? db-item)
  (eq? (db-item-status db-item) 'C))

(define (read-all db-data)
  db-data)

(define (read-until-last-charge db-data)
  (reverse
   (let loop ((items (reverse db-data)))
     (if (null? items)
         '()
         (let ((item (car items)))
           (if (db-item-charging? item)
               (list item)
               (cons item (loop (cdr items)))))))))

(define (read-from-time-to-now db-data time-begin)
  (let loop ((idx 0) (cur-data db-data))
    (if (null? cur-data)
        '()
        (let ((item (car cur-data)))
          (if (< (db-item-time item) time-begin)
              (loop (add1 idx) (cdr cur-data))
              (drop db-data idx))))))

(define (read-week db-data)
  (read-from-time-to-now db-data (- (current-seconds) (* 7 24 60 60))))

(define (read-day db-data)
  (read-from-time-to-now db-data (- (current-seconds) (* 24 60 60))))

(define (read-hours db-data hours)
  (read-from-time-to-now db-data (- (current-seconds) (* hours 60 60))))

(define (format-time seconds)
  (time->string (seconds->local-time seconds) "%Y-%m-%d_%H:%M"))

(define gnuplot-template #<#EOF
set ylabel '{/:Bold Capacity (%)}'
set xdata time
set timefmt '%Y-%m-%d_%H:%M'
set format x '%m-%d %H:%M'
set xlabel '{/:Bold Time}' offset 0,-4
set xtics rotate by 55 offset -5,-4
set grid
set bmargin 10
set yrange [0:100]

EOF
)

(define (plot db-files data-filter)
  (let ((data-files '())
        (gplot-file (create-temporary-file "batmon.gnuplot"))
        (gplot-code gnuplot-template))

    (define (cleanup)
      (for-each delete-file* (cons gplot-file data-files)))

    (for-each
     (lambda (db-file)
       (let* ((db-data (with-input-from-file db-file read-list))
              (data (data-filter db-data))
              (data-file (create-temporary-file "batmon.dat")))
         (set! data-files (cons data-file data-files))

         (with-output-to-file data-file
           (lambda ()
             (for-each
              (lambda (item)
                (printf "~a\t~a\n"
                        (db-item-capacity item)
                        (format-time (db-item-time item))))
              data)))

         (set! gplot-code
               (string-append
                gplot-code
                (sprintf "plot ~a using 2:1 with linespoints title '~a'\n"
                         (qs data-file)
                         (pathname-file db-file))))))
     db-files)

    (with-output-to-file gplot-file (cut print gplot-code))

    (handle-exceptions exn
      (cleanup)
      (system* (sprintf "gnuplot -persist ~a" (qs gplot-file))))
    (cleanup)))

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

--range|-r <range>
  Possible <range> values:
    * all          Entire DB data
    * last-cycle   Data since last charge
    * day          Data in the last 24h
    * week         Data in the last week
    * -<hours>     Data in the last <hours> hours
EOF
))
    (fprintf port msg)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))

  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))

  (let ((data-dir
         (make-pathname (list (get-environment-variable "HOME") ".cache")
                        "batmon-data"))
        (valid-ranges `((all . ,read-all)
                        (last-cycle . ,read-until-last-charge)
                        (day . ,read-day)
                        (week . ,read-week)
                        ))
        (range read-all))

    (let loop ((args (command-line-arguments)))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((or (string=? arg "--data-dir") (string=? arg "-d"))
                 (when (null? (cdr args))
                   (die! "--data-dir requires an argument"))
                 (set! data-dir (cadr args))
                 (loop (cddr args)))
                ((or (string=? arg "--range") (string=? arg "-r"))
                 (when (null? (cdr args))
                   (die! "--range requires an argument"))
                 (let ((val (cadr args)))
                   (if (string-prefix? "-" val)
                       (let ((hours (string->number (substring val 1))))
                         (unless (and (integer? hours) (positive? hours))
                           (die! "~a: invalid range specification"))
                         (set! range (lambda (db-data) (read-hours db-data hours))))
                       (let ((range-id (string->symbol val)))
                         (unless (memq range-id (map car valid-ranges))
                           (die! "~a: invalid range specification." range))
                         (set! range (alist-ref range-id valid-ranges)))))
                 (loop (cddr args)))
                (else
                 (die! "Invalid parameter: ~a" arg))))))

    (plot (glob (make-pathname data-dir "*.db")) range)
    ))
) ;; end module
