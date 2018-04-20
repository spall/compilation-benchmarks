#lang racket/base

(require racket/match
         racket/string
         racket/list
         "makegraph.rkt"
         (only-in"makegraphfunctions.rkt"
                 collapse-targets)
         "flags.rkt")

(provide parse-rusage)

(define (ends-semicolon? str)
  (string-suffix? str ";"))

(define (read-full-line fip)
  (let loop ()
    (define line (read-line fip))
    (cond
      [(or (equal? "" line)
           (eof-object? line))
       line]
      [(string-suffix? (car (reverse (string-split line))) "\\")
       (string-append (string-trim line "\\" #:left? #f)
                      " "
                      (loop))]
      [else
       line])))

(define (clean-target-name str)
  (string-trim (string-trim (string-trim (string-trim
                                          (string-trim str "." #:left? #f)
                                          ";" #:left? #f)
                                         "'" #:left? #f) "`" #:right? #f)))

(define (clean-dir-name str)
  (string-trim (string-trim str "'" #:left? #f) "`" #:right? #f))

(define (check-current-target tname t caller)
  (unless (equal? tname (target-name t))
    (error 'check-current-target "~a: Expected target ~a got target ~a" caller (target-name t) tname)))

(define (submake-cmd? cmd)
  (string-contains? cmd "make"))

(define EXPECTED-NUM 13)

;; return #f or rusage-data structure
(define (rusage-info? line cmd)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (cond
    [(equal? (car tmp) "rc")
     (unless (= EXPECTED-NUM (length words))
       (error 'parse-times-line "Expected ~a pieces of information; have ~a\n" EXPECTED-NUM line))
     (define rds (create-rusage-data cmd))
     (let parse ([info words])
       (unless (empty? info)
         (let* ([next (string-split (car info) "=" #:trim? #f)]
                [info_ (car next)]
                [val (car (cdr next))])
           (match info_
             ["rc" (set-rusage-data-rc! rds val)]
             ["elapsed" (set-rusage-data-elapsed! rds (string->number val))]
             ["user" (set-rusage-data-user! rds (string->number val))]
             ["system" (set-rusage-data-system! rds (string->number val))]
             ["maxrss" (set-rusage-data-maxrss! rds val)]
             ["avgrss" (set-rusage-data-avgrss! rds val)]
             ["ins" (set-rusage-data-ins! rds val)]
             ["outs" (set-rusage-data-outs! rds val)]
             ["minflt" (set-rusage-data-minflt! rds val)]
             ["majflt" (set-rusage-data-majflt! rds val)]
             ["swaps" (set-rusage-data-swaps! rds val)]
             ["avgmem" (set-rusage-data-avgmem! rds val)]
             ["avgdata" (set-rusage-data-avgdata! rds val)]
             [else
              (printf "Do not recognize ~a so ignoring\n" info)])
           (parse (cdr info)))))
     (unless (all-fields-set? rds)
       (error 'parse-times-line "Failed to populate entire structure: ~a" rds))
     rds]
    [else
     #f]))

(define (parse-file fip)
  (define mgraph (create-makegraph))
  (define root-id (mcons "<ROOT>" "top"))
  (define root (create-target "<ROOT>"))
  (set-makegraph-root! mgraph root)

  (define (parse-line line ts prqs? ttimes dirs)
    (define tid (caar ts)) ;; mutable pair <name,makefile>
    ;; 2nd thing is makefile; which may be #f
    (define t (cdar ts)) ;; (car ts) is a pair of <tid,target>

    (define (process-finished-target)
      ;; mfile is going to be top of dirs.....
      (when (list? (car dirs))
        (error 'process-finished-target "Car of dirs is ~a is a list" dirs))
      
      (set-mcdr! tid (car dirs)) ;; just a dir
      (set-target-mfile! t (car dirs))
      
      (when (empty? (cdr ts))
        (error 'parse-line "Expected at least one more target to be in stack"))
      (when (empty? (cdr prqs?))
        (error 'parse-line "Expected at least one more prq? to be in stack"))
      
      (define parent (cdadr ts))
      
      (if (cadr prqs?)
          (add-dependency parent tid (car ttimes))
          (add-recipe parent tid (car ttimes)))
      
      (cond
        [(target-in-graph? mgraph tid)
         (when DEBUG
           (printf "Target is already in graph for ~a; going to consolidate them.\n" tid))
         (define tmp (get-target mgraph tid))

         ;; I think we want to combine edges; because processing a target is really
         ;; us processing a build path... and if we "reprocess" a target, we've just
         ;; processed another build path and we want to record that path in the graph
         ;; with new edges.
         (set-target-deps! tmp (append (target-deps t) (target-deps tmp)))
         ;; TODO: should the above guarantee edges are sorted?
         ;; Will edges of t always have lower numbers than edges already in tmp?
         
         ;; TODO: same question here.
         (set-target-recipes! tmp (append (target-recipes t) (target-recipes tmp)))]
        [else
         ;; add target to graph
         (when DEBUG
           (printf "Finished processing ~a; adding to graph.\n" tid))
         (add-target-to-makegraph mgraph tid t)]))
    
    (when (eof-object? line)
      (unless (equal? root t)
        (error 'parse-file "Unexpected end of line before end of target ~a" (target-name t))))

    (unless (eof-object? line)
      (match (string-split line)
        [`("Toplevel" "make" "directory" ,dir)
         (when dirs
           (error 'parse-line "Expected dirs to be #f; dirs is ~a" dirs))
         (read-file ts prqs? ttimes (list dir))]
        [`("File" ,target "was" "considered" "already." . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Was considered already")
         (process-finished-target)
         (read-file (cdr ts) (if (car prqs?)
                                 (cons #f (cdr prqs?))
                                 prqs?) (cdr ttimes) dirs)]
        [`("No" "need" "to" "remake" "target" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "No need to remake")
         
         (process-finished-target)
         
         (read-file (cdr ts) (cdr prqs?) (cdr ttimes) dirs)]
        [`("Successfully" "remade" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Successfully remade")
         
         (process-finished-target)
         
         (read-file (cdr ts) (cdr prqs?) (cdr ttimes) dirs)] ;; reset ttimes?
        [`("Pruning" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         ;; make has decided this target doesnt need to be considered. so
         ;; add it with a time of 0
         
         (define ntarget-id (mcons tname (car dirs)))
         
         ;; check if target already exists in graph
         (unless (target-in-graph? mgraph ntarget-id) ;; create target and add to graph
           (when DEBUG
             (printf "Pruning target ~a; but does not already exist in graph; adding.\n" ntarget-id))
           (define tmp (create-target tname))
           (set-target-mfile! tmp (mcdr ntarget-id))
           (add-target-to-makegraph mgraph ntarget-id tmp))
         
         (if (car prqs?)
             (add-dependency t ntarget-id 0)
             (add-recipe t ntarget-id 0))
         
         (read-file ts prqs? ttimes dirs)]
        [`("Must" "remake" "target" ,target . ,rest) ;; not sure if we care about this line...
         (define tname (clean-target-name target))
         (check-current-target tname t "must remake target")
         
         (read-file ts prqs? ttimes dirs)]
        [`("Invoking" "recipe" "from" ,mkfile "to" "update" "target" ,target . ,rest) ;; not sure what this line indicates......
         (define tname (clean-target-name target))
         (check-current-target tname t "invoking recipe")
         
         (read-file ts prqs? ttimes dirs)]
        [`("CURDIR:" ,dir . ,rest) ;; rest should be empty
         ;; TODO: says what dir shell command executed in.....
         ;; this won't work if recipe doesnt execute any shell commands.
         ;; for example; recipe that only has prerequisites and doesnt do anything else....
         
         ;; How do we deal with that case?
         ;; NOT SURE YET
         ;; TODO: DO WE EVEN NEED THIS ANYMORE?
         
         (read-file ts prqs? ttimes dirs)]
        [`("executing" "sub-make:" , cmd ... ";" "in" "directory" . ,rest)
         ;; parse cmd looking for -C dir
         (define cdir (car rest))
         (when DEBUG
           (printf "current directory is ~a\n" cdir))
         
         (match cmd
           [`("-C" ,dir . ,rcmd)
            ;; change directory here
            (define ndir (string-append cdir "/" dir))
            (when DEBUG
              (printf "new directory after recursive make is ~a\n" ndir))
            (read-file ts prqs? ttimes (cons ndir dirs))]
           [else ;; also change directory here because might have done a cd dir ;; make ...
            (read-file ts prqs? ttimes (cons cdir dirs))])]
        
        [`("finishing" "sub-make:" , cmd ... ";" "in" "directory" . ,rest)
         (define cdir (car rest))
         (when DEBUG
           (printf "current directory is ~a\n" cdir))
         
         (match cmd
           [`("-C" ,dir . ,rcmd) ;; change directory here
            (define ndir (string-append cdir "/" dir))
            (unless (equal? (car dirs) ndir)
              (error 'parse-line "Expected to be leaving directory ~a; but leaving ~a instead." (car dirs) ndir))
            (read-file ts prqs? ttimes (cdr dirs))]
           [else ;; also change directory here...
            (unless (equal? (car dirs) cdir)
              (error 'parse-line "Expected to be leaving directory ~a; but leaving ~a instead." (car dirs) cdir))
            (read-file ts prqs? ttimes (cdr dirs))])]
        
        [`("Finished" "prerequisites" "of" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "finished prereqs")
         (read-file ts (cons #f (cdr prqs?)) ttimes dirs)]
        [`("Considering" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         #| considering a new target.  MIGHT need to create a new 
         target structure.
         |#
         (define ntarget-id (mcons tname #f)) ;; haven't determined makefile yet.
         (define ntarget (create-target tname))
         
         (read-file (cons (cons ntarget-id ntarget) ts)
                    (cons #t prqs?) (cons '() ttimes) dirs)]
        [`("argv=/bin/bash" "-c" . ,rest)
         (define cmd (string-join rest " "))
         (define nline (read-full-line fip))
         #| executed part of a target recipe.
         If the cmd (rest) is a recursive make call; the time for that is part of edge
         leaving this target.
         
         if the cmd is NOT a recursive make call; then the time is part of this target
         exclusively and we want to3 return this time to "outer" target    
         |#
         (cond
           [(rusage-info? nline cmd) =>
            (lambda (info)
              (read-file ts prqs? (if (submake-cmd? cmd)
                                      ttimes
                                      (cons (cons info (car ttimes))
                                            (cdr ttimes)))
                         dirs))]
           [else
            (when DEBUG 
              (printf "Expected times line to follow argv line; got ~a instead\n" nline))
            (error 'parse-file "Got ~a following ~a instead of rusageinfo" nline line)
            (read-file ts prqs? ttimes dirs)])]
        [else
         (read-file ts prqs? ttimes dirs)])))
    
  (define (read-file ts prqs? ttimes dirs)
    (define line (read-full-line fip))
    (parse-line line ts prqs? ttimes dirs))
  
  (read-file (list (cons root-id root)) (list #f) (list '()) #f)
  (add-target-to-makegraph mgraph root-id root)
  mgraph)
      
(define (parse-rusage file-path [debug? #f])
  (define file (open-input-file file-path #:mode 'text))
  (define result (parse-file file))
  (set-DEBUG! debug?)
  ;; remove duplicate targets
  (collapse-targets result)
  (close-input-port file)
  result)
