#lang racket/base

(require racket/match
         racket/string
         racket/list
         "makegraph.rkt"
         (only-in"makegraphfunctions.rkt"
                 collapse-targets
		 get-last-edge)
         "flags.rkt")

(provide parse-rusage)

(define SHELLCOUNT 0)
(struct shcall (cmd n num-submakes duplicate?) #:mutable #:transparent)
(define (create-shcall cmd n)
  (shcall cmd n 0 #f))

(struct submake (cmd n count depth) #:mutable #:transparent)
(define (create-submake cmd n)
  (submake cmd n 0 0))

(define (remove-extra-quotes str)
  (define ls (string->list str))
  (let loop ([ls_ ls])
    (let ([nls (remove #\" ls_)])
      (cond
        [(equal? ls_ nls)
         (list->string ls_)]
        [else
         (loop nls)]))))

(define (ends-semicolon? str)
  (string-suffix? str ";"))

(define (get-last-edges t n_)
  (define (driver edges n)
    (cond
      [(= 0 n)
       '()]
      [(empty? edges)
       (error 'get-last-edges "No more edges; still needed ~a more" n)]
      [else
       (cons (car edges) (driver (cdr edges) (- n 1)))]))
       
  (driver (target-out-edges t)  n_))	

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

(define (elapsed? line cmd)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  (cond
    [(equal? (car words) "elapsed=")
     (define rds (create-rusage-data cmd))
     (set-rusage-data-elapsed! rds (string->number (cadr words)))
     rds]
    [else
     #f]))

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

(struct state (ts prqs? ttimes dirs submakes shcalls overheads))

(define (parse-file fip)
  (define mgraph (create-makegraph))
  (define root-id (mcons "<ROOT>" "top"))
  (define root (create-target "<ROOT>"))
  (set-makegraph-root! mgraph root)

  (define (parse-line line st)
    (define ts-local (state-ts st))
    (define prqs?-local (state-prqs? st))
    (define ttimes-local (state-ttimes st))
    (define dirs-local (state-dirs st))
    (define submakes-local (state-submakes st))
    (define shcalls-local (state-shcalls st))
    (define overheads-local (state-overheads st))
    (define tid (caar ts-local)) ;; mutable pair <name,makefile>
    ;; 2nd thing is makefile; which may be #f
    (define t (cdar ts-local)) ;; (car ts) is a pair of <tid,target>

    (define (process-finished-target)
      ;; mfile is going to be top of dirs.....
      (when (list? (car dirs-local))
        (error 'process-finished-target "Car of dirs is ~a is a list" dirs-local))
      
      (set-mcdr! tid (car dirs-local)) ;; just a dir
      (set-target-mfile! t (car dirs-local))

      (unless (empty? submakes-local)
        (set-submake-depth! (car submakes-local) (- (submake-depth (car submakes-local)) 1))
        (when (= 0 (submake-depth (car submakes-local)))
          (set-submake-count! (car submakes-local) (+ (submake-count (car submakes-local)) 1))))
      
      (when (empty? (cdr ts-local))
        (error 'parse-line "Expected at least one more target to be in stack"))
      (when (empty? (cdr prqs?-local))
        (error 'parse-line "Expected at least one more prq? to be in stack"))

      (define parent (cdadr ts-local))

      (if (cadr prqs?-local)
          (add-dependency parent tid t (car ttimes-local))
          (add-recipe parent tid t (car ttimes-local)))
      
      (cond
        [(target-in-graph? mgraph tid)
         (when DEBUG
           (printf "Target is already in graph for ~a; going to consolidate them.\n" tid))
         (define tmp (get-target mgraph tid))

         ;; I think we want to combine edges; because processing a target is really
         ;; us processing a build path... and if we "reprocess" a target, we've just
         ;; processed another build path and we want to record that path in the graph
         ;; with new edges.
         (set-target-out-edges! tmp (append (target-out-edges t) (target-out-edges tmp)))
         
	 (set-target-in-edges! tmp (append (target-in-edges t) (target-in-edges tmp)))]
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
        [`("executing" "top-make:" ,cmd ... ";" "in" "directory" . ,rest)
         (define tname (symbol->string (gensym "TOP")))
         (define ntarget-id (mcons tname (car rest)))
         (define ntarget (create-target tname))
         (set-target-mfile! ntarget (car rest))
         (read-file (struct-copy state st
                                 [ts (cons (cons ntarget-id ntarget) ts-local)]
                                 [prqs? (cons #f prqs?-local)]
                                 [ttimes (cons '() ttimes-local)]
                                 [dirs (list (car rest))]))]
	[`("Start" "of" "make" "overhead:" ,ts_ . ,rest)
	 (define start (string->number ts_))
         (read-file (struct-copy state st
                                 [overheads (cons (list start) overheads-local)]))]
	[`("End" "of" "make" "overhead:" ,ts_ . ,rest)
	
	 (define end (string->number ts_))
         (read-file (struct-copy state st
                                 [overheads (cons (cons end (car overheads-local)) (cdr overheads-local))]))]
        [`("finishing" "top-make:" ,cmd ... ";" "in" "directory" . ,rest)
	 ;; TODO: add overhead time to this somehow......
	 (define stmp (cadr (car overheads-local)))	
	 (define etmp (car (car overheads-local)))

         (define parent (cdadr ts-local)) ;; should be <ROOT>
         (add-recipe parent tid t (cons (- etmp stmp) (car ttimes-local)))

         (when (target-in-graph? mgraph tid)
           (error 'parse-file "Target <~a,~a> already in graph" (target-name t) (target-mfile t)))

         (add-target-to-makegraph mgraph tid t)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]
                                 [ttimes (cdr ttimes-local)]
                                 [dirs (cdr dirs-local)]
                                 [overheads (cdr overheads-local)]))]
        [`("File" ,target "was" "considered" "already." . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Was considered already")
         (process-finished-target)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (if (car prqs?-local)
                                            (cons #f (cdr prqs?-local))
                                            prqs?-local)]
                                 [ttimes (cdr ttimes-local)]))]
        [`("No" "need" "to" "remake" "target" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "No need to remake")
         
         (process-finished-target)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]
                                 [ttimes (cdr ttimes-local)]))]
        [`("Successfully" "remade" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Successfully remade")
         
         (process-finished-target)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]
                                 [ttimes (cdr ttimes-local)]))]
        [`("Pruning" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         ;; make has decided this target doesnt need to be considered. so
         ;; add it with a time of 0
         
         (define ntarget-id (mcons tname (car dirs-local)))
         
         ;; check if target already exists in graph
         (unless (target-in-graph? mgraph ntarget-id) ;; create target and add to graph
           (when DEBUG
             (printf "Pruning target ~a; but does not already exist in graph; adding.\n" ntarget-id))
           (define tmp (create-target tname))
           (set-target-mfile! tmp (mcdr ntarget-id))
           (add-target-to-makegraph mgraph ntarget-id tmp))
         
         (if (car prqs?-local)
             (add-dependency t ntarget-id (get-target mgraph ntarget-id) (list 0))
             (add-recipe t ntarget-id (get-target mgraph ntarget-id) (list 0)))

         (read-file st)]
        [`("Must" "remake" "target" ,target . ,rest) ;; not sure if we care about this line...
         (define tname (clean-target-name target))
         (check-current-target tname t "must remake target")
         
         (read-file st)]
        [`("Invoking" "recipe" "from" ,mkfile "to" "update" "target" ,target . ,rest) ;; not sure what this line indicates......
         (define tname (clean-target-name target))
         (check-current-target tname t "invoking recipe")
         
         (read-file st)]
        [`("executing" "shell-command:" ,n_ . ,cmd)
         (when (equal? (target-name t) "<ROOT>")
           (printf "Not considering a target and running cmd ~a\n" cmd))
         (define n (string->number n_))
         (define nshcall (create-shcall (string-join cmd " ") n))
         (define nextline (read-full-line fip))
         ;; if next line is an executing sub-make then this is a duplicate.
         (match (string-split nextline)
           [`("executing" "sub-make:" ,n_2 ":" ,cmd ... ";" "in" "directory" . ,rest)
            ;; parse cmd looking for -C dir
            (define n2 (string->number n_2))
            (unless (= n n2)
              (error 'parse-line "Top of shell call stack is shell call ~a; expected it to be shell call ~a\n" n n2))
            
            (define cdir (car rest))
            (define nsubmake (create-submake (string-join cmd " ") n2))2
            (when DEBUG
              (printf "current directory is ~a\n" cdir))
            
            (define ndir (match cmd
                           [`("-C" ,dir . ,rcmd)
                            (define ndir (string-append cdir "/" dir))
                            (when DEBUG
                              (printf "new directory after recursive make is ~a\n" ndir))
                            ndir]
                           [else 
                            cdir]))
         
            ;; increment # of submakes launches by this shell call
            (set-shcall-num-submakes! nshcall (+ 1 (shcall-num-submakes nshcall)))
            (set-shcall-duplicate?! nshcall #t)

            (read-file (struct-copy state st
                                    [dirs (cons ndir dirs-local)]
                                    [submakes (cons nsubmake submakes-local)]
                                    [shcalls (cons nshcall shcalls-local)]))]
           [else
            (parse-line nextline (struct-copy state st
                                              [shcalls (cons nshcall shcalls-local)]))])]
        
        [`("executing" "sub-make:" ,n_ ":" ,cmd ... ";" "in" "directory" . ,rest)
         ;; parse cmd looking for -C dir
         (define n (string->number n_))
         (define cdir (car rest))
         (define nsubmake (create-submake (string-join cmd " ") n))
         (when DEBUG
           (printf "current directory is ~a\n" cdir))
         
         (define ndir (match cmd
                        [`("-C" ,dir . ,rcmd)
                         (define ndir (string-append cdir "/" dir))
                         (when DEBUG
                           (printf "new directory after recursive make is ~a\n" ndir))
                         ndir]
                        [else 
                         cdir]))

         ;; check if n matches shell call on top of shcall stack
         (when (empty? shcalls-local)
           (error 'parse-line "Shell call stack is empty"))

         (unless (= (shcall-n (car shcalls-local)) n)
           (error 'parse-line "Top of shell call stack is shell call ~a; expected it to be shell call ~a\n" (shcall-n (car shcalls-local)) n))

         ;; increment # of submakes launches by this shell call
         (set-shcall-num-submakes! (car shcalls-local) (+ 1 (shcall-num-submakes (car shcalls-local))))

         (read-file (struct-copy state st
                                 [dirs (cons ndir dirs-local)]
                                 [submakes (cons nsubmake submakes-local)]))]
        [`("Finished" "prerequisites" "of" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "finished prereqs")
         (read-file (struct-copy state st
                                 [prqs? (cons #f (cdr prqs?-local))]))]
        [`("Considering" "target" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         #| considering a new target.  MIGHT need to create a new 
         target structure.
         |#
         (define ntarget-id (mcons tname #f)) ;; haven't determined makefile yet.
         (define ntarget (create-target tname))

         ;; increase submake depth by 1
         (unless (empty? submakes-local)
           (set-submake-depth! (car submakes-local) (+ 1 (submake-depth (car submakes-local)))))

         (read-file (struct-copy state st
                                 [ts (cons (cons ntarget-id ntarget) ts-local)]
                                 [prqs? (cons #t prqs?-local)]
                                 [ttimes (cons '() ttimes-local)]))]
        [`("argv=" . ,rest)
         (define cmd (string-join rest " "))
         (define timesline (read-full-line fip))
         
         (cond
           [(elapsed? timesline cmd) =>
            (lambda (info)
              ;; read next line. should be a finished shell-command line
              (define finishline (let loop ([ln (read-full-line fip)])
                                   (if (equal? "" ln)
                                       (loop (read-full-line fip))
                                       ln)))
              (match (string-split finishline)
                [`("finished" "shell-command:" ,n_ . ,rest)
                 (define n (string->number n_))
                 ;; match n to shell command currently on top of stack.
                 (when (empty? shcalls-local)
                   (error 'parse-line "shcalls is empty"))
                 (unless (= n (shcall-n (car shcalls-local)))
                   (error 'parse-line "Expected shell call with n of ~a to be top of stack, not ~a" n (shcall-n (car shcalls-local))))
                 ;; if this is a submake ...........
                 (cond
                   [(shcall-duplicate? (car shcalls-local)) ;; ignore it
                    (when DEBUG
                      (printf "ignoring cmd ~a\n" cmd))
                    (read-file (struct-copy state st
                                            [shcalls (cdr shcalls-local)]))]
                   [(> (shcall-num-submakes (car shcalls-local)) 0) ;; launched at least 1 submake but
                    ;; "not a duplicate"
                    ;; still need to do some sort of math.....
                    ;; can we just get last n nodes and do same thing as we did for submake...
                    ;; TODO what do we do in this case?
                    ;; possible cases
                    ;; for i ...... make i
                    ;; - just have a make for each call....
                    ;; not clear this will ever happen, but
                    ;(printf "Processing shell command with submakes that is not a duplicate ~a; ~a\n" n_ (string-join rest " "))

                    (define last-edges (get-last-edges t (shcall-num-submakes (car shcalls-local))))
                    ;; add edges to fake target.
                    ;; remove edges from current target.
                    ;; create  new fake node.
                    (define tmpname (symbol->string (gensym "FAKE")))
                    (define tmp-FAKE-ID (mcons tmpname "top"))
                    (define tmp-FAKE (create-target tmpname))
                    (set-target-mfile! tmp-FAKE "top")
                    ;; add to graph
                    (add-target-to-makegraph mgraph tmp-FAKE-ID tmp-FAKE)
                    
                    (for ([last-edge last-edges])
		      (remove-edge t last-edge 'out)
                      (add-edge tmp-FAKE last-edge 'out 'dep))
                    
                    ;; add edge from current target to fake target.
                    ;; should be a recipe
                    (set-rusage-data-submake?! info #t)
                    (add-recipe t tmp-FAKE-ID (list info))

                    (read-file (struct-copy state st
                                            [shcalls (cdr shcalls-local)]))]
                   [else ;; if this is not a submake......
                    (set! SHELLCOUNT (+ 1 SHELLCOUNT))

                    (read-file (struct-copy state st
                                            [ttimes (cons (cons info (car ttimes-local))
                                                          (cdr ttimes-local))]
                                            [shcalls (cdr shcalls-local)]))])]
                [else
                 (error 'parse-line "Got ~a instead of 'finished shell-command:' following ~a" finishline line)]))]
           [else
            (error 'parse-line "Got ~a following ~a instead of rusageinfo" timesline line)])]

        ;; produced by submake script; may be a duplicate of another argv line produced by rusage script
        ;; may not be, depending on how submake was launched
        ;; will occur first if it is a duplicate.
        [`("submake-argv=" . ,rest)
         (define argv-cmd (string-join rest " "))
         (define timesline (read-full-line fip))
         (define finishline (let loop ([ln (read-full-line fip)])
                              (if (equal? "" ln)
                                  (loop (read-full-line fip))
                                  ln)))
         
         (cond
           [(elapsed? timesline argv-cmd) =>
            (lambda (info)
              (match (string-split finishline)
                [`("finishing" "sub-make:" ,n_ ":" ,cmd ... ";" "in" "directory" . ,rest)
                 ;; TODO: add overhead time to this somehow...
		 (define stmp (cadr (car overheads-local)))	
	 	 (define etmp (car (car overheads-local)))		

		 (define n (string->number n_))
                 ;; should be at top of submake stack so check that n's match
                 (when (empty? submakes-local)
                   (error 'parse-line "Submakes is empty"))
                 (unless (= n (submake-n (car submakes-local)))
                   (error 'parse-line "Expected submake with n of ~a to be on top of stack, not ~a" n (submake-n (car submakes-local))))
                 (unless (= 0 (submake-depth (car submakes-local)))
                   (error 'parse-line "~a depth is ~a not zero" argv-cmd (submake-depth (car submakes-local))))
                 (define cdir (car rest))
                 (when DEBUG
                   (printf "current directory is ~a\n" cdir))
                 ;; error checking.
                 (define ldir (match cmd
                                [`("-C" ,dir . ,rcmd)
                                 (string-append cdir "/" dir)]
                                [else
                                 cdir]))
                 (unless (equal? (car dirs-local) ldir)
                   (error 'parse-line "Expected to be leaving directory ~a; but leaving ~a instead." (car dirs-local) cdir))

                 (define count (submake-count (car submakes-local)))
                 (cond
                   [(> count 1) ;; use FAKE target.
                    (define last-edges (get-last-edges t count))
                    ;; add edges to fake target.
                    ;; remove edges from current target.
                    (define tmpname (symbol->string (gensym "FAKE")))
                    (define tmp-FAKE-ID (mcons tmpname "top"))
                    (define tmp-FAKE (create-target tmpname))
                    (set-target-mfile! tmp-FAKE "top")
                    ;; add to graph
                    (add-target-to-makegraph mgraph tmp-FAKE-ID tmp-FAKE)
                    
                    (for ([last-edge last-edges])
		      (remove-edge t last-edge 'out)
                      (add-edge tmp-FAKE last-edge 'out 'dep))
                    
                    ;; add edge from current target to fake target.
                    ;; should be a recipe
                    (set-rusage-data-submake?! info #t)
                    (add-recipe t tmp-FAKE-ID tmp-FAKE (list (- etmp stmp) info))]
                   [else
                    ;; add info to an edge
                    (define last-edge (get-last-edge t))
                    (set-rusage-data-submake?! info #t)
                    (set-edge-data! last-edge (cons (- etmp stmp) (cons info (edge-data last-edge))))])]
                [else
                 (error 'parse-line "Expected finishing submake line, got ~a instead" finishline)])

              (read-file (struct-copy state st
                                      [dirs (cdr dirs-local)]
                                      [submakes (cdr submakes-local)]
                                      [overheads (cdr overheads-local)])))]
           [else
            (error 'parse-line
                   "Expected times line to follow argv line; got ~a instead\n" timesline)])]
        [else
         (when DEBUG
           (printf "Didn't match ~a\n" line))
         (read-file st)])))
  
  (define (read-file st)
    (define line (read-full-line fip))
    (parse-line line st))
  
  (read-file (state (list (cons root-id root)) (list #f) (list '()) #f '() '() '()))
  (add-target-to-makegraph mgraph root-id root)
  mgraph)
      
(define (parse-rusage file-path [debug? #f])
  (define file (open-input-file file-path #:mode 'text))
  (define result (parse-file file))
  (set-DEBUG! debug?)
  (close-input-port file)
  result)
