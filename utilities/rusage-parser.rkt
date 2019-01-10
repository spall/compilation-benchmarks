#lang racket/base

(require racket/match
         racket/string
         racket/list
         "makegraph.rkt"
	 "parsers.rkt"
         "flags.rkt")

(provide parse-rusage)

#|  Currently there is  tid <--> target object

    Want to move to   tid --> {to_1, to_2, .... , to_n} where n is the number of times the recipes of the target are run?  
    Yes because the object isn't changed if the recipes aren't run. even if some dependency is run again for some reason.

    Currently everytime a target is encountered; we try to create a new target and add it to graph
    If that target already exists in the graph then shcall targets are potentially merged.

    What I think we want to do is:
      If the target is encountered and has not been rebuilt then we do not create a new target
      and we just refer to the last target structure created for that tid.  

      If the target is encountered and has been rebuilt then we want to create a new target
      structure that points to the correct version of the dependencies
      
    I think there are two ways to implement this:
      1.  add an extra field to edge struct which says which version of the target data structure
          the tid refers 2
          In this case a tid would point to a list and then the extra field in the edge would 
          say where to go in the list to find the correct structure. 

      2.  add an extra field to the tid structure which says which version of the target data
          structure the tid referse to.  
          This option would move us back to tid <--> target object; which might make updating 
          the system simpler.  

          Would then need to keep track of which number tid we were on.....; Could have another
          hash table that went from original tid structure to the last id used.
          
          I think 2 seems better barring some unforseen implementation difficulty
|#


(define SHELLCOUNT 0)
(struct shcall (cmd n dir num-submakes duplicate?) #:mutable #:transparent)
(define (create-shcall cmd n dir)
  (shcall cmd n dir 0 #f))

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

(struct state (ts prqs? dirs submakes shcalls overheads mgraph))

(define (parse-file fip)
  (define bgraph (create-buildgraph))
  (define tid-version-counter (make-hash))

  (define (next-version! p)
    (define v (+ 1 (hash-ref tid-version-counter p -1)))
    (hash-set! tid-version-counter p v)
    v)

  (define (current-version p)
    (define v (hash-ref tid-version-counter p 0))
    (when (= 0 v)
	  (hash-set! tid-version-counter p 0))
    v)

  (define really-submake? #f)

  (define (parse-line line st)
    (define mgraph (state-mgraph st))
    (define ts-local (state-ts st))
    (define prqs?-local (state-prqs? st))
    (define dirs-local (state-dirs st))
    (define submakes-local (state-submakes st))
    (define shcalls-local (state-shcalls st))
    (define overheads-local (state-overheads st))
    (define tid (caar ts-local))
    (define t (cdar ts-local)) 

    (define (process-finished-target [remade? #f] [phony? #f])
      ;; mfile is going to be top of dirs.....
      (when (list? (car dirs-local))
        (error 'process-finished-target "Car of dirs is ~a is a list" dirs-local))
      
      (set-targetid-mfile! tid (car dirs-local)) ;; TODO car dirs-local needs to be a dir/makefile path
      (set-target-mfile! t (car dirs-local))
      (set-target-data! t (list 0))      
      (set-target-phony?! t phony?)

      (unless (empty? submakes-local)
        (set-submake-depth! (car submakes-local) (- (submake-depth (car submakes-local)) 1))
        (when (= 0 (submake-depth (car submakes-local)))
          (set-submake-count! (car submakes-local) (+ (submake-count (car submakes-local)) 1))))

      (when (empty? (cdr ts-local))
        (error 'parse-line "Expected at least one more target to be in stack"))
      (when (empty? (cdr prqs?-local))
        (error 'parse-line "Expected at least one more prq? to be in stack"))

      (define parent (cdr (cadr ts-local))) 

      (define already-in-graph? (target-in-graph? mgraph tid))
      (add-target-to-makegraph mgraph tid t)
      (when already-in-graph?
        (define tmp (get-target mgraph tid))
	(match (target-type tmp)
	  ['unknown 
	   (if remade?
	       (set-target-type! tmp 'name)
	       (set-target-type! tmp 'file))]
	  ['name
	   (unless remade?
	     (set-target-type! tmp 'file))]
	  ['file
	   (void)]))
      
      ;; use remade when adding target to graph to increment how many times it was run.

      (if (cadr prqs?-local)
          (add-dependency parent tid)
          (add-recipe parent tid)))
    
    (when (eof-object? line)
      (unless (equal? #f t)
        (error 'parse-file "Unexpected end of line before end of target ~a" (target-name t))))

    (unless (eof-object? line)
      (match (string-split line)
        [`("executing" "top-make:" ,cmd ... ";" "in" "directory" . ,rest)
	 (cond
	  [(equal? #f mgraph)
	   ;; a new top level invocation of make so create a new makegraph
	   (define new-mgraph (create-makegraph))
	   (printf "Created a new mgraph ~a\n" line)
           (define tname (symbol->string (gensym "TOP")))
	   #|  Is it correct to have a new targetid + target each time we execute the same top-level make?
	   |#
	   
           (define ntarget-id (create-targetid tname (car rest) 
					       (next-version! (cons tname (car rest)))))
           (define ntarget (create-target tname))

           ;; need to parse cmd for -f / --file
	   (define makefiles (topmake-makefiles (string-join cmd " ")))
	   (if (empty? makefiles)
	       (set-target-mfile! ntarget (car rest)) ;; just directory
	       (set-target-mfile! ntarget (path->complete-path (last makefiles) (car rest))))

           (read-file (struct-copy state st
                                   [ts (cons (cons ntarget-id ntarget) ts-local)]
                                   [prqs? (cons #f prqs?-local)]
                                   [dirs (list (car rest))]
				   [mgraph new-mgraph]))]
          [else ;; really is a submake

	   (when (empty? shcalls-local)
	     (printf "line is ~a\n" line)
	     (error 'parse-line "Shell call stack is empty"))

	   (define cdir (car rest))
	   (define nsubmake (create-submake (string-join cmd " ") (shcall-n (car shcalls-local))))
	   (define-values (dirs mfiles) (submake-dirs-makefiles (string-join cmd " ")))
	   (define ndir (let ([tmp (for/fold ([ndir cdir])
	    	                           ([dir dirs])
                                   (path->complete-path dir ndir))])
		        (if (empty? mfiles)
			    tmp
			    (path->complete-path (car mfiles) tmp))))
	   (set-shcall-num-submakes! (car shcalls-local) (+ 1 (shcall-num-submakes (car shcalls-local))))
	   (set! really-submake? #t)
           (read-file (struct-copy state st
                                 [dirs (cons ndir dirs-local)]
                                 [submakes (cons nsubmake submakes-local)]))])]
	[`("Start" "of" "make" "overhead:" ,ts_ . ,rest)
	 (define start (string->number ts_))
         (read-file (struct-copy state st
                                 [overheads (cons (list start) overheads-local)]))]
	[`("End" "of" "make" "overhead:" ,ts_ . ,rest)
 
	 (define end (string->number ts_))
         (read-file (struct-copy state st
                                 [overheads (cons (cons end (car overheads-local)) (cdr overheads-local))]))]
        [`(,pid_ "finishing" "top-make:" ,cmd ... ";" "in" "directory" . ,rest)
	 ;; TODO: add overhead time to this somehow......
	 (define stmp (if (empty? overheads-local)
	 	      	  0
			  (cadr (car overheads-local))))	
	 (define etmp (if (empty? overheads-local)
	 	      	  0
			  (car (car overheads-local))))

         (define parent (cdr (cadr ts-local))) ;; should be <ROOT>

	 (add-target-to-makegraph mgraph tid t)
	 (set-makegraph-root! mgraph tid)

	 (add-makegraph bgraph mgraph)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]
                                 [dirs (cdr dirs-local)]
                                 [overheads (if (empty? overheads-local)
				 	        overheads-local
						(cdr overheads-local))]
				 [mgraph #f]))]
        [`("File" ,target "was" "considered" "already." . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Was considered already")
         
	 (process-finished-target)


         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]))]
        [`("No" "need" "to" "remake" "target" ,target . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "No need to remake")
		         
         (process-finished-target)


         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]))]
        [`(,submake "***" ,_ ,name "Error" . ,errnum)
	 (error 'rusage-parser "Submake error ~a" line)
	 (printf "matched something\n")

	 (define tname (string-append "'" (string-trim name "]" #:left? #f)))
         (check-current-target tname t (format "Error ~a" (car errnum)))

	 (process-finished-target #t)

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]))]
	 
        [`("Successfully" "remade" "target" "file" ,target "PHONY?" ,ph? . ,rest)
         (define tname (clean-target-name target))
         (check-current-target tname t "Successfully remade")
         
         (process-finished-target #t (if (= 0 (string->number ph?))
	 			     	 #f
					 #t))

         (read-file (struct-copy state st
                                 [ts (cdr ts-local)]
                                 [prqs? (cdr prqs?-local)]))]
        [`("Pruning" "file" ,target . ,rest)
         (define tname (clean-target-name target))
         ;; make has decided this target doesnt need to be considered. so
         ;; add it with a time of 0
         
         (define ntarget-id (create-targetid tname (car dirs-local)
					     (current-version (cons tname (car dirs-local)))))

	 (define ntarget (create-target tname))
	 (set-target-mfile! ntarget (car dirs-local))
         ;; add data to target
         (set-target-data! ntarget (list 0))
	 (add-target-to-makegraph mgraph ntarget-id ntarget)

         
         (if (car prqs?-local)
             (add-dependency t ntarget-id)
             (add-recipe t ntarget-id))

         (read-file st)]
        [`("Must" "remake" "target" ,target . ,rest) ;; not sure if we care about this line...
         (define tname (clean-target-name target))
         (check-current-target tname t "must remake target")
         
         (read-file st)]
        [`("executing" "shell-command:" ,n_ ";" ,dir ";" . ,cmd)
         (when (equal? (target-name t) "<ROOT>")
           (printf "Not considering a target and running cmd ~a\n" cmd))
         (define n (string->number n_))
         (define nshcall (create-shcall (string-join cmd " ") n dir))
         (define nextline (read-full-line fip))
         ;; if next MEANINGFUL line is an executing sub-make then this is a duplicate.
         (match (string-split nextline)
           [`("executing" "sub-make:" ,n_2 ":" ,cmd ... ";" "in" "directory" . ,rest)
            ;; parse cmd looking for -C dir
            (define n2 (string->number n_2))
            (unless (= n n2)
              (error 'parse-line "Top of shell call stack is shell call ~a; expected it to be shell call ~a\n" n n2))
            
            (define cdir (car rest))
            (define nsubmake (create-submake (string-join cmd " ") n2))
            (when (debug?)
              (printf "current directory is ~a\n" cdir))
            
	    (define-values (dirs mfiles) (submake-dirs-makefiles (string-join cmd " ")))
	    (define ndir (let ([tmp (for/fold ([ndir cdir])
	    	                              ([dir dirs])
                                      (path->complete-path dir ndir))])
		           (if (empty? mfiles)
			       tmp
			       (path->complete-path (car mfiles) tmp))))
         
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
         (when (debug?)
           (printf "current directory is ~a\n" cdir))
         
         
	 (define-values (dirs mfiles) (submake-dirs-makefiles (string-join cmd " ")))
	 (define ndir (let ([tmp (for/fold ([ndir cdir])
	    	                           ([dir dirs])
                                   (path->complete-path dir ndir))])
		        (if (empty? mfiles)
			    tmp
			    (path->complete-path (car mfiles) tmp))))
         
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
         ;; considering a new target.  MIGHT need to create a new target structure.
	 
         (define ntarget-id (create-targetid tname #f #f)) ;; haven't determined dir/makefile yet
         (define ntarget (create-target tname))

         ;; increase submake depth by 1
         (unless (empty? submakes-local)
           (set-submake-depth! (car submakes-local) (+ 1 (submake-depth (car submakes-local)))))

         (read-file (struct-copy state st
                                 [ts (cons (cons ntarget-id ntarget) ts-local)]
                                 [prqs? (cons #t prqs?-local)]))]
        [`("argv=" . ,rest)
         (define timesline (read-full-line fip))
	 (when (empty? shcalls-local)
                   (error 'parse-line "shcalls is empty"))

	 (define cmd (shcall-cmd (car shcalls-local)))

         (cond
           [(elapsed? timesline cmd) =>
            (lambda (info)
              ;; read next line. should be a finished shell-command line
              (define finishline (let loop ([ln (read-full-line fip)])
                                   (if (equal? "" ln)
                                       (loop (read-full-line fip))
                                       ln)))
              (match (string-split finishline)
                [`(,pid_ "finished" "shell-command:" ,n_ . ,rest)
                 (define n (string->number n_))
                 ;; match n to shell command currently on top of stack.
                 (when (empty? shcalls-local)
                   (error 'parse-line "shcalls is empty"))
                 (unless (= n (shcall-n (car shcalls-local)))
                   (error 'parse-line "Expected shell call with n of ~a to be top of stack, not ~a" n (shcall-n (car shcalls-local))))
                 (define pid (string-trim (string-trim pid_ "[") "]"))
		 
		 (set-rusage-data-id! info n)
		 (set-rusage-data-pid! info pid)

		 (set-rusage-data-dir! info (shcall-dir (car shcalls-local)))
                 (cond
                   [(shcall-duplicate? (car shcalls-local)) ;; if this is a submake ignore it
                    (when (debug?)
                      (printf "ignoring cmd ~a\n" cmd))
                    (read-file (struct-copy state st
                                            [shcalls (cdr shcalls-local)]))]
                   [(> (shcall-num-submakes (car shcalls-local)) 0) ;; launched at least 1 submake
                    (define last-edges (get-last-edges t (shcall-num-submakes (car shcalls-local))))
                    ;; add edges to fake target.
                    ;; remove edges from current target.
                    ;; create  new fake node.
		    (define tmp-name (symbol->string (gensym "FAKE")))
		    (define tmp-FAKEid (create-targetid tmp-name "top" 
							(next-version! (cons tmp-name "top"))))
                    (define tmp-FAKE (create-target tmp-name))
                    (set-target-mfile! tmp-FAKE "top") ;; this is probably wrong
		    
                    ;; add to graph
                    (add-target-to-makegraph mgraph tmp-FAKEid tmp-FAKE)
                    
                    (for ([last-edge last-edges])
		      (remove-edge t last-edge)
                      (add-edge tmp-FAKE last-edge 'dep))
                    
                    ;; add edge from current target to fake target.
                    ;; should be a recipe
                    (set-rusage-data-submake?! info #t)
                    ;(set-target-data! tmp-FAKE (list info)) TODO: what about this case?
                    
                    (add-recipe t tmp-FAKEid)

                    (read-file (struct-copy state st
                                            [shcalls (cdr shcalls-local)]))]
                   [else ;; if this is not a submake......
                    (set! SHELLCOUNT (+ 1 SHELLCOUNT))

		    (define tmp-name (symbol->string (gensym "SHCALL")))
		    (define shcall-targetid (create-targetid tmp-name "top"
							     (next-version! (cons tmp-name "top"))))
                    (define shcall-target (create-target tmp-name))
                    (set-target-mfile! shcall-target "top") ;; this is probably wrong
                    (set-target-data! shcall-target (list info))
		    (set-target-phony?! shcall-target #t)
                    (add-target-to-makegraph mgraph shcall-targetid shcall-target)
		    
                    (add-recipe t shcall-targetid)

                    (read-file (struct-copy state st
                                            [shcalls (cdr shcalls-local)]))])]
                [else
                 (error 'parse-line "Got ~a instead of 'finished shell-command:' following ~a" finishline line)]))]
           [else
            (error 'parse-line "Got ~a following ~a instead of rusageinfo" timesline line)])]


        [`("topmake-argv=" . ,rest)
	 #:when really-submake?
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
                [`(,pid_ "finishing" "top-make:" ,cmd ... ";" "in" "directory" . ,rest)
                 ;; TODO: add overhead time to this somehow...

		 (set! really-submake? #f)
		 (define stmp (if (empty? overheads-local)
		 	      	  0	  
				  (cadr (car overheads-local))))	
	 	 (define etmp (if (empty? overheads-local)
		 	      	  0
				  (car (car overheads-local))))		

		 (when (empty? submakes-local)
                   (error 'parse-line "Submakes is empty"))
                 (unless (= 0 (submake-depth (car submakes-local)))
                   (error 'parse-line "~a depth is ~a not zero" argv-cmd (submake-depth (car submakes-local))))
                 (define pid (string->number (string-trim (string-trim pid_ "[") "]")))
		 (set-rusage-data-id! info (submake-n (car submakes-local)))
                 (set-rusage-data-pid! info pid)
                 (define cdir (car rest))
		 (set-rusage-data-dir! info cdir)
                 (when (debug?)
                   (printf "current directory is ~a\n" cdir))
                 ;; error checking.

                 
	         (define-values (dirs mfiles) (submake-dirs-makefiles (string-join cmd " ")))
	         (define ldir (let ([tmp (for/fold ([ndir cdir])
	    	                                   ([dir dirs])
                                           (path->complete-path dir ndir))])
		                (if (empty? mfiles)
			            tmp
			            (path->complete-path (car mfiles) tmp))))
         
                 (unless (equal? (car dirs-local) ldir)
                   (error 'parse-line "Expected to be leaving directory ~a; but leaving ~a instead." (car dirs-local) cdir))

                 (define count (submake-count (car submakes-local)))
                 (cond
                   [(> count 1) ;; use FAKE target.
                    (define last-edges (get-last-edges t count))
                    ;; add edges to fake target.
                    ;; remove edges from current target.
		    (define tmp-name (symbol->string (gensym "FAKE")))
		    (define tmp-FAKEid (create-targetid tmp-name "top"
							(next-version! (cons tmp-name "top"))))
                    (define tmp-FAKE (create-target tmp-name))
                    (set-target-mfile! tmp-FAKE "top")
		    
                    ;; add to graph
                    (add-target-to-makegraph mgraph tmp-FAKEid tmp-FAKE)
                    
                    (for ([last-edge last-edges])
		      (remove-edge t last-edge)
                      (add-edge tmp-FAKE last-edge 'dep))
                    
                    ;; add edge from current target to fake target.
                    ;; should be a recipe
                    (set-rusage-data-submake?! info #t)
                    ;(set-target-data! tmp-FAKE (list (- etmp stmp) info))
                    
                    (add-recipe t tmp-FAKEid)]
                   [else
                    (set-rusage-data-submake?! info #t)
                    ;; is the following line correct?
                    (set-target-data! t (cons (- etmp stmp) (cons info (target-data t))))])]
                [else
                 (error 'parse-line "Expected finishing submake line, got ~a instead" finishline)])

              (read-file (struct-copy state st
                                      [dirs (cdr dirs-local)]
                                      [submakes (cdr submakes-local)]
                                      [overheads (if (empty? overheads-local)
				      		     overheads-local
						     (cdr overheads-local))])))])]         
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
                [`(,pid_ "finishing" "sub-make:" ,n_ ":" ,cmd ... ";" "in" "directory" . ,rest)
                 ;; TODO: add overhead time to this somehow...
		 (define stmp (if (empty? overheads-local)
		 	      	  0	  
				  (cadr (car overheads-local))))	
	 	 (define etmp (if (empty? overheads-local)
		 	      	  0
				  (car (car overheads-local))))		

		 (define n (string->number n_))
                 ;; should be at top of submake stack so check that n's match
                 (when (empty? submakes-local)
                   (error 'parse-line "Submakes is empty"))
                 (unless (= n (submake-n (car submakes-local)))
                   (error 'parse-line "Expected submake with n of ~a to be on top of stack, not ~a" n (submake-n (car submakes-local))))
                 (unless (= 0 (submake-depth (car submakes-local)))
                   (error 'parse-line "~a depth is ~a not zero" argv-cmd (submake-depth (car submakes-local))))
                 (define pid (string->number (string-trim (string-trim pid_ "[") "]")))
		 (set-rusage-data-id! info n)
                 (set-rusage-data-pid! info pid)
                 (define cdir (car rest))
		 (set-rusage-data-dir! info cdir)
                 (when (debug?)
                   (printf "current directory is ~a\n" cdir))
                 ;; error checking.

                 
	         (define-values (dirs mfiles) (submake-dirs-makefiles (string-join cmd " ")))
	         (define ldir (let ([tmp (for/fold ([ndir cdir])
	    	                                   ([dir dirs])
                                           (path->complete-path dir ndir))])
		                (if (empty? mfiles)
			            tmp
			            (path->complete-path (car mfiles) tmp))))
         
                 (unless (equal? (car dirs-local) ldir)
                   (error 'parse-line "Expected to be leaving directory ~a; but leaving ~a instead." (car dirs-local) cdir))

                 (define count (submake-count (car submakes-local)))
                 (cond
                   [(> count 1) ;; use FAKE target.
                    (define last-edges (get-last-edges t count))
                    ;; add edges to fake target.
                    ;; remove edges from current target.
		    (define tmp-name (symbol->string (gensym "FAKE")))
		    (define tmp-FAKEid (create-targetid tmp-name "top"
							(next-version! (cons tmp-name "top"))))
                    (define tmp-FAKE (create-target tmp-name))
                    (set-target-mfile! tmp-FAKE "top")
		    (set-target-phony?! tmp-FAKE #t)
                    ;; add to graph
                    (add-target-to-makegraph mgraph tmp-FAKEid tmp-FAKE)
                    
                    (for ([last-edge last-edges])
		      (remove-edge t last-edge)
                      (add-edge tmp-FAKE last-edge 'dep))
                    
                    ;; add edge from current target to fake target.
                    ;; should be a recipe
                    (set-rusage-data-submake?! info #t)
                    ;(set-target-data! tmp-FAKE (list (- etmp stmp) info))
                    
                    (add-recipe t tmp-FAKEid)]
                   [else
                    (set-rusage-data-submake?! info #t)
                    ;; is the following line correct?
                    (set-target-data! t (cons (- etmp stmp) (cons info (target-data t))))])]
                [else
                 (error 'parse-line "Expected finishing submake line, got ~a instead" finishline)])

              (read-file (struct-copy state st
                                      [dirs (cdr dirs-local)]
                                      [submakes (cdr submakes-local)]
                                      [overheads (if (empty? overheads-local)
				      		     overheads-local
						     (cdr overheads-local))])))]
           [else
            (error 'parse-line
                   "Expected times line to follow argv line; got ~a instead\n" timesline)])]
        [else
         (when (debug?)
           (printf "Didn't match ~a\n" line))
         (read-file st)])))
  
  (define (read-file st)
    (define line (read-full-line fip))
    (parse-line line st))
  
  (read-file (state (list (cons #f #f)) (list #f) #f '() '() '() #f))
  bgraph)
      
(define (parse-rusage file-path)
  (define file (open-input-file file-path #:mode 'text))
  (define result (parse-file file))
  (close-input-port file)
  result)
