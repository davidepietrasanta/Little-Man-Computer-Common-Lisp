;;;; -*- Mode: Lisp -*-
;; lmc.lisp
;; Progetto lisp
;; 844824_Pietrasanta_Davide_LP_E1P_2018_LMC
;; 814247_Orsanigo_Veronica_LP_E1P_2018_LMC



(defstruct (state (:type list) :named) A ACC PP PC M MEM I IN O OUT F FLAG)

(defstruct (halted-state (:type list) :named) A ACC PP PC M MEM I IN O OUT F FLAG)

(defparameter flag 'flag)

(defparameter noflag 'noflag)


(defun check-state (state)
  "Controlla che state sia uno stato e che i suo parametri siano corretti"
  (cond((state-p state)
        (and
         (check-acc (state-acc state))
         (check-pc (state-pc state))
         (check-mem (state-mem state))
         (check-in (state-in state))
         (check-out (state-out state))
         (check-flag (state-flag state))))
       ((halted-state-p state)
        (and
         (check-acc (halted-state-acc state))
         (check-pc (halted-state-pc state))
         (check-mem (halted-state-mem state))
         (check-in (halted-state-in state))
         (check-out (halted-state-out state))
         (check-flag (halted-state-flag state))))
       (T
        nil)))


(defun check-acc (acc)
  (and 
   (>= acc 0) 
   (< acc 1000)))


(defun check-pc (pc)
  (and 
   (>= pc 0) 
   (< pc 100)))


(defun check-list (list)
  (cond ((null list)t)
        ((equal (car list) nil)
         nil)
        ((and 
          (>= (car list) 0) 
          (< (car list) 1000)) 
         (check-list (cdr list))))) 
 

(defun check-mem (mem)
  (and 
   (listp mem) 
   (= (list-length mem) 100) 
   (check-list mem)))


(defun check-in (in)
  (and 
   (listp in) 
   (check-list in)))


(defun check-out (out)
  (and 
   (listp out) 
   (check-list out)))


(defun check-flag (f)
  (or 
   (eql f flag) 
   (eql f noflag)))


(defun mod-1000 (x)
  (cond ((> x 999)
        (mod x 1000))
        ((< x 0)
         (+ x 1000))
        (T x)
))


(defun mod-100 (x)
  (cond ((> x 99)
        (mod x 100))
        ((< x 0)
         (+ x 100))
        (T x)
))


(defun incr-pc (x)
  "Incrementa il pc"
  (mod-100 (+ x 1)))


(defun one-instruction (state)
  "Esegue un' istruzione singola"
  (cond
   ((equal (check-state state) nil)
    nil)
   (T
    (let ((istr (find-istr state)))
      (cond ((equal (first-istr istr)  1) ; add
             (add istr state))
            ((equal (first-istr istr) 2) ; sub
             (sub istr state))
            ((equal (first-istr istr) 3) ; store
             (store istr state))
            ((equal (first-istr istr) 5) ; load
             (loadi istr state))
            ((equal (first-istr istr) 6) ; branch
             (branch istr state))
            ((equal (first-istr istr) 7) ; branch-z
             (branch-z istr state))
            ((equal(first-istr istr)  8) ; branch-p
             (branch-p istr state))
            ((equal istr 901) ; input
             (input state))
            ((equal istr 902) ; output
             (output state))
            ((equal (first-istr istr) 0) ; halt
             (halt state)))))))

 
(defun execution-loop (state)
  "Esegue un ciclo di istruzioni a partire dallo stato state"
  (check-state state)
  (cond ((equal (check-state state) nil)
         nil)
        ((halted-state-p state)
         (halted-state-out state))
        ((equal (find-istr state) nil)
         nil)
        ((or 
          (and 
           (>= (find-istr state) 400) 
           (< (find-istr state) 500)) 
          (eql (find-istr state) 900) 
          (> (find-istr state) 902))
         nil)
        (T
         (let ((new-state (one-instruction state)))
           (execution-loop new-state)))))
          

(defun find-istr (state)
  "Trova istr contenuta in cella di memoria puntata da pc 
es: pc=2 mem=(102 201 200 ...) -> 200"
  (let (
        (pc (state-pc state)) 
        (mem (state-mem state))) 
    (nth pc mem)))


(defun first-istr (istr)
  "fz che data istr, ricava la prima cifra es: 102 -> 1"
  (let ((xx (mod istr 100))) 
    (/ (- istr xx) 100)))


(defun find-xx (istr)
  "Trova le ultime due cifre di un'istr"
  (mod istr 100))


;;; fz che modificano acc, pc, mem, in, out, flag

(defun mod-acc (state acc)
  (setf (state-acc state) acc)
  state)

(defun mod-pc (state pc)
  (setf (state-pc state) pc)
  state)

(defun mod-mem1 (state num-mem) ; per store
  (setf 
   (nth num-mem (state-mem state)) 
   (state-acc state))
  state)
  
(defun mod-mem2 (state num-mem) ; per load
  (setf 
   (state-acc state) 
   (nth num-mem (state-mem state)) )
  state)

(defun mod-in (state in)
  (setf (state-in state) (cdr in))
  state)

(defun mod-out (state out)
  (setf 
   (state-out state) 
   (append out (list (state-acc state))))
  state)
        
(defun mod-flag (state flag)
  (setf (state-flag state) flag)
  state)


;; add 1XX 

(defun add (istr state)
  (let ((r (+ 
            (state-acc state) 
            (nth (find-xx istr) (state-mem state)))))
    (cond ((> r 999) 
           (mod-flag state 'flag))
          (T
           (mod-flag state 'noflag))))
  (mod-acc state 
           (mod-1000 
            (+ 
             (state-acc state) 
             (nth (find-xx istr) (state-mem state)))))
  (mod-pc state 
          (incr-pc (state-pc state)))
  )


;; sub 2XX

(defun sub (istr state)
  (let ((r (- 
            (state-acc state) 
            (nth (find-xx istr) (state-mem state)))))
    (cond ((< r 0) 
           (mod-flag state 'flag))
          (T
           (mod-flag state 'noflag))))    
  (mod-acc state 
           (mod-1000 
            (- 
             (state-acc state) 
             (nth (find-xx istr) (state-mem state)))))
  (mod-pc state 
          (incr-pc (state-pc state))))


;; store 3XX

(defun store (istr state)
  (mod-mem1 state (find-xx istr))
  (mod-pc state (incr-pc (state-pc state))))


;; load 5XX

(defun loadi (istr state)
  (mod-mem2 state (find-xx istr))
  (mod-pc state (incr-pc (state-pc state))))


;; branch 6XX

(defun branch (istr state)
  (mod-pc state (find-xx istr)))


;; branch-zero 7XX

(defun branch-z (istr state)
  (cond ((and 
          (equal (state-acc state) 0) 
          (equal (state-flag state) 'noflag))
         (mod-pc state (find-xx istr)))
        (T 
         (mod-pc state (incr-pc (state-pc state))))))


;; branch >0 8XX

(defun branch-p (istr state)
  (cond ((equal (state-flag state) 'noflag)
         (mod-pc state (find-xx istr)))
        (T
         (mod-pc state (incr-pc (state-pc state))))))


;; input 901

(defun input (state)
  (cond ((null (state-in state)) nil)
        (T
         (mod-acc state (car (state-in state)))
         (mod-in state (state-in state))
         (mod-pc state (incr-pc (state-pc state))))))


;; output 902

(defun output (state)
  (mod-out state (state-out state))
  (mod-pc state (incr-pc (state-pc state))))


;; halt 0xx

(defun halt (state)
  (let ((halted-state (make-halted-state 
                       :acc (state-acc state) 
                       :pc (state-pc state) 
                       :mem (state-mem state) 
                       :in (state-in state) 
                       :out (state-out state) 
                       :flag (state-flag state))))
  halted-state))


(defun commenti (riga) 
  "Elimina i commenti di una stringa"
  (let ((pos (position #\/ riga)))
    (cond ((eql pos nil)
            riga)
          ((> (length riga) (+ 1 pos))
           (cond ((eql (char riga (+ 1 pos)) '#\/)
                  (subseq riga 0 pos))
                 (T
                  riga)))
           (T
            riga))))


(defun commenti-list-string (list-string)
  "Elimina i commenti in una lista di stringhe"
  (mapcar #'commenti list-string))


(defun delete-empty (list-string)
  "Elimina stringhe vuote in una lista di stringhe"
  (let ((list-empty (find-empty-string list-string 0 (list)))
        (deleted 0))
    (delete-empty1 list-string list-empty deleted)))

(defun delete-empty1 (list-string list-empty deleted)
  "I parametri sono la lista di stringhe, 
la lsita delle posizioni delle etichette,deleted da mettere a zero"
  (cond ((null list-empty) list-string)
        (T
         (setf 
          list-string 
          (skip-element (- (car list-empty) deleted) list-string))
         (delete-empty1 list-string (cdr list-empty) (+ 1 deleted)))))


(defun skip-element (n list)
  "Rimuove l'elemento ennesimo da una lista non in modo non distruttivo"
  (let ((list-temp (subseq list 0 n))
        (n1 (+ n 1)))
    (let ((list-temp (nconc list-temp (subseq list n1)))) 
      list-temp)))


(defun find-empty-string (list-string count list-empty)
  "I parametri sono la lista di stringhe, 
un contatore da mettere a zero e una lista da riempire"
    (cond
          ((eql count (length list-string)) list-empty)
          (T
           (cond ((= (length (nth count list-string)) 0)
                  (setf list-empty (nconc list-empty (list count)))
                 ))
           (find-empty-string list-string (+ 1 count) list-empty))))


(defun match (istr)
 "Istr deve essere una stringa.
 Ritorna il valore macchina dell'istruzione, se e' un'istruzione, 
altrimenti NIL."
 (let ((x))
   (cond  ((equalp "ADD" istr) (setf x 100))                    
          ((equalp "SUB" istr) (setf x 200))
          ((equalp "STA" istr) (setf x 300))
          ((equalp "LDA" istr) (setf x 500))
          ((equalp "BRA" istr) (setf x 600))
          ((equalp "BRZ" istr) (setf x 700))
          ((equalp "BRP" istr) (setf x 800))
          ((equalp "INP" istr) (setf x 901))
          ((equalp "OUT" istr) (setf x 902))
          ((equalp "DAT" istr) (setf x 0))  
          ((equalp "HLT" istr) (setf x 0))
          (T (setf x NIL))  ) 
   x))


(defun is-istr(istr)
  "Ritorna T se istr e' un'istruzione, 
altrimenti NIL (istr deve essere una stringa)."
  (let ((x))
    (cond  ((equalp "ADD" istr) (setf x T))                    
           ((equalp "SUB" istr) (setf x T))
           ((equalp "STA" istr) (setf x T))
           ((equalp "LDA" istr) (setf x T))
           ((equalp "BRA" istr) (setf x T))
           ((equalp "BRZ" istr) (setf x T))
           ((equalp "BRP" istr) (setf x T))
           ((equalp "INP" istr) (setf x T))
           ((equalp "OUT" istr) (setf x T))
           ((equalp "DAT" istr) (setf x T))
           ((equalp "HLT" istr) (setf x T))
           (T (setf x NIL)))
    x))


(defun is-istr0(istr)
   "Ritorna T se istr e' un'istruzioneche non accetta argomenti,
 altrimenti NIL (istr deve essere una stringa)."
   (let ((x))
     (cond ((equalp "INP" istr) (setf x T))
           ((equalp "OUT" istr) (setf x T))
           ((equalp "DAT" istr) (setf x T))
           ((equalp "HLT" istr) (setf x T))
           (T (setf x NIL)))
     x))


(defun is-istr1(istr)
   "Ritorna T se istr e' un'istruzione che accetta argomenti,
 altrimenti NIL (istr deve essere una stringa)."
   (let ((x))
     (cond ((equalp "ADD" istr) (setf x T))
           ((equalp "SUB" istr) (setf x T))
           ((equalp "STA" istr) (setf x T))
           ((equalp "LDA" istr) (setf x T))
           ((equalp "BRA" istr) (setf x T))
           ((equalp "BRZ" istr) (setf x T))
           ((equalp "BRP" istr) (setf x T))
           ((equalp "DAT" istr) (setf x T))
           (T (setf x NIL)))
     x))


(defun is-dat(istr)
  "Ritorna T se istr e' DAT, altrimenti NIL"
  (let ((x))
    (cond ((equalp "DAT" istr) (setf x T))
          ( T (setf x NIL)))
    x))


(defun is-lettera(char)
  "Ritorna T se char e' una lettera, altrimenti NIL"
   (and 
    (char-not-greaterp char #\z) 
    (char-not-lessp char #\a))
)


(defun is-numero(char)
  "Ritorna T se char e' un numero, altrimenti NIL"
  (and 
   (char-not-greaterp char #\9) 
   (char-not-lessp char #\0))
)


(defun is-alphanum(char)
  "Ritorna T se char contiene solo caratteri alphanumerici,
 altrimenti NIL"
  (or 
   (is-numero char) 
   (is-lettera char))
)


(defun alphanum(stringa)
  "Ritorna T se char contiene solo caratteri alphanumerici,
 altrimenti NIL"
  (every #'is-alphanum stringa)
)


(defun is-label(label)
  "Ritorna T se la label e' una possibile etichetta,
 altrimenti NIL (label deve essere una stringa)."
  (cond ((eql (is-istr label) T) 
         (return-from is-label NIL))

        ((eql (is-lettera (elt label 0)) NIL) 
         (return-from is-label NIL))
        
        ((eql (alphanum label) NIL) 
         (return-from is-label NIL))
   
        (T 
         (return-from is-label T)))
 )


(defun file->string(filename)
  "Converte un file in una lista di stringhe,
 una stringa per ogni riga"
  (with-open-file
      (Stream filename
             :direction :input)
    (read-all-lines Stream)))


(defun read-all-lines(Stream)
  (let (
        (line (read-line Stream nil nil)))
    (when line
      (append (cons line ()) (read-all-lines stream)))))


;; ATTENZIONE HO VOLUTAMENTE SCELTO IL POSTO DI 
;; STRING-TRIM-LIST IN MODO CHE VENGANO ELIMINATE
;; LE LINEE CON SOLO SPAZI ALL'INTERNO

(defun file->string-no-comment (filename)
  "Legge un file e ritorna una lista di stringhe,
 una stringa per ogni riga, senza commenti,
 linee vuoto o linee con soli spazi."
  (let ((list-string (file->string filename)))
    (setf 
     list-string  
     (string-trim-list (commenti-list-string list-string))) 
    (setf 
     list-string  
     (string-downcase-list (delete-empty list-string)))
    list-string)) 


(defun string-trim-list (list-string)
  (mapcar #'string-trim2 list-string))


(defun string-trim2 (string)
  (string-trim " " string))


(defun string-downcase-list (list-string)
  (mapcar #'string-downcase list-string))


(defun split-string (string)
  (delete-empty (lispworks:split-sequence " " string)))


(defun delimiterp (c) 
  "Se il carattere e' uno spazio ritorna T,
 altrimenti NIL"
  (or (char= c #\Space) (char= c #\,)))


;; (macchina "sub 2") -> 202
(defun macchina (stringa) 
   "Data una stringa ritorna il valore macchina,
 ATTENZIONE: Bisogna passargli una stringa con le etichette gia' risolte" 
   (let ((len (length (split-string stringa))))
         (cond ( (null stringa)
                 NIL)

               ( (and (is-istr (elt (split-string stringa) 0))
                      (eql len 2)
                      (is-istr1 (elt (split-string stringa) 0))
                      (is-label (elt (split-string stringa) 1)) )  
                 NIL)  

               ( (and (eql len 2)
                      (is-dat (elt (split-string stringa) 0))
                      (>= (parse-integer (elt (split-string stringa) 1)) 0)
                      (<= (parse-integer (elt (split-string stringa) 1)) 999))  
                 (+ 0 (parse-integer (elt (split-string stringa) 1))) )
               
               ( (and (is-istr (elt (split-string stringa) 0))
                      (eql len 2)
                      (is-istr1 (elt (split-string stringa) 0))
                      (>= (parse-integer (elt (split-string stringa) 1)) 0)
                      (<= (parse-integer (elt (split-string stringa) 1)) 99))  
                 (+ 
                  (match (elt (split-string stringa) 0)) 
                  (parse-integer (elt (split-string stringa) 1))) )
               
               ( (and (is-istr (elt (split-string stringa) 0)) 
                      (eql len 1)
                      (is-istr0 (elt (split-string stringa) 0)))
                 (match (elt (split-string stringa) 0)))
               
               ( (and (eql len 1)
                      (is-label (elt (split-string stringa) 0)))
                 NIL)
               
               ( (and (eql len 2)
                      (or (is-label (elt (split-string stringa) 0))
                          (is-label (elt (split-string stringa) 1)) ) )
                 NIL)
               
               ( T 
                 NIL))))


;; Quindi dal file si estrae ("add 10", "sub 3") ritorna (110, 203, 0....0)
;; (parsing (list "add 12" "sub 2", etc)) 
(defun parsing (list-of-string)
  "Ritorna una lista con i valori macchina delle istruzioni del file."
  (let ((lista-codice (mapcar #'macchina list-of-string)))
        (let ((x (- 100 (length lista-codice))))
          (append lista-codice (make-list x :initial-element 0)))))


(defun valutatore-label (filename)
  "Crea una lista di liste con dentro le etichette trovate a inizio riga
 e il valore corrispondente in posizione memoria.
  Se ci sono etichette doppie da' error,
 se non ci sono etichette da elaborare da' nil"
  (let ((lista-string (file->string-no-comment filename))
        (lista-label (list))
        (count 0)
        (splitted))
    (valutatore-label1 lista-string lista-label count splitted)))


(defun valutatore-label1 (lista-string lista-label count splitted)
  (cond ((null lista-string) 
         lista-label)
        (T
         (setf splitted (lispworks:split-sequence " " (car lista-string)))
         (cond ((is-label (nth 0 splitted))
                (cond ((is-not-equal-label 
                        lista-label (list (nth 0 splitted) count))
                       (setf 
                        lista-label 
                        (nconc 
                         lista-label (list (list (nth 0 splitted) count))))
                       (setf count (+ 1 count))
                       (valutatore-label1 
                        (cdr lista-string) lista-label count splitted))
                      (T
                       (list "error"))))
                (T
                 (setf count (+ 1 count))
                 (valutatore-label1 
                  (cdr lista-string) lista-label count splitted))))))


(defun is-not-equal-label (list-label label)
  "Controlla che l'etichetta trovata a inizio
 riga non sia presente in piu' posizioni"
  (cond ((null list-label)
         t)
        ((string-equal (nth 0 label) (nth 0 (car list-label)))
         nil)
        (T
         (is-not-equal-label (cdr list-label) label))))


(defun ceck-dat-label(string)
  "Ritorna NIL se c'e' una DAT seguita da una LABEL,
 altrimenti T"
  (cond ( (and (= 2 (length (split-string string)))
               (equalp "DAT" (nth 0 (split-string string)))
               (is-label(nth 1 (split-string string))))
          (return-from ceck-dat-label NIL))
        ( (and (= 3 (length (split-string string)))
               (equalp "DAT" (nth 1 (split-string string)))
               (is-label(nth 2 (split-string string))))
          (return-from ceck-dat-label NIL))
        (T (return-from ceck-dat-label T))))


(defun ceck-if-dat-ok(filename)
  "T se dat e' sempre ok, NIL se dat ha delle labels dopo"
  (every #'ceck-dat-label (file->string-no-comment filename)))
                    
  
(defun delete-label (filename)
  "Ritorna una lista di stringhe senza le labels prima delle istruzioni"
  (mapcar #'delete-in-delete-label (file->string-no-comment filename)))


(defun delete-in-delete-label (string)
  "Elimina la prima parola di una stringa se quella e' un'etichetta"
  (let ((splitted-string (lispworks:split-sequence " " string)))
    (cond ((and 
            (= (length splitted-string) 1) 
            (is-label (nth 0 splitted-string)))
           (setf string NIL))
          ((and 
            (> (length splitted-string) 0) 
            (is-label (nth 0 splitted-string)))
           (setf string (delete-first-word string))))
    (let ((x string)) x) ) )


(defun delete-first-word (stringa)
  "Ritorna una stringa uguale ma senza la prima parola"
  (let ((position-first-space (+ 1 (position #\Space stringa)))
        ) ; Il +1 e' per togliere lo spazio nella new-string
    (let ((new-string (subseq stringa position-first-space))) 
      new-string)))


(defun sostitutore-label (filename) 
  "Ritorna una lista di stringhe con le etichette risolte
 usando le funzioni definite sotto"
  (let ((list-label (valutatore-label filename))
        (list-string (delete-label filename))
        ); Dunque lista-string e' una lista di stringhe 
         ;senza le etichette prima delle istr
    (if (equal (valutatore-label filename) '("error")
               ) ; Se e' ("error") significa che ci 
                 ;sono etichette doppie, quindi fallisce
        (list "error")
 ; se sono state trovate due etichette uguali
 ; ritorna ("error") in modo
 ; da non confondere con file vuoto del parsing
      (sostitutore-label1 list-string list-label))))


(defun sostitutore-label1 (list-string list-label)
  (cond ((null list-label) list-string)
        (T
         (setf 
          list-string 
          (sostituisci-label1 list-string (car list-label) 0))
         (sostitutore-label1 list-string (cdr list-label)))))


(defun sostituisci-label1 (list-string el-label count)
 (cond ((eql count (length list-string))
        list-string)
       (T
        (setf 
         (nth count list-string) 
         (sostituisci-label (nth count list-string) el-label))
        (sostituisci-label1 list-string el-label (+ 1 count))))
 list-string)


(defun sostituisci-label (el-list-string el-label)
  "Sostituisce le etichette solo nei casi giusti"
  (let ((label (nth 0 el-label))
        (list-el (split-string el-list-string))
        (num-label (write-to-string (nth 1 el-label))))
    (cond ((eql (length list-el) 2)
           (cond ((string-equal (nth 1 list-el) label)
                  (setf (nth 1 list-el) num-label)
                 (setf 
                  el-list-string 
                  (concatenate 'string 
                               (concatenate 
                                        'string (nth 0 list-el) " ")
                               num-label))))))
    el-list-string))


(defun is-NIL (something)
  "Ritorna T se something e' NIL, altrimenti NIL"
  (eql something NIL))
   

(defun lmc-load (filename)
  "Dato un file ritorna la memoria in codice macchina LM,
 se ci sono errori ritorna NIL"
  (let ((x (lmc-load1 filename)))
    (cond ( (some #'is-NIL x) (return-from lmc-load NIL)))
    (return-from lmc-load x)))

(defun lmc-load1 (filename)
  "Dato un file ritorna la memoria in codice-macchine LM
 e dove ci sono errori ritorna NIL per le celle di memoria
 in cui si e' verificato l'errore"
  (cond ( (> (length (file->string-no-comment filename)) 100) 
          (return-from lmc-load1 NIL))

        ( (not (ceck-if-dat-ok filename)) 
          (return-from lmc-load1 NIL)) )
  (let ((list-string (sostitutore-label filename)))
    (parsing list-string)))


(defun lmc-run (filename in)
  "Dato un file e una lista di input ritorna l'output della LM"
  (let ((mem (lmc-load filename)))
        (let ((state (make-state 
                      :acc 0 
                      :pc 0 
                      :mem mem 
                      :in in 
                      :out '() 
                      :flag 'noflag)))
          (let ((output (execution-loop state))) output))))



;;;; end of file -- lmc-lisp          
