# Little-Man-Computer-Common-Lisp
A "Little man computer" in Common Lisp. This implementation was written by Davide Pietrasanta and Veronica Orsanigo. 

You can find the specifics [Here](https://github.com/davidepietrasanta/Little-Man-Computer-Common-Lisp/blob/master/LP%20201901%20E1P%20LMC.pdf)

Il progetto consiste nella creazione di un LMC (little man computer), che e' formato da:
- Memoria di 100 celle numerate da 0 a 99 contenenti numeri fra 0 e 999, senza distinzione fra numeri e istruzioni
- Registro accumulatore (0 all'inizio)
- Program counter: contiene l'indirizzo dell'istruzione da eseguire (0 all'inizio), se raggiunge 99 il valore successivo e' 0
- Coda input: contiene valori dati in input all'LMC, compresi fra 0 e 999
- Coda output: all'inizio e' vuota, poi contiene i valori di output dell'LMC, compresi fra 0 e 999
- Flag: bit che vale all'inizio 0 e 1 se l'ultima operazione aritmatica da' risultato maggiore di 999 o minore di 0
Al LMC si aggiunge anche la possibilita' di lettura ed esecuzione di file assembly.

Per il progetto in Common Lisp ci sentiamo di dover illustrare tali specifiche:
-Un file completamente vuoto viene considerato corretto e generera' una memoria di cento 0.
-Se in un file ci sono righe vuote esse non vengono considerate per il calcolo del valore delle labels.
-Se in un file ci sono righe contenenti solo spazi esse non vengono considerate per il calcolo del valore delle labels.
-Se in un file ci sono righe contenenti solo commenti esse non vengono considerate per il calcolo del valore delle labels.
-Se in una riga viene usata una label non presente nel resto del file il programma terminera' con NIL. 
-Se invece una label e' presente ma non viene usata (dunque e' prima di un'istruzione) il programma continua tranquillamente.
-Se in una riga e' presente solo una label, in assenza di istruzioni, il programma terminera' con NIL.
-In caso il file sia NON ben formato allora la lmc-load terminera' con NIL e cosi' anche la lmc-run.
-In caso si voglia esaminare piu' a fondo dove il file sia sbagliato (secondo il programma) bisognera' invocare lmc-load1 
 che ritorna la memoria completa con NIL dove ha trovato un errore.
-Sia le labels che le istruzioni sono case insensitive.
-Le labels accettate sono solo alfanumeriche, iniziano con una lettera e sono diverse da istruzioni
 (ES. A1b2, a1bcd2, ciao, ciao123, a123, aBcD)

Ora seguono alcuni esempio di richiamo a funzioni presenti nel progetto:
- (execution-loop
	'(state :acc 0  
		:pc 0 
		:mem (901 103 902 10 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0
			0 0 0 0 0 0 0 0 0 0) 
		:in (10) 
		:out () 
		:flag noflag))

- (one-instruction
	'(state :acc 0  
		:pc 0 
		:mem (901 103 902 10 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0 
			0 0 0 0 0 0 0 0 0 0
			0 0 0 0 0 0 0 0 0 0) 
		:in (10) 
		:out () 
		:flag noflag))

-(lmc-run "nome_file.txt" (list 901 902 705 600 0 4 5 6 7 8 9 0))
 (lmc-run "nome_file.txt" '(901 902 705 600 0 4 5 6 7 8 9 0))

-(lmc-load "nome_file.txt")


A cio' aggiungiamo un commento sugli HLT e sui possibili loop. 
Riteniamo che imporre un controllo iniziale sulla presenza di HLT nel file vada a ridurre la potenza della LM. Una istruzione di HLT potrebbe esser costruita e poi usata in run-time.
Abbiamo dunque scelto di non depotenziare la LM e di lasciare la costruzione di eventuali HLT in run-time al programmatore/utente.
