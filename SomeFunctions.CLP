;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Definición de variables globales
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defglobal 
  ?*majorScale* = (create$ 2 2 1 2 2 2 1 2 2 1)
  ?*notes* = (create$ "a" "b" "c" "d" "e" "f" "g")
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Aqui comienzan las definiciones de las funciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(deffunction getSublist (?i ?j ?list $?c)
	(subseq$ ?list ?i ?j)
)

(deffunction elementAt (?text ?position $?c)
	(sub-string ?position ?position ?text)
)

(deffunction isMember(?element ?list $?c)
	(member$ ?element ?list)
)

(deffunction printData (?message $?c)
	(printout t ?message crlf)
)

(deffunction readData (?message $?c)
	(printData ?message)
	(bind ?string (readline))
)

(deffunction concatStr (?numberNote ?note ?end $?c)
	(str-cat ?numberNote ?note ?end )
)

(deffunction validateNote(?note $?c)
	(bind ?len (str-length ?note))
	(if (<= ?len 2)
		then
		(bind ?subnote (elementAt ?note 1))
		(if(neq (isMember ?subnote ?*notes*) FALSE)
			then
			(return ?subnote)
			else
				(return FALSE)
		)
		else
			(return FALSE)
	)
)

(deffunction setChord (?note1 ?note2 ?note3 $?c)
	(bind ?note1 (validateNote ?note1))
	(bind ?note2 (validateNote ?note2))
	(bind ?note3 (validateNote ?note3))
	(bind ?flag 1)
	(if(neq ?note1 FALSE)
		then
		;(retract 1)
		(assert-string (concatStr  "(note1 " ?note1 ")"))
		else
		(printData "Error en la nota 1")
		(assert (error 1))
		(bind ?flag FALSE)
	)
	
	(if(neq ?note2 FALSE)
		then
		;(retract 2)
		(assert-string (concatStr  "(note2 " ?note2 ")"))
		else
		(printData "Error en la nota 2")
		(assert (error 2))
		(bind ?flag FALSE)
	)

	(if(neq ?note3 FALSE)
		then
		;(retract 3)
		(assert-string (concatStr  "(note3 " ?note3 ")"))
		else
		(printData "Error en la nota 1")
		(assert (error 3))
		(bind ?flag FALSE)
	)

	(if(neq ?flag FALSE)
		then
		(assert (scale sinAsignar))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;definición de reglas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrule inicio
	(note1 ?note1)
	(note2 ?note2)
	(note3 ?note3)
	(not(scale))
	=>
	(reset)
	(setChord ?note1 ?note2 ?note3)	
)

(defrule defineScale
	(scale sinAsignar)
	=>
	(bind ?scaleName (readData "Digite el nombre el nombre de la escala(a b c d e f g): "))
	(while(eq (isMember ?scaleName ?*notes*) FALSE)
		(printData "Error cadena ingresada inválida.")
		(bind ?scaleName (readData "Digite el nombre el nombre de la escala(a b c d e f g): "))
	)
	(assert-string (concatStr  "(scale " ?scaleName ")"))
	(retract 4)
)