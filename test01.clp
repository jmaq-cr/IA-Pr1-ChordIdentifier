(defglobal
    ?*notation_type* = 1
    ?*notes_num* = (create$ 0 2# 4# 1b 1# 3# 5# 7b 5b 3b 6b 4b 2b 7# 6#)
    ?*notes_ing* = (create$ c d e f g a b cb db eb gb ab bb c# f#)
    ?*notes_ita* = (create$ do re mi fa sol la si dob reb mib solb lab sib do# fa#)

    ?*tonality* = nil

    ?*notes* = (create$ a b c d e f g)
    ?*majorScaleSharp* = (create$ c g d a e b f# c#)
    ?*majorScaleBemol* = (create$ c f bb eb ab db gb cb)
    ?*ordenSharp* = (create$ f c g d a e)
    ?*ordenBemol* = (create$ b e a d g c)
    ?*actualScale* = (create$ )
    ?*chordNames* = (create$ "I" "ii" "iii" "IV" "V" "vi" "bVII")
    ?*majorScale* = (create$ 2 2 1 2 2 2 1)
    ?*inversionNames* = (create$ "Fundamental" "Primera Inversión" "Segunda Inversión")
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Vuelva a intentar." crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction ask-for-note (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "Vuelva a intentar." crlf)
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   (bind ?member_index (member$ ?answer ?allowed-values))
   (bind ?return_value (nth$ ?member_index ?*notes_ing*))

   ?return_value)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes si no y s n))
   (if (or (eq ?response yes) (eq ?response y) (eq ?response si) (eq ?response s))
       then yes 
       else no))

(deffunction select-language ()
    (printout t 
    "         1- Notacion Inglesa" crlf crlf
    "         2- Notacion Italiana" crlf crlf
    )
   (bind ?response (ask-question "Seleccione la notacion: " 1 2))
    (bind ?*notation_type* ?response)
    (printout t "Tipo de notacion: " ?*notation_type* crlf)
)


(deffunction isMember(?element ?list $?c)
	(member$ ?element ?list)
)

(deffunction select-tonality()
    (if(eq (yes-or-no-p "Desea numeros para indicar la tonalidad (s/n)? ") yes)
        then (bind ?return_value (ask-for-note "Ingrese la nota:" ?*notes_num*))
        else (if (eq ?*notation_type* 1) ;notation_type = ing
                then
                    (bind ?return_value (ask-for-note "Ingrese la nota para la tonalidad:" ?*notes_ing*))
                else
                    (bind ?return_value (ask-for-note "Ingrese la nota para la tonalidad::" ?*notes_ita*))
             )
    )
)

(deffunction select-three-notes()

    (if (eq ?*notation_type* 1) ;notation_type = ing
        then
            (bind ?note1 (ask-for-note "Ingrese la nota1:" ?*notes_ing*))
            (bind ?note2 (ask-for-note "Ingrese la nota2:" ?*notes_ing*))
            (bind ?note3 (ask-for-note "Ingrese la nota3:" ?*notes_ing*))
        else
            (bind ?note1 (ask-for-note "Ingrese la nota1:" ?*notes_ita*))
            (bind ?note2 (ask-for-note "Ingrese la nota2:" ?*notes_ita*))
            (bind ?note3 (ask-for-note "Ingrese la nota3:" ?*notes_ita*))
    )
    (return (create$ ?note1 ?note2 ?note3))
)

(deffunction select-three-altitudes(?notes)
    (bind ?a (ask-question "Ingrese la altura1:" 0 1 2 3 4 5 6 7 8))
    (bind ?b (ask-question "Ingrese la altura2:" 0 1 2 3 4 5 6 7 8))
    (bind ?c (ask-question "Ingrese la altura3:" 0 1 2 3 4 5 6 7 8))

    (bind ?altitudes (create$ ?a ?b ?c))
    (bind ?amounts (create$))
    (bind ?index 1)

    (while (< ?index 4)
    	(bind ?amounts (insert$ ?amounts ?index (+ (* (nth$ ?index ?altitudes) 7) (isMember (nth$ ?index ?notes) ?*notes*) )))
	(bind ?index (+ ?index 1))
    )

    (bind ?lo (min(min (nth$ 1 ?amounts) (nth$ 2 ?amounts)) (nth$ 3 ?amounts)))
    (bind ?hi (max(max (nth$ 1 ?amounts) (nth$ 2 ?amounts)) (nth$ 3 ?amounts)))
    (bind ?mid (- (+ (nth$ 1 ?amounts) (nth$ 2 ?amounts) (nth$ 3 ?amounts)) ?lo ?hi))
    (bind ?sortedAmounts (create$ ?lo ?mid ?hi))

    (bind ?result (create$))
    (bind ?index 1)

    (while (< ?index 4)
    	(bind ?result (insert$ ?result ?index (nth$ (isMember (nth$ ?index ?sortedAmounts) ?amounts) ?notes)))
    	(bind ?index (+ ?index 1))
	)
    (return ?result)
)

(deffunction getSublist (?i ?j ?list $?c)
	(subseq$ ?list ?i ?j)
)
member_index
(deffunction elementAt (?text ?position $?c)
	(sub-string ?position ?position ?text)
)

(deffunction concat (?string1 ?string2 $?c)
	(sym-cat ?string1 ?string2)
)

(deffunction getInitialScale(?note $?c)SYMBOL
	(bind ?position(isMember ?note ?*notes*))
	(bind ?start (getSublist 0 (- ?position 1) ?*notes*))
	(bind ?end (getSublist ?position  8 ?*notes*))-
	(return (create$ ?end ?start))
)

(deffunction generateScaleSharp(?tonality ?ordenScale ?majorScale ?character $?c)
	(if (eq ?tonality c) then
		(return (create$ c d e f g a b))
	else
		(if (eq ?tonality c#) then
			(return (create$ c# d# e# f# g# a# b#))
		else
			(if (eq ?tonality "cb") then
				(bind ?*actualScale* (create$ cb db eb fb gb ab bb))
			else
				(bind ?tempScale (getInitialScale(nth$ 1(explode$(elementAt ?tonality 1)))))
				(bind ?total (isMember ?tonality ?majorScale))
				(bind ?index 1)
				(while(< ?index ?total)
					(bind ?noteSharp (nth$ ?index ?ordenScale))
					(bind ?indexNote (isMember ?noteSharp ?tempScale))
					(bind ?noteSharp (concat ?noteSharp ?character))
					(bind ?tempScale(delete$ ?tempScale ?indexNote ?indexNote))
					(bind ?tempScale(insert$ ?tempScale ?indexNote ?noteSharp))
					(bind ?index (+ ?index 1))
				)
				(return ?tempScale)
			)
		)
	)	
)
(deffunction findElement(?element ?list $?c)
        (member$ ?element ?list)
)

(deffunction getElement(?index ?list $?c)
	(nth$ ?index ?list)
)
(deffunction getAmountTones(?note1 ?note2 ?scale $?c)
	(bind ?posNote1 (findElement ?note1 ?scale))
	(bind ?posNote2 (findElement ?note2 ?scale))
	(bind ?total 0)
	(while (neq ?posNote1 ?posNote2)
		(bind ?total (+ ?total (getElement ?posNote1 ?*majorScale*)))
		(bind ?posNote1 (+ (mod ?posNote1 7) 1))
	)
	(return ?total)
)

(deffunction printData (?message $?c)
	(printout t ?message crlf)
)

(deffunction main()
	(select-language)
	(bind ?tonality (select-tonality))
	(bind ?*actualScale* (getInitialScale(nth$ 1(explode$(elementAt ?tonality 1)))))
	(if (eq (isMember ?tonality ?*majorScaleSharp*) FALSE) then
		(bind ?tempScale (generateScaleSharp ?tonality ?*ordenBemol* ?*majorScaleBemol* "b"))
	else
		(bind ?tempScale (generateScaleSharp ?tonality ?*ordenSharp* ?*majorScaleSharp* "#"))
	)
	(printout t "Para ingresar las notas no es necesario indicar el bemol o sostenido." crlf)

	(bind ?notes (select-three-notes))
	(bind ?notes (select-three-altitudes ?notes))
	(printData ?notes)
	(assert (note1 (nth$ 1 ?notes)))
	(assert (note2 (nth$ 2 ?notes)))
	(assert (note3 (nth$ 3 ?notes)))
)
;------------------------------------------------------------------------------------------------
;Algunas reglas
;------------------------------------------------------------------------------------------------
(defrule obtainChord
	(note1 ?note1)
	(note2 ?note2)
	(note3 ?note3)
	=>
	(if (and(findElement ?note1 ?*actualScale*)
	        (findElement ?note2 ?*actualScale*)
		(findElement ?note3 ?*actualScale*))then
		(bind ?total1 (getAmountTones ?note1 ?note2 ?*actualScale*))
		(bind ?total2 (getAmountTones ?note2 ?note3 ?*actualScale*))
		(if (or (and (eq ?total1 4) (eq ?total2 3)) 
			(and (eq ?total1 3) (eq ?total2 4))
			(and (eq ?total1 3) (eq ?total2 3)))
			then
			(bind ?numberChord (findElement ?note1 ?*actualScale*))
			(printData (concat Estado:  (getElement 1 ?*inversionNames*)))
			(printData (concat "Numero de acorde:"  (getElement ?numberChord ?*chordNames*)))
			else	
				(if (or (and (eq ?total1 3) (eq ?total2 5)) 
					(and (eq ?total1 4) (eq ?total2 5))
					(and (eq ?total1 3) (eq ?total2 6)))
					then
					(bind ?numberChord (findElement ?note3 ?*actualScale*))
					(printData (concat Estado:  (getElement 2 ?*inversionNames*)))
					(printData (concat "Número de acorde:"  (getElement ?numberChord ?*chordNames*)))
				else
					(if (or (and (eq ?total1 5) (eq ?total2 4)) 
						(and (eq ?total1 5) (eq ?total2 3))
						(and (eq ?total1 6) (eq ?total2 3)))
						then
						(bind ?numberChord (findElement ?note2 ?*actualScale*))
						(printData (concat Estado:  (getElement 3 ?*inversionNames*)))
						(printData (concat "Numero de acorde:"  (getElement ?numberChord ?*chordNames*)))
						else
							(printData "No hay acorde.")
					)
				)
			)
	else
		(printData "No hay acorde.")
	)
)

(defrule start
	=>
	(main)

)