(defglobal
    ?*notation_type* = 1
    ?*notes_num* = (create$ 0 2# 4# 1b 1# 3# 5# 7b 5b 3b 6b 4b 2b 7# 6#)
    ?*notes_ing* = (create$ c d e f g a b cb db eb gb ab bb c# f#)
    ?*notes_ita* = (create$ do re mi fa sol la si dob reb mib solb lab sib do# fa#)

    ?*tonality* = nil
)
(deffunction read-a-number ()
    (bind ?iscorrect FALSE)
    
    (while (neq ?iscorrect TRUE)
        (printout t "Ingrese un numero: ")
        (bind ?number (read))   
        (if (eq (numberp ?number) TRUE)
            then 
                (bind ?iscorrect TRUE)
            else
                (printout t "Incorrecto, vuelva a intentar." crlf)
        )
        
    )
    (return ?number)
)

(deffunction select-language ()
    (printout t 
    "         1- Notacion Inglesa" crlf crlf
    "         2- Notacion Italiana" crlf crlf
    "Seleccione la notacion" crlf crlf
    )
   (bind ?response (read-a-number))
   (switch ?response
      (case 1 then (bind ?*notation_type* 1))
      (case 2 then (bind ?*notation_type* 2))
      (default none)
    )
    (printout t "Tipo de notacion: " ?*notation_type* crlf)
)

(deffunction convert-note ()
    (bind ?isvalid FALSE)

    
    (while (neq ?isvalid TRUE)
        (printout t 
        "Seleccione la nota" crlf crlf
        )
        (bind ?response (read))

        (if (eq ?*notation_type* 1) ;notation_type = ing
            then
                (bind ?member_value (member$ ?response ?*notes_ing*))
                (if (neq ?member_value FALSE)
                    then
                        (bind ?isvalid TRUE)
                        (bind ?return_value ?response)
                )
        )

        (if (eq ?*notation_type* 2) ;notation_type = ita
            then
                (bind ?member_value (member$ ?response ?*notes_ita*))
                (if (neq ?member_value FALSE)
                    then
                        (bind ?isvalid TRUE)
                        (bind ?return_value (nth$ ?member_value ?*notes_ing*))
                )
        )
    )
    (return ?return_value)
)

(deffunction convert-number-to-note ()
    (bind ?isvalid FALSE)
    (while (neq ?isvalid TRUE)
        (printout t 
        "Seleccione la nota" crlf crlf
        )
        (bind ?response (read))

        (bind ?member_value (member$ ?response ?*notes_num*))
        (if (neq ?member_value FALSE)
            then
                (bind ?isvalid TRUE)
                (bind ?return_value (nth$ ?member_value ?*notes_ing*))
        )
    )
    (return ?return_value)
)

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))

(deffunction select-tonality()
    (if(eq (yes-or-no-p "Desea numeros (yes/no)? ") yes)
        then (convert-number-to-note)
        else (convert-note)
    )


)

