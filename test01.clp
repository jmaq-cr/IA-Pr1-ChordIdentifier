(defglobal
    ?*notation_type* = 1
    ?*notes_num* = (create$ 0 2# 4# 1b 1# 3# 5# 7b 5b 3b 6b 4b 2b 7# 6#)
    ?*notes_ing* = (create$ c d e f g a b cb db eb gb ab bb c# f#)
    ?*notes_ita* = (create$ do re mi fa sol la si dob reb mib solb lab sib do# fa#)

    ?*tonality* = nil
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

(deffunction select-tonality()
    (if(eq (yes-or-no-p "Desea numeros (s/n)? ") yes)
        then (bind ?return_value (ask-for-note "Ingrese la nota:" ?*notes_num*))
        else (if (eq ?*notation_type* 1) ;notation_type = ing
                then
                    (bind ?return_value (ask-for-note "Ingrese la nota:" ?*notes_ing*))
                else
                    (bind ?return_value (ask-for-note "Ingrese la nota:" ?*notes_ita*))
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

)

(deffunction select-three-altitudes()
    (bind ?a (ask-question "Ingrese la altura1:" 0 1 2 3 4 5 6 7 8))
    (bind ?b (ask-question "Ingrese la altura2:" 0 1 2 3 4 5 6 7 8))
    (bind ?c (ask-question "Ingrese la altura3:" 0 1 2 3 4 5 6 7 8))
    (bind ?lo (min(min ?a ?b) ?c))
    (bind ?hi (max(max ?a ?b) ?c))
    (bind ?mid (- (+ ?a ?b ?c) ?lo ?hi))
    (return (create$ ?lo ?mid ?hi))
)