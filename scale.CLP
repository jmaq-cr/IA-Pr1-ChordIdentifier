(defglobal 
  ?*notes* = (create$ "a" "b" "c" "d" "e" "f" "g")
  ?*majorScaleSharp* = (create$ "c" "g" "d" "a" "e" "b" "f" "c#")
  ?*majorScaleBemol* = (create$ "c" "f" "bb" "eb" "ab" "db" "gb" "cb")
  ?*ordenSharp* = (create$ "f" "c" "g" "d" "a" "e")
  ?*ordenBemol* = (create$ "b" "e" "a" "d" "g" "c")
  ?*actualScale* = (create$ )
) 

(deffunction getSublist (?i ?j ?list $?c)
	(subseq$ ?list ?i ?j)
)

(deffunction elementAt (?text ?position $?c)
	(sub-string ?position ?position ?text)
)

(deffunction isMember(?element ?list $?c)
	(member$ ?element ?list)
)

(deffunction concat (?string1 ?string2 $?c)
	(str-cat ?string1 ?string2)
)

(deffunction getInitialScale(?note $?c)
	(bind ?position(isMember ?note ?*notes*))
	(bind ?start (getSublist 0 (- ?position 1) ?*notes*))
	(bind ?end (getSublist ?position  8 ?*notes*))
	(return (create$ ?end ?start))
)

(deffunction generateScaleSharp(?tonality ?ordenScale ?majorScale ?character $?c)
	(if (eq ?tonality "c") then
		(return (create$ "c" "d" "e" "f" "g" "a" "b"))
	else
		(if (eq ?tonality "c#") then
			(return (create$ "c#" "d#" "e#" "f#" "g#" "a#" "b#"))
		else
			(if (eq ?tonality "cb") then
				(bind ?*actualScale* (create$ "cb" "db" "eb" "fb" "gb" "ab" "bb"))
			else
				(bind ?tempScale (getInitialScale(elementAt ?tonality 1)))
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