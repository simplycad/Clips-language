
(deffunction pytaj (?pytanie $?allowed-values)
   (printout t ?pytanie)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?pytanie)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
		  	(printout t "(Dziekuje za skorzystanie z programu! Do widzenia)" crlf)
   ?answer)
   

(defrule start
   (initial-fact)
=>
   (printout t ".-----------------------------------------." crlf)
   (printout t "|                                         |" crlf)
   (printout t "|            *Naprawa samochodu*          |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|              Wymiana plynow             |" crlf)
   (printout t "|        i filtrow ekspolatacyjnych       |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|          autor: Grzegorz Janik          |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "`-----------------------------------------'" crlf)
   
   
   (printout t "Witaj! Podaj podaj przebieg samochodu z ostatniego serwisu" crlf)
   (bind ?przebieg1 (read))
   (printout t "Podaj aktualny przebieg samochodu" crlf)
   (bind ?przebieg2 (read))
   (assert (przebieg1 ?przebieg1) (przebieg2 ?przebieg2))
)

(defrule R1
	(przebieg1 ?przebieg1)(przebieg2 ?przebieg2)
=> 
	(if (< ?przebieg2 (+ ?przebieg1 15000))
	then
	(printout t "(Przebieg Twojego samochodu jest zbyt niski aby dokonac wymiany plynow i filtrow)" crlf)
	(printout t (pytaj "Czy chcesz kontynuowac? <tak/nie> " tak nie) crlf)
	else
	(printout t "(Od ostatniego przegladu przejechales conajmniej 15000 km, wymagany serwis)" crlf))
	
)


