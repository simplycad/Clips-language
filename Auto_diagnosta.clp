(deffunction pobierz_liczbe (?limit_low ?limit_high)
   (bind ?liczba (read))
   (while(or (not (integerp ?liczba))
             (< ?liczba ?limit_low)
             (> ?liczba ?limit_high)
         )
      (format t "%nPodales zla liczbe, sprobuj ponownie <%d,%d>:" ?limit_low ?limit_high)
      (bind ?liczba (read))
   )
   (printout t crlf)
   ?liczba
)

(deffunction warunek ()
   (bind ?odp (readline))
   (bind ?odp (lowcase ?odp))
   (while(and (<> 0 (str-compare ?odp "tak")) (<> 0 (str-compare ?odp "nie")))
      (printout t "<tak|nie>: ")
      (bind ?odp (readline))
   )
   (printout t crlf)
   ?odp
)

(deffunction pytaj (?pytanie $?allowed-values)
   (printout t ?pytanie)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?pytanie)
      (bind ?answer (read))
	  
    (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
	(printout t "(Dziekuje za skorzystanie z programu! Do widzenia)" crlf)
   ?answer
)
   
   
   
(deffunction odpowiedz (?o)
(switch ?o
(case 1 then (bind ?diesel 1)(assert (d ?diesel)))
(case 2 then (bind ?benzynka 2)(assert (b ?benzynka)))
(case 3 then (bind ?gaz 3)(assert (g ?gaz)))
(default (bind ?zle 4)(assert (z ?zle)))
))


(defrule Test
   (initial-fact)
=>
   (printout t ".-----------------------------------------." crlf)
   (printout t "|                                         |" crlf)
   (printout t "|           *Naprawa samochodu*           |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|              Wymiana oleju              |" crlf)
   (printout t "|              w samochodzie              |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|          autor: Grzegorz Janik          |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "`-----------------------------------------'" crlf)
   
   (printout t "Witaj. Czy chesz przeprowadzic test diagnostyczny?" <tak/nie> crlf)
   (assert (start (warunek)))
   )
   
(defrule Olej
  (start ?start)
  (test (= 0 (str-compare ?start "nie")))
=>
(printout t "Czy przeprowadzic kontrole wymiany oleju?" <tak/nie> crlf)
(assert (olej (warunek)))   

)

(defrule Olej_test
(olej ?olej)
(test (= 0 (str-compare ?olej "tak")))
 =>
 (printout t "Podaj podaj przebieg samochodu z ostatniego serwisu" crlf)
   (bind ?przebieg1 (read))
   (printout t "Podaj aktualny przebieg samochodu" crlf)
   (bind ?przebieg2 (read))
   (assert (przebieg1 ?przebieg1) (przebieg2 ?przebieg2))
)

(defrule Olej_przebieg
	(przebieg1 ?przebieg1)(przebieg2 ?przebieg2)
=> 
	(if (< ?przebieg2 (+ ?przebieg1 15000))
	then
	(printout t "(Przebieg Twojego samochodu jest zbyt niski aby dokonac wymiany plynow i filtrow)" crlf)
	(printout t (pytaj "Czy chcesz kontynuowac? <tak/nie> " tak nie) crlf)
	else
	(printout t "(Od ostatniego przegladu przejechales conajmniej 15000 km, wymagany serwis)" crlf))
	
)

(defrule Olej_silnik
(przebieg1 ?przebieg1)(przebieg2 ?przebieg2)
=>

(printout t "(Wybierz rodzaj Twojego samochodu - wybierz 1, 2 lub 3)" crlf)
(printout t "(1. Diesel)" crlf)
(printout t "(2. Benzyna)" crlf)
(printout t "(3. LPG)" crlf)
(odpowiedz (read))

)

(defrule Olej_diesel
(d ?diesel)
=>
	(printout t "Czy Twoj samochodu posiada filr czasek stalych <tak|nie>: ")
	(bind ?dpf (warunek))
	(if (= 0 (str-compare "tak" ?dpf)) then (bind ?filtr 0)
	(printout t "(Zastosuj olej Castrol Edge 5W30 TDI DPF Long Life III)" crlf) else (printout t "(Zastosuj olej Castrol EDGE 5W40 Turbodiesel)" crlf)
	(bind ?filtr 1)
	)
)


(defrule Olej_benzyna
(b ?benzynka)
=>
(printout t "Czy Twoj samochod posiada filr GPF lub OPF" crlf)
(bind ?gopf (warunek))
	(if (= 0 (str-compare "tak" ?gopf)) then (bind ?filtr 0)
	(printout t "(Zastosuj olej TOTAL QUARTZ INEO ECS 5W30)" crlf) else (printout t "(Zastosuj olej VALVOLINE MaxLife 10W40)" crlf)
	(bind ?filtr 1)
	)
)

(defrule Olej_gaz
(g ?gaz)
=>
(printout t "Czy podstawowe paliwa to benzyna? <tak|nie>:" crlf)
(bind ?paliwb (warunek)) 
(assert (paliwb ?paliwb))

(printout t "Czy Twoj samochod posiada filr GPF lub OPF <tak|nie>:" crlf)
(bind ?gpf (warunek))


(if (= 0 (str-compare "tak" ?gpf)) then (bind ?filtrbenz 1) 
else (bind ?filtrbenz 0)) (assert (filtrbenz ?filtrbenz)) 

)

(defrule Olej_gazbenz
(filtrbenz ?filtrbenz) (paliwb ?paliwb)
=>
(if (and (= 0 (str-compare "tak" ?paliwb))(= ?filtrbenz 1)) then (printout t "Czyli to olej benzyna" crlf))
(if (and (= 0 (str-compare "nie" ?paliwb))(= ?filtrbenz 0)) then (printout t "qwert" crlf)
(bind ?paliwd (warunek)) (assert (paliwd ?paliwd)))
(if (and (= 0 (str-compare "nie" ?paliwb))(= ?filtrbenz 1)) then (printout t "Blad systemu - prosze wybrac ponownie" crlf)) 
)


(defrule Olej_gazdiesl
(paliwd ?paliwd)
=>
(printout t "Czy posiada filtr DPF? <tak|nie>:" crlf) 
(bind ?gazdiesl (warunek))
(if (= 0 (str-compare "tak" ?gazdiesl)) then (printout t "memlel" crlf) else 
(printout t "kukuryku" crlf))
)


(defrule Olej_zle
(z ?zle)
=>
(printout t "(Podales zla odpowiedz - wybierz 1, 2 lub 3)" crlf)
(odpowiedz (read))
)


(defrule Koniec
(olej ?olej)
  (test (= 0 (str-compare ?olej "nie")))
=>
  (printout t "Dziekuje za skorzystanie z programu.")
)
