
(deffunction akumulator (?v)
(if (> ?v 13) then (printout t "Napiecie w normie, sprawdz stan rozrusznika" crlf))
(if (< ?v 14) then (printout t "Podlacz akumulator do ladowania" crlf))
)

(deffunction cisnienie (?silnik ?cisnienie)
(switch ?silnik
(case "benzyna" then
        (if (> ?cisnienie 6)
          then
            (printout t "Kompresja prawidlowa. Należy sprawdzic szczelnosc zaworu EGR." crlf)
          else
            (printout t "Kompresja nieprawidlowa. Sprawdzic szczelnosc silnika." crlf)
        )
      )
(case "diesel" then
        (if (> ?cisnienie 13)
          then
            (printout t "Kompresja prawidlowa. Należy sprawdzic szczelnosc zaworu EGR." crlf)
          else
            (printout t "Kompresja nieprawidlowa. Sprawdzic szczelnosc silnika." crlf)
        )
      )
(default "Blad w wyborze silnika")
	  )
	  )


(deffunction opony (?mm)
(if (> ?mm 1.5) then (printout t "Bieznik opon prawidlowy, sprawdz zbieznosc kol" crlf))
(if (< ?mm 1.6) then (printout t "Nalezy wymienic opony" crlf))
)

(deffunction wtrysk (?rmp ?ipw)
(bind ?q (** ?rmp ?ipw))
(printout t "(Prawidlowa korekta wtrysku wynosi: )" ?q crlf)
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

(deffunction warunek2 ()
   (bind ?odp (readline))
   (bind ?odp (lowcase ?odp))
   (while(and (<> 0 (str-compare ?odp "benzyna")) (<> 0 (str-compare ?odp "diesel")))
      (printout t "<benzyna|diesel>: ")
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

(defrule Diagnosta
  (start ?start)
  (test (= 0 (str-compare ?start "tak")))
=>
(printout t "Czy samochod traci moc?" <tak/nie> crlf)
(assert (moc (warunek)))   
)

(defrule Aut_moc
  (moc ?moc)
  (test (= 0 (str-compare ?moc "tak")))
=>
(printout t "Czy wymieniono filtr paliwa? " <tak/nie> crlf)
(assert (filtr (warunek)))   
)

(defrule Aut_mocnie
  (moc ?moc)
  (test (= 0 (str-compare ?moc "nie")))
=>
(printout t "Czy wystepuja drgania podczas jazdy? " <tak/nie> crlf)
(assert (drgania (warunek)))   
)

(defrule Aut_drgania

(drgania ?drgania)
(test (= 0 (str-compare ?drgania "tak")))
=>
(printout t "Czy sprawdzono opony pod wzgledem uszkodzen? " <tak/nie> crlf)
(assert (opon (warunek)))
)

(defrule Aut_ubytki
(drgania ?drgania)
(test (= 0 (str-compare ?drgania "nie")))
=>
(printout t "Czy zauwazono ubytek oleju silnikowego ? " <tak/nie> crlf)
(assert (ubytek (warunek)))
)


(defrule Aut_ubytkinie
(ubytek ?ubytek)
(test (= 0 (str-compare ?ubytek "nie")))
=>
(printout t "Czy zauwazono ubytek plynu z chlodnicy? " <tak/nie> crlf)
(assert (chlod (warunek)))
)

(defrule Aut_chlodnica
(chlod ?chlod)
(test (= 0 (str-compare ?chlod "nie")))
=>
(printout t "Czy wystepuja problemy z rozruchem silnika? " <tak/nie> crlf)
(assert (rozruch (warunek)))
)


(defrule Aut_chlodnicatak
(chlod ?chlod)
(test (= 0 (str-compare ?chlod "tak")))
=>
(printout t "Czy zauwazono zolty nalot pod korkiem wlewu oleju? " <tak/nie> crlf)
(bind ?nalot (warunek))
(if(= 0 (str-compare ?nalot "tak")) then (printout t "Sprawdz uszczelke pod glowica." crlf) 
else (printout t "Sprawdz przewody chlodnicy" crlf))
)

(defrule Rozruch
(rozruch ?rozruch)
(test (= 0 (str-compare ?rozruch "tak")))
=>
(printout t "Sprawdz napiecie akumulatora, podaj wartosc (przyklad: 12.3): " crlf)
(bind ?napiecie (read)) (assert (napiecie ?napiecie))

)

(defrule Rozruch_test
(napiecie ?napiecie)
=>
(akumulator ?napiecie)
)


(defrule Autubytki2
(ubytek ?ubytek)
(test (= 0 (str-compare ?ubytek "tak")))
=>
(printout t "Czy zauwazono plamy pod silnikiem " <tak/nie> crlf)
(assert (plamy (warunek)))
)


(defrule Aut_ubytki2
(plamy ?plamy)
(test (= 0 (str-compare ?plamy "tak")))
=>
(printout t "Sprawdz stan przewodow olejowych" crlf)
)

(defrule Aut_ubytki3
(plamy ?plamy)
(test (= 0 (str-compare ?plamy "nie")))
=>
(printout t "Podaj rodzaj silnika: " <diesel/benzyna> crlf)
(assert (silnik (warunek2)))

(printout t "Podaj cisnienie silnika w bar: " crlf)
(bind ?cisnienie (read)) (assert (cisnienie ?cisnienie))
)

(defrule Aut_kompresja
(silnik ?silnik)(cisnienie ?cisnienie)
=>
(cisnienie ?silnik ?cisnienie)
)

(defrule Aut_bieznik
(opon ?opon)
(test (= 0 (str-compare ?opon "nie")))
=>
(printout t "Sprawdz stan tarcz hamulcowych" crlf)
)

(defrule Aut_bieznik2
(opon ?opon)
(test (= 0 (str-compare ?opon "tak")))
=>
(printout t "Sprawdz wys bieznika ogumienia podaj wartosc w mm (przyklad: 1.5): " crlf) 
(bind ?biez (read)) 
(assert (biez ?biez))
(opony ?biez)
)

(defrule Aut_moc_filtr
  (filtr ?filtr)
  (test (= 0 (str-compare ?filtr "nie")))
=>
(printout t "Nalezy dokonac wymiany filtru paliwa" crlf)
)

(defrule Aut_moc_wtryski
  (filtr ?filtr)
  (test (= 0 (str-compare ?filtr "tak")))
=>
(printout t "Sprawdz uklad wtryskowy, uruchomic kalkulator czasu wtryskow?" <tak/nie> crlf)
(assert (wtrysk (warunek)))
)


(defrule Aut_wtrys
(wtrysk ?wtrysk)
(test (= 0 (str-compare ?wtrysk "tak")))

=>
(printout t "Podaj wartosc RPM" crlf) (bind ?w1 (read)) 
(printout t "Podaj wartosc IPW" crlf) (bind ?w2 (read)) 
(assert (w1 ?w1)(w2 ?w2))
(wtrysk ?w1 ?w2)
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
(if (and (= 0 (str-compare "tak" ?paliwb))(= ?filtrbenz 1)) then (printout t "TOTAL QUARTZ 9000 ENERGY 5W40" crlf))
(if (and (= 0 (str-compare "nie" ?paliwb))(= ?filtrbenz 0)) then (printout t "Czy podstawowe paliwo to Diesel?" crlf))
(bind ?paliwd (warunek)) (assert (paliwd ?paliwd))

(if (and (= 0 (str-compare "nie" ?paliwb))(= ?filtrbenz 1)) then (printout t "Blad systemu - prosze wybrac ponownie" crlf)
(printout t "Czy podstawowe paliwa to benzyna? <tak|nie>:" crlf)
(bind ?paliwb2 (warunek))
(if (= 0 (str-compare "tak" ?paliwb2)) then (printout t "Zastosuj olej Motul 8100 X-Clean C2/C3 FE 5W30 5L" crlf) else (printout t "Sprawdź wersje swojego silnika" crlf)))
)

(defrule Olej_gazdiesl
(paliwd ?paliwd)
(test (= 0 (str-compare ?paliwd "tak")))
=>
(printout t "Czy posiada filtr DPF? <tak|nie>:" crlf) 
(bind ?gazdiesl (warunek))
(if (= 0 (str-compare "tak" ?gazdiesl)) then (printout t "Zastosuj olej FORMULA F 5W30 5W-30 5L" crlf) else 
(printout t "Zastosuj olej VALVOLINE MAXLIFE 5W40" crlf))
)

(defrule Olejdiesel_blad
(paliwd ?paliwd)
(test (= 0 (str-compare ?paliwd "nie")))
=>
(printout t "Blad - sprawdz wersje swojego silnika" crlf) 
)


(defrule Olej_zle
(z ?zle)
=>
(printout t "(Podales zla odpowiedz - wybierz 1, 2 lub 3)" crlf)
(odpowiedz (read))
)


(defrule Koniec
(olej ?olej) (wtrysk ?wtrysk) (rozruch ?rozruch) (olej ?olej)
  (test (or (= 0 (str-compare ?olej "nie")) (= 0 (str-compare ?wtrysk "nie")) (= 0 (str-compare ?rozruch "nie")) (= 0 (str-compare ?olej "nie"))))
=> 
  (printout t "Dziekuje za skorzystanie z programu.")
)
