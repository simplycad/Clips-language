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

(deffunction pobierz_tak_nie ()
   (bind ?odp (readline))
   (bind ?odp (lowcase ?odp))
   (while(and (<> 0 (str-compare ?odp "tak")) (<> 0 (str-compare ?odp "nie")))
      (printout t "<tak|nie>: ")
      (bind ?odp (readline))
   )
   (printout t crlf)
   ?odp
)

(deffunction pobierz_tak_nie_nie-wiem ()
   (bind ?odp (readline))
   (bind ?odp (lowcase ?odp))
   (while(and (<> 0 (str-compare ?odp "tak")) (<> 0 (str-compare ?odp "nie")) (<> 0 (str-compare ?odp "nie wiem")))
      (printout t "<tak|nie|nie wiem>: ")
      (bind ?odp (readline))
   )
   (printout t crlf)
   ?odp
)

(deffunction pobierz_plec ()
   (bind ?odp (readline))
   (bind ?odp (lowcase ?odp))
   (while(and (<> 0 (str-compare ?odp "kobieta")) (<> 0 (str-compare ?odp "mezczyzna")))
      (printout t "<kobieta|mezczyzna>: ")
      (bind ?odp (readline))
   )
   (printout t crlf)
   ?odp
)

(defrule wywiad
   (initial-fact)
=>
   (printout t ".-----------------------------------------." crlf)
   (printout t "|                                         |" crlf)
   (printout t "|       Ocena ryzyka zachorowania         |" crlf)
   (printout t "|             na raka piersi              |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|              na podstawie               |" crlf)
   (printout t "|              wystepowania               |" crlf)
   (printout t "|            czynnikow ryzyka             |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|       autor: Wlodzimierz Wysocki        |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "`-----------------------------------------'" crlf)
   (retract  *)
   
   (printout t "Okresl plec pacjenta <kobieta|mezczyzna>: ")
   (assert (plec (pobierz_plec)))
)

(defrule plec_mezczyzna
	(plec "mezczyzna")
=> 
	(assert (ryzyko "bardzo niskie"))
	(printout t "(mezczyzni choruja na raka piersi stukrotnie rzadziej od kobiet)" crlf)
	(assert (wyniki 1))
)

(defrule wywiad_kobieta
	(plec "kobieta")
        ?pid <- (plec ?)
=>
	(retract ?pid)
	(assert (ryzyko "brak"))

	(printout t "wiek pacjentki <0, 100>: ")
;	(assert (wiek (pobierz_liczbe 0 100)))
	(bind ?wiek (pobierz_liczbe 0 100))
	(assert (wiek ?wiek))
	
	(printout t "Czy badana zamieszkuje w miescie? <tak|nie|nie wiem>: ")
	(assert (miasto (pobierz_tak_nie_nie-wiem)))

	(printout t "Podaj wiek, w ktorym wystapila pierwsza miesiaczka <0, " ?wiek ">: ")
	(assert (wiek-miesiaczki (pobierz_liczbe 0 ?wiek)))

	(printout t "Czy badana rodzila? <tak|nie>: ")
	(bind ?czy-rodzila (pobierz_tak_nie))
	(if (= 0 (str-compare ?czy-rodzila "tak")) 
		then
			(printout t "Podaj wiek pierwszego porodu <0, " ?wiek "> : ")
			(assert (wiek-porodu (pobierz_liczbe 0 ?wiek)))
		else
			(assert (nie-rodzila "tak"))
	)

	(printout t "Czy badana przechodzi(la) klimakterium? <tak|nie>: ")
	(bind ?czy-klimakterium (pobierz_tak_nie))
	(if (= 0 (str-compare ?czy-klimakterium "tak")) 
		then
			(printout t "Podaj wiek wystapienia klimakterium <0, " ?wiek "> : ")
			(assert (wiek-klimakterium (pobierz_liczbe 0 ?wiek)))
		else
			(assert (wiek-klimakterium ?wiek))
	)

	(printout t "Czy u badanej wystepuje otylosc 30% i wiecej ponad norme? <tak|nie>: ")
	(assert (otylosc (pobierz_tak_nie)))

	(printout t "Czy badana nadmiernie spozywa alkohol przez dluzszy czas? <tak|nie|nie wiem>: ")
	(assert (alkohol (pobierz_tak_nie_nie-wiem)))

	(printout t "Czy warunki zycia badanej charakteryzuje wysoki stan socjoekonomiczny? <tak|nie|nie wiem>: ")
	(assert (wysoki-stan (pobierz_tak_nie_nie-wiem)))
	
	(printout t "Czy u badanej wystepuja lagodne zmiany w sutku? <tak|nie|nie wiem>: ")
	(assert (lagodne-zmiany (pobierz_tak_nie_nie-wiem)))
	
	(printout t "Czy u najblizszych krewnych badanej wystapil rak sutka? <tak|nie>: ")
	(bind ?czy-krewni (pobierz_tak_nie))
	(if (= 0 (str-compare ?czy-krewni "tak")) then
		(printout t "Podaj liczbe rakow sutka u najblizszych krewnych <0, 5> : ")
		(assert (krewni-najblizsi (pobierz_liczbe 0 5)))
		(printout t "Podaj liczbe rakow sutka u krewnych pierwszego stopnia, ktore zachorowaly przed 50 rokiem zycia <0, 5> : ")
		(assert (krewni-pierwszego (pobierz_liczbe 0 5)))
	)

	(printout t "Czy u badanej nastapilo napromieniowanie sutka z przyczyn innych niz rak sutka? <tak|nie|nie wiem>: ")
	(assert (napromieniowanie (pobierz_tak_nie_nie-wiem)))

	(printout t "Czy u badanej stwierdzono obecnosc charakterystycznych mutacji genu BRCA-1? <tak|nie>: ")
	(assert (BRCA1 (pobierz_tak_nie)))

	(printout t "Czy u badanej wystepuje rak drugiego sutka? <tak|nie>: ")
	(assert (rak-drugiego (pobierz_tak_nie)))
)

(defrule bardzo_wysokie_ryzyko
	(declare (salience 10)) 
	(ryzyko "brak")
	(or 
		(BRCA1 "tak")
		(rak-drugiego "tak")
	)
        ?rid <- (ryzyko ?)
        ?bid <- (BRCA1 ?)
        ?rdid <- (rak-drugiego ?)
=>
	(retract ?rid)
	(retract ?bid)
	(retract ?rdid)
	(assert (ryzyko "bardzo wysokie"))
	(assert (wyniki 1))
)

(defrule wysokie_ryzyko
	(declare (salience 9)) 
	(ryzyko "brak")
	(or 
		(krewni-pierwszego ?krewni-pierwszego&:(> ?krewni-pierwszego 1))
		(krewni-najblizsi ?krewni-najblizsi&:(> ?krewni-najblizsi 2))
	)
        ?rid <- (ryzyko ?)
        ?kpid <- (krewni-pierwszego ?)
        ?knid <- (krewni-najblizsi ?)
=>
	(retract ?rid)
	(retract ?kpid)
	(retract ?knid)
	(assert (ryzyko "wysokie"))
)

(defrule powyzszone_ryzyko
	(declare (salience 8)) 
	(ryzyko "brak")
	(or 
		(napromieniowanie "tak")
		(krewni-najblizsi ?krewni-najblizsi&:(= ?krewni-najblizsi 2))
	)
        ?rid <- (ryzyko ?)
	?nid <- (napromieniowanie ?)
	?knid <- (krewni-najblizsi ?)
=>
	(retract ?rid)
	(retract ?nid)
	(retract ?knid)
	(assert (ryzyko "podwyzszone"))
)

(defrule srednie_ryzyko
	(declare (salience 7)) 
	(ryzyko "brak")
	(or 
		(wysoki-stan "tak")
		(and 
			(nie-rodzila "tak")
			(wiek ?wiek&:(> ?wiek 30))
		)
		(otylosc "tak")
		(lagodne-zmiany "tak")
		(wiek-porodu ?wiek-porodu&:(> ?wiek-porodu 30))
	)
        ?rid <- (ryzyko ?)
	?wsid <- (wysoki-stan ?)
	?nrid <- (nie-rodzila ?)
	?otid <- (otylosc ?)
	?lzid <- (lagodne-zmiany ?)
	?wpid <- (wiek-porodu ?)
=>
	(retract ?rid)
	(retract ?wsid)
	(retract ?nrid)
	(retract ?otid)
	(retract ?lzid)
	(retract ?wpid)
	(printout t crlf "---- srednie-----" crlf)
  	(assert (ryzyko "srednie"))
)

(defrule niskie_ryzyko
	(declare (salience 6)) 
	(ryzyko "brak")
	(or 
		(miasto "tak")
		(wiek-miesiaczki ?wiek-miesiaczki&:(< ?wiek-miesiaczki 12))
		(wiek-klimakterium ?wiek-klimakterium&:(> ?wiek-klimakterium 55))
	)
        ?rid <- (ryzyko ?)
        ?mid <- (miasto ?)
        ?wmid <- (wiek-miesiaczki ?)
        ?wkid <- (wiek-klimakterium ?)
=>
	(retract ?rid)
	(retract ?mid)
	(retract ?wmid)
	(retract ?wkid)
	(printout t crlf "---- niskie-----" crlf)
	(assert (ryzyko "niskie"))
)

(defrule bardzo_niskie_ryzyko
	(declare (salience 5)) 
	(ryzyko "brak")
        ?rid <- (ryzyko ?)
=>	
	(retract ?rid)
	(printout t crlf "---- bardzo niskie-----" crlf)
	(assert (ryzyko "bardzo niskie"))
)

(defrule wiek_spr
	(declare (salience 4)) 
	(wiek ?wiek&:(> ?wiek 55))
	(ryzyko ?ryzyko&~"brak")
=>
	(printout t crlf "---- wiek -----" crlf)
	(switch ?ryzyko
		(case "bardzo niskie" then (assert (ryzyko "niskie")))
		(case "niskie" then (assert (ryzyko "srednie")))
		(case "srednie" then (assert (ryzyko "podwyzszone")))
		(case "podwyzszone" then (assert (ryzyko "wysokie")))
		(case "wysokie" then (assert (ryzyko "bardzo wysokie")))
		(case "brak" then (assert (ryzyko "niskie")))
		(case "bardzo wysokie" then (assert (ryzyko "ekstremalnie wysokie")))
		(default (assert (ryzyko "brak")))
	)
	(assert (wyniki 1))
)


(defrule wiek_spr_nie
	(declare (salience 4)) 
	(ryzyko ?ryzyko&~"brak")
	(wiek ?wiek&:(<= ?wiek 55))
=>
	(printout t crlf "---- NIE wiek -----" crlf)
	(assert (wyniki 1))
)


; wyswietlenie wynikow i pytanie czy badac jeszcze
(defrule wyniki
	(declare (salience 5)) 
	(wyniki ?wyniki)
	(ryzyko ?ryzyko&~"brak")
=>
	(retract *)	;usuniecie wszystkich faktow
	(printout t crlf "Na podstawie przeprowadzonego wywiadu" crlf)
	(printout t "oceniam ryzyko zachorowania na raka piersi jako " ?ryzyko "." crlf crlf)
	(printout t "Czy przeprowadzic kolejne badanie? <tak|nie>: ")
	(bind ?kolejne (pobierz_tak_nie))
	(if (= 0 (str-compare "tak" ?kolejne)) then
		(assert (initial-fact))
	)
)
