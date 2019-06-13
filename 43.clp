
(deffunction odpowiedz2 (?o)
(switch ?o
(case 1 then (printout t "Pole prostokata o bokach ?a i ?b wynosi ?y"))
(case 2 then (printout t "Obwod prostokata  ?a i ?b wynosi ?y"))
(case 3 then (printout t "Przekatna prostokata  ?a i ?b wynosi ?y"))
(case 4 then (printout t "?n wyraz ciagu Fibonacciego wynosi ?y "))
(default (bind ?z )(assert (z ?z)))
))

(deffunction odpowiedz (?o)
(switch ?o
(case 1 then pole (?a ?b)
(case 2 then pole (?a ?b)
(case 3 then pole (?a ?b)
(case 4 then fibo (?n)
(default (bind ?z )(assert (z ?z)))
))

(deffunction pole (?a ?b)
(bind ?y (*(?a ?b)))
(assert (y ?y))
)

(deffunction obwod (?a ?b)
(bind ?y (+(*(?a 2))(*(?b 2))))
(assert (y ?y))
)

(deffunction przekatna (?a ?b)
(bind ?y (sqrt(**(?a))(**(?b))))
(assert (y ?y))
)

(deffunction fibo (?n)
(bind ?y (-(*(**(/ (+ 1 (sqrt 5)) 2) ?n)(/ 1 (sqrt 5)))(*(**(/ (- 1 (sqrt 5)) 2) ?n) (/ 1 (sqrt 5)))))
(assert (y ?y))
)
 (defrule start
 (initial-fact)
=>
 (printout t ".-----------------------------------------." crlf)
 (printout t "|                                         |" crlf)
 (printout t "|          Witaj, program oblicza         |" crlf)
 (printout t "|       n-ty wraz ciagu Fibonacciego      |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|         autor: Grzegorz Janik           |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "`-----------------------------------------'" crlf)
 
(printout t "(Obliczam- wybierz 1, 2 lub 3)" crlf)
(printout t "(1. Pole prostokatna)" crlf)
(printout t "(2. Obwod prostokata)" crlf)
(printout t "(3. Przekatna prostokata)" crlf)
(printout t "(4. Ciag fibonacciego)" crlf)
(bind ?odp (read))
(assert (odp ?odp))
)


(defrule R2
(odp ?odp)
=>
(printout t "Wprowadz boki prostokata" crlf)
(bind ?a (read))(bind ?b (read))
(odpowiedz ?odp)
)

 
(defrule R3
(r ?r)
=>
(printout t "Podaj n-ty wyraz ciagu Fibonacciego" crlf)
(bind ?n (read))
(assert (n ?n))
(printout t "Wartosc ciagu Fibonacciego Fn wynosi: " (fibo ?n) crlf))
)

(defrule R4
(y ?y)
=>
(odpowiedz2 odp)
)




