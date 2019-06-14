(deffunction pole (?a ?b)
(printout t "Pole prostokata o bokach a = "?a" i b = "?b" wynosi "(* ?a ?b) crlf))
 
(deffunction obwod (?a ?b)
(printout t "Obwod prostokata o bokach a = "?a" i b = "?b" wynosi "(+ (* 2 ?a) (* 2 ?b)) crlf))
 
(deffunction przekatna (?a ?b)
(printout t "Przekatna prostokata o bokach a = "?a" i b = "?b" wynosi "(sqrt (+ (** ?a 2) (** ?b 2))) crlf))
 
(deffunction fibo (?n)
  (printout t ?n" wyraz ciagu Fibonacciego wynosi "(-(*(**(/ (+ 1 (sqrt 5)) 2) ?n)(/ 1 (sqrt 5)))(*(**(/ (- 1 (sqrt 5)) 2) ?n) (/ 1 (sqrt 5)))) crlf )
)
 
(defrule start
(initial-fact)
=>
(printout t "Obliczam:" crlf)
(printout t "(1. Pole prostokatna)" crlf)
(printout t "(2. Obwod prostokata)" crlf)
(printout t "(3. Przekatna prostokata)" crlf)
(printout t "(4. Ciag fibonacciego)" crlf)
(bind ?odp (readline))
  
(while(and (<> 0 (str-compare ?odp "1"))
 (<> 0 (str-compare ?odp "2")) 
 (<> 0 (str-compare ?odp "3")) 
 (<> 0 (str-compare ?odp "4")))
    (printout t "Wybierz <1 | 2 | 3 | 4>: ")
    (bind ?odp (readline)))

  (assert (odp ?odp))
)
 
(defrule R2
  (odp ?odp)
  (test (or(= 0 (str-compare ?odp "1")) (= 0 (str-compare ?odp "2")) (= 0 (str-compare ?odp "3"))))
=>
  (printout t "Podaj boki prostokata" crlf)
  (bind ?a (read)) (bind ?b (read))
  
  (switch ?odp
    (case "1" then (bind ?wynik (pole ?a ?b)))
    (case "2" then (bind ?wynik (obwod ?a ?b)))
    (case "3" then (bind ?wynik (przekatna ?a ?b)))
  )
  
(assert (wynik ?wynik))
)
 
(defrule R3
  (odp ?odp)
  (test (= 0 (str-compare ?odp "4")))
=>
  (printout t "Podaj n-ty wyraz ciagu Fibonacciego: ")
  (bind ?n (read))
  (bind ?wynik (fibo ?n))
  (assert (wynik ?wynik))
)
 
(defrule R4
  (wynik ?wynik)
=>
  ?wynik
)