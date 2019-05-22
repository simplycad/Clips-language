(defrule start
 (initial-fact)
=>
 (printout t ".-----------------------------------------." crlf)
 (printout t "|                                         |" crlf)
 (printout t "|          Witaj, program oblicza         |" crlf)
 (printout t "|       n-ty wraz ciagu Fibonacciego      |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|         autor: Grzegorz Janik           |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "`-----------------------------------------'" crlf)
 (printout t "Podaj n-ty wyraz ciagu Fibonacciego :")
 (bind ?n (read))
 (assert (n ?n))
 )

(deffunction licz-fib
(?n)
(-(*(**(/ (+ 1 (sqrt 5)) 2) ?n)(/ 1 (sqrt 5)))(*(**(/ (- 1 (sqrt 5)) 2) ?n) (/ 1 (sqrt 5)))) 
)

(defrule oblicz
(n ?n)
=>
(printout t "Wartosc ciagu Fibonacciego Fn wynosi: " (licz-fib ?n) crlf))
