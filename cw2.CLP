(defrule start
(initial-fact)
=>

   (printout t ".-----------------------------------------." crlf)
   (printout t "|                                         |" crlf)
   (printout t "|        Witaj, program rozwiazuje        |" crlf)
   (printout t "|         nastepujacy uklad rownan        |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|              {a1x+b1y=c1                |" crlf)
   (printout t "|              {a2x+b2y=c2                |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "|         autor: Grzegorz Janik           |" crlf)
   (printout t "|                                         |" crlf)
   (printout t "`-----------------------------------------'" crlf)

(printout t "Podaj a1 : ")
(bind ?a1 (read))
(printout t "Podaj b1 : ")
(bind ?b1 (read))
(printout t "Podaj a2 : ")
(bind ?a2 (read))
(printout t "Podaj b2 : ")
(bind ?b2 (read))
(printout t "Podaj c1 : ")
(bind ?c1 (read))
(printout t "Podaj c2 : ")
(bind ?c2 (read))
(assert (a1 ?a1)(b1 ?b1)(a2 ?a2)(b2 ?b2)(c1 ?c1)(c2 ?c2))
)

(defrule licz-wyznacznik
(a1 ?a1)
(b1 ?b1)
(a2 ?a2)
(b2 ?b2)
(c1 ?c1)
(c2 ?c2)
=>
(bind ?w (- (* ?a1 ?b2)(* ?b1 ?a2)))
(assert (w ?w))
(bind ?w1 (- (* ?c1 ?b2)(* ?c2 ?b1)))
(assert (w1 ?w1))
(bind ?w2 (- (* ?a1 ?c2)(* ?a2 ?c1)))
(assert (w2 ?w2))
)


(defrule licz
(w ?w)
(w1 ?w1)
(w2 ?w2)
=>

(if (not (and (= ?w 0)(= ?w1 ?w2))) then


(bind ?x (/ ?w1 ?w))
(assert (x ?x))
(bind ?y (/ ?w2 ?w))
(assert (y ?y))

(printout t "x wynosi = " ?x crlf)
(printout t "y wynosi = " ?y crlf)
    
else(printout t "Uklad ten ma nieskonczenie wiele rozwiazan." crlf)
))


