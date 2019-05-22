(defrule start
 (initial-fact)
=>
 (printout t ".-----------------------------------------." crlf)
 (printout t "|                                         |" crlf)
 (printout t "|        Witaj, program rozwiazuje        |" crlf)
 (printout t "|       nastepujacy ciag geometryczny     |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|           Sn=a1*(1-q^n)/(1-q)           |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "|         autor: Grzegorz Janik           |" crlf)
 (printout t "|                                         |" crlf)
 (printout t "`-----------------------------------------'" crlf)
 (printout t "Podaj pierwszy element ciagu a1 :")
 (bind ?a1 (read))
 (printout t "Podaj drugi element ciagu a2 :")
 (bind ?a2 (read))
 (printout t "Podaj liczbe elementow ciagu n :")
 (bind ?n (read))
 (assert (a1 ?a1)(a2 ?a2)(n ?n))
 )

(defrule przypis
(a1 ?a1)(a2 ?a2)(n ?n)
=>
(bind ?q (/ ?a2 ?a1)) 
(bind ?q1 (** ?q ?n))
(assert (q ?q)(q1 ?q1))
)

(deffunction licz-ciag
(?a1 ?a2 ?n ?q ?q1)
(/(* ?a1(- 1 ?q1))(- 1 ?q))
)

(defrule oblicz
 (a1 ?a1)(a2 ?a2)(n ?n)(q ?q)(q1 ?q1)
=>
(printout t "Wartosc ciagu wynosi Ns = "(licz-ciag ?a1 ?a2 ?n ?q ?q1) crlf))
