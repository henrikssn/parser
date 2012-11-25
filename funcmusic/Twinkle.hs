module Twinkle where
import Haskore

vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

v1 = lmap vol  
    [c 5 qn, c 5 qn, g 5 qn, g 5 qn, a 5 qn, a 5 qn, g 5 hn,
     f 5 qn, f 5 qn, e 5 qn, e 5 qn, d 5 qn, d 5 qn, c 5 hn]

twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] v1))
