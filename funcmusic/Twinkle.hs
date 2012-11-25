module Twinkle where
import Haskore

fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

v1 = lmap vol  
    [c 5 qn, c 5 qn, g 5 qn, g 5 qn, a 5 qn, a 5 qn, g 5 hn,
     f 5 qn, f 5 qn, e 5 qn, e 5 qn, d 5 qn, d 5 qn, c 5 hn]

--exbass = lmap (fd hn) [c 3, g 2, c 3, g 2, f 3, e 3, d 3, c 3]

vv = v1 --:=: exbass

twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] vv))

