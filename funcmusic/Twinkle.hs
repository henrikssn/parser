module Twinkle where
import Haskore
import AutoComp

fd d n = n d v
vol  n = n   v
v      = [Volume 100]
lmap f l = line (map f l)

v1            = lmap vol  
                [c 5 qn, c 5 qn, g 5 qn, g 5 qn, a 5 qn, a 5 qn, g 5 hn,
                 f 5 qn, f 5 qn, e 5 qn, e 5 qn, d 5 qn, d 5 qn, c 5 hn]
                 
v2            = lmap vol
                [g 5 qn, g 5 qn, f 5 qn, f 5 qn, e 5 qn, e 5 qn, d 5 hn]

twinkleMelody = v1 :+: v2 :+: v2 :+: v1

twinkleChords :: ChordProgression
twinkleChords = map (\(ch,du) -> ((ch,Major),du)) $
                    [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]
                    ++ (concat . (replicate 4)) [(C,hn),(G,hn)] ++
                    [(C,wn),(F,hn),(C,hn),(G,hn),(C,hn),(G,hn),(C,hn)]

--exbass = lmap (fd hn) [c 3, g 2, c 3, g 2, f 3, e 3, d 3, c 3]

twinkle = Instr "piano" (Tempo 2 (Phrase [Dyn SF] $ twinkleMelody :=: autoChord (C,Major) twinkleChords
                                                  :=: autoBass calypso (C,Major) twinkleChords))

