module Coming where
import Haskore
import AutoComp

fd d n = n d v
vol  n = n   v
v      = [Volume 100]
lmap f l = line (map f l)

comingMelody = lmap vol  
                [f 4 qn, g 4 qn, a 4 qn, c 5 qn, 
                 g 4 qn, a 4 qn, b 4 qn, c 5 en, d 5 en,
                 a 4 qn, b 4 qn, c 5 qn, e 5 qn,
                 a 4 qn, c 5 qn, f 5 en, e 5 en, d 5 en, c 5 en]

comingChords = [((F, Major), wn), ((G, Major), wn), ((A, Minor), wn), ((D, Major), hn),
                                                                      ((D, Minor), hn)]

comingSong = comingMelody :=: (autoChord (F, Major) comingChords) :=: (autoBass house (F,Major) comingChords)

coming = Instr "piano" (Tempo 2 (Phrase [Dyn SF] comingSong))

