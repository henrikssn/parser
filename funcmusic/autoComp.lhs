> import Haskore
> import Ratio

> type BassStyle = [(Int, Ratio Int)]
> basic, calypso, boogie :: BassStyle
> basic                 = [(0, 1%2), (4, 1%2)]
> calypso               = [(-1, 1%4), (0, 1%8), (2, 1%8),
>                          (-1, 1%4), (0, 1%8), (2, 1%8)]
> boogie                = [(0, 1%8), (4, 1%8),
>                          (5, 1%8), (4, 1%8),
>                          (0, 1%8), (4, 1%8),
>                          (5, 1%8), (4, 1%8)]



autoBass :: BassStyle -> Key -> ChordProgression -> Music

autoChord :: Key -> ChordProgression -> Music


