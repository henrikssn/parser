> module AutoComp where
> import Haskore hiding (Key)

------------------------------------------------------------------------------

                              Functional Music 

                                     by

                     Johan Förberg and Erik Henriksson
                          F10               π10
                                
------------------------------------------------------------------------------                             


Abstract
--------

A system for automatically generating simple bass lines given a key and a chord
progression, implemented in literate Haskell using the Haskore music library.


Chords and keys
---------------

In this exercise, we will restrict ourselves to the simplest possible CHORDS,
the majors and minors. A MAJOR CHORD consists of three notes; the first, the
third and the fifth. These are chosen from the BASE SCALE.  In the case of C
major, the scale is

  C  D  E  F  G  A  B  (C)  
  1  2  3  4  5  6  7  (8)

So, to play a C major chord, one would select the notes C, E, and G.  The MINOR
CHORD is obtained by diminishing the third, i.e. lowering it by a semitone.
Thus, a C minor chord consists of C, Eb and G.

In tonal music, one often talks about the dominant chord or KEY of a song. The
key is the "overall chord" of a song. In simple terms, the key is equivalent to
the opening chord of a song, but this may not be true in complicated music.

In simple music, it is expected that the chord notes be chosen from the scale
of the key. Thus, it would not be "correct" to play A major in the key of C
major, since A major == (A, C#, E) and C# is not in the scale of C major. This
can be remedied by lowering C# -> C, giving the chord A minor.  It turns out
that A minor is a much more pleasing chord for simple music in the key of C
major.


Bass styles
-----------

This exercise will be restricted to three fundamental bass styles: "simple", 
"calypso", and "boogie".

> type BassStyle = [(Int, Dur)]
> basic, calypso, boogie :: BassStyle
> basic                 = [(0, hn), (4, hn)]
> calypso               = [(-1, qn), (0, en), (2, en),
>                          (-1, qn), (0, en), (2, en)]
> boogie                = [(0, en), (4, en),
>                          (5, en), (4, en),
>                          (0, en), (4, en),
>                          (5, en), (4, en)]



autoBass :: BassStyle -> Key -> ChordProgression -> Music

> major :: PitchClass -> [PitchClass]
> major pc = map (\x -> fst $ trans x (pc,5)) [0,2,4,5,7,9,11]

> minor :: PitchClass -> [PitchClass]
> minor pc = map (\x -> fst $ trans x (pc,5)) [0,2,3,5,7,9,11]

> type Chord = (PitchClass, Mode)

For our purposes, a chord is determined by a pitch-class and a mode. e.g.,
(C, Major) :: Chord.

> type Key = Chord
> type ChordProgression = [(Chord, Dur)]
> autoChord :: Key -> ChordProgression -> Music
> autoChord k = line . (map createChord)
>                  where createChord (cho,du) = chord [Note (fst cho,4) du v, 
>                                                      Note (trans 4 (fst cho,4)) du v, 
>                                                      Note (trans 7 (fst cho,4)) du v]
>                        v = [Volume 80]
> -- durackord:  grundtonen + 4 semiton + 3 semiton
> -- mollackord: grundtonen + 3 semiton + 4 semiton

Accidentals and when to break the rules
---------------------------------------

The rules given above are only valid for the simplest kinds of music. In fact,
much of modern music is devoted to different ways of breaking these rules.  In
some cases it can be more interesting and satsifying to deliberately play a
note out of key; such notes are called ACCIDENTALS. When the melody plays a
note which is inconsistent with the chord being played, it is said to be in
DISSONANCE. Dissonance is a powerful device to use when composing music, but it
takes much practice to grasp it fully.

