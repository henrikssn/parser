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


Building this module
--------------------

We use an ancient version of Haskore which does not link with the Haskell 2010
libraries. This is complicated as the campus machines run different versions of
GHC. Namely, the machine at [login.student.lth.se] runs the older 6.12.3, but
the authors have encountered versions as recent as 7.4... in some University
buildings. Parts of "haskore-vintage" will fail to terminate on recent Haskells
even if the imports are updated, so the 6.12.3 version is necessary to compile.


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

For our purposes, a chord is determined by a pitch-class and a mode. e.g.,
(C, Major) :: Chord. Many more chords are possible but we will restrict 
ourselves to these two fundamental chords.

> type Chord = (PitchClass, Mode)
> type Key = Chord
> type ChordProgression = [(Chord, Dur)]
>

The autoChord function puts a chorded accompaniment to a song, given its 
chord progresssion. The key is not used, since we've chosen to specify the
chord fully in the ChordProgression type. Because nextChord needs to know 
how the previous chord looked, it's hard to get around explicit recursion.

> autoChord :: Key -> ChordProgression -> Music
> autoChord _ = line . createChord []
>     where createChord :: [Pitch] -> ChordProgression -> [Music]
>           createChord _     []              = []
>           createChord prevc ((cho, dur):cs) = let thisc = nextChord prevc cho
>                                               in  foldr1 (:=:) (map (durate dur) thisc) 
>                                                   : createChord thisc cs
>               where durate :: Dur -> Pitch -> Music
>                     durate dur pitch = Note pitch dur voc

The nextChord function decides how to finger the next chord, given how the
previous chord looked. It tries to put down a good fingering that is as close
to the previous chord as possible. We do this by generating all the possible
fingerings and minimizing the distance, brute force style. There are also some
bounds on how high and low notes we allow in the chord part

> lowerBound  = absPitch (E, 4)
> higherBound = absPitch (G, 5)
>
> nextChord :: [Pitch] -> Chord -> [Pitch]
> nextChord pc nc = foldr1 minimize $ chordPerms nc
>     where minimize cc acc | chordDist pc cc < chordDist pc acc  = cc
>                           | otherwise = acc
>
>
> chordPerms :: Chord -> [[Pitch]]
> chordPerms (pc, md) = filter filt $ perms ch
>     where ch = [(sc !! 0,1), (sc !! 2,1), (sc !! 4,1)]
>           sc = scale (pc,md)
>           filt (a:_) = absPitch a >= lowerBound
>
> perms :: [Pitch] -> [[Pitch]]
> perms p = p : perms' p
>     where
>         perms' ((ptc, oct):ps) 
>          | absPitch (ptc, oct + 1) < higherBound     = thisPerm : perms' thisPerm
>          | otherwise = []
>              where thisPerm = ps ++ [(ptc, oct + 1)] 
>
> chordDist :: [Pitch] -> [Pitch] -> Int
> chordDist pc nc = sum . (map diff) $ zip pc nc
>       where diff :: (Pitch, Pitch) -> Int
>             diff (p1, p2) = abs $ location p2 - location p1
> 
> location :: Pitch -> Int
> location (pcl, oct) = pitchClass pcl + (12 * oct)

Bass styles
-----------

This exercise will be restricted to three fundamental bass styles: "simple", 
"calypso", and "boogie". We also add an improvised "house" bass of the kind 
found in modern electronic music.

> type BassStyle = [(Int, Dur)]
> basic, calypso, boogie, house :: BassStyle
> basic                 = [(0, hn),  (4, hn)]
> calypso               = [(-1, qn), (0, en), (2, en),
>                          (-1, qn), (0, en), (2, en)]
> boogie                = [(0, en),  (4, en),
>                          (5, en),  (4, en)]
> house                 = [(-1,en),  (0,en)]
>
> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key = line . bassGen (cycle style) key
>
> bassGen :: BassStyle -> Key -> ChordProgression -> [Music] 
> bassGen _ _ [] = []
> bassGen style@((offset,sdur):srest) key chords@((chord,cdur):crest)
>   | sdur == 0 = bassGen srest key chords
>   | cdur == 0 = bassGen style key crest
>   | otherwise = toMusic (app dur) : (bassGen ((offset, sdur - dur) : srest) 
>                                              key 
>                                              ((chord, cdur - dur) : crest))
>                 where dur = min sdur cdur
>                       app dur = (((scale chord) !! offset, snd chord), dur)
>                       toMusic (ch,du)
>                           | offset == -1 = Rest du
>                           | offset < 3 = Note (calcPitch (fst ch,3)) du vob
>                           | otherwise = Note (calcPitch (fst ch,4)) du vob
>                       calcPitch p@(pc,pch)
>                           | absPitch p < 36 = calcPitch (pc,pch+1)
>                           | absPitch p > 55 = calcPitch (pc,pch-1)
>                           | otherwise       = p

Scales
------

We use the major and minor fundamental scales in this exercise.

> major :: PitchClass -> [PitchClass]
> major pc = map (\x -> fst $ trans x (pc,5)) [0,2,4,5,7,9,11]
>
> minor :: PitchClass -> [PitchClass]
> minor pc = map (\x -> fst $ trans x (pc,5)) [0,2,3,5,7,9,11]
>
> scale :: Chord -> [PitchClass]
> scale ch
>   | snd ch == Major = major $ fst ch
>   | otherwise = minor $ fst ch

The example songs
-----------------

The mandatory example song is "Twinkle, twinkle, litte star". In addition
we have chosen the song "We'll be coming back" by Calvin Harris, an electronic 
song which is currently popular in dance clubs around Lund.

The songs are transcribed into separate modules, as Twinkle.hs and Coming.hs.


Volume
------

The volume specifies how loud an instrument should be. We have adjusted the 
volumes to sound well with the given sample music.

In real music, a different volume also suggests a different tonal quality but 
this is not so in the simple kind of electronic music we are concerned with.

> vob = [Volume 80]
> voc = [Volume 80]


Accidentals and when to break the rules
---------------------------------------

The rules given above are only valid for the simplest kinds of music. In fact,
much of modern music is devoted to different ways of breaking these rules. In
some cases it can be more interesting and satsifying to deliberately play a
note out of key; such notes are called ACCIDENTALS. When the melody plays a
note which is inconsistent with the chord being played, they are said to be in
DISSONANCE. Dissonance is a powerful device to use when composing music, but it
takes much practice to grasp it fully.

