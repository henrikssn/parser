> module AutoComp where
> import Haskore hiding (chord, Key)

Chords and keys
---------------

In this exercise, we will restrict ourselves to the simplest possible 
chords, the majors and minors. A major chord consists of three notes; 
the first, the third and the fifth. These are chosen from the base scale. 
In the case of C major, the scale is

  C  D  E  F  G  A  B  (C)  
  1  2  3  4  5  6  7  (8)

So, to play a C major chord, one would select the notes C, E, and G. 
The minor chord is obtained by diminishing the third, i.e. lowering it 
by a semitone. Thus, a C minor chord consists of C, Eb and G.

In tonal music, one often talks about the key of a song. The key is the 
"overall chord" of a song. In simple terms, the key is equivalent to the 
opening chord of a song, but this may not be true in complicated music.

In simple music, it is expected that the chord notes be chosen from the 
scale of the key. Thus, it would not be "correct" to play A major in the 
key of C major, since A major == (A, C#, E) and C# is not in the scale of
C major. This can be remedied by lowering C# -> C, giving the chord A minor.
It turns out that A minor is a much more pleasing chord for simple music in 
the key of C major.

Accidentals and when to break the rules
---------------------------------------

The rules given above are only valid for the simplest kinds of music. In fact,
much of modern music is devoted to different ways of breaking these rules.
In some cases it can be more interesting and satsifying to deliberately 
play a note out of key; such notes are called accidentals. When the melody
plays a note which is inconsistent with the chord being played, it is called
dissonance. Dissonance is a powerful device to use when composing music, but 
it takes much practice to grasp it fully.



autoBass :: BassStyle -> Key -> ChordProgression -> Music

autoChord :: Key -> ChordProgression -> Music


