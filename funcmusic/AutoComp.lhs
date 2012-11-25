Functional Music, Linus Åkesson, d00la@efd.lth.se

-----------------------------------------------------------------

        Talking about music is like fishing about architecture.
        -- Frank Zappa


This is an introductory essay on the subject of the harmonics of
modern Western major/minor based music. Starting out with a small
set of basic rules and assumptions, the text will go on
explaining basic notes, sharp and flat notes, note sets,
modulation, keys, scales, chords, chord notation, and finally
present an algorithm which generates rudimentary accompaniment by
interpreting a sequence of chords.

As hinted by Zappa's remark above, our approach to harmonics will
be purely operational. We will deal with notes, rythm and
harmony, the same tools that musicians deal with -- but rather
than use the tools to create something new, we'll simply analyze
and play around with the tools for our personal insight and
enjoyment.


> module AutoComp where
> import Haskore hiding (chord, Key)    -- I want those names.


The starting point - seven basic notes
======================================

In this section, and at a few other places throughout the essay,
assumptions will be made. There is nothing inexplicable, nothing
axiomatic about these assumptions -- in the end everything boils
down to the physical theory of overtones. However, explaining
this is beyond the scope of this text.

In Western music, barring certain exceptions, one works with
seven notes at a time. These notes are usually arranged in what
we call a major scale. As a convenience, names have been given to
seven frequencies, so that these seven frequencies form such a
major scale. The names are different in different countries; in
Germany and Scandinavia, for instance, the names are C, D, E, F,
G, A, H, whereas in France they are do, re, mi, fa, so, la, si.
Since this text is written in English, we'll use the English
naming system:

        C       D       E       F       G       A       B

The notes in the major scale are not equidistant. In fact, there
are two distances (ratios between frequencies, actually): The
tone and the semitone. In the following diagram, t means tone and
s means semitone:

    C       D       E       F       G       A       B       C'
    |<--t-->|<--t-->|<--s-->|<--t-->|<--t-->|<--t-->|<--s-->|

C' is the note whose frequency is twice the frequency of C. In
music speak, you'd say that C' is one octave above C. Two notes
that are an octave apart sound very similar, and for our purposes
we'll regard them as being equal.

The diagram tells us that the product of all the steps (ttsttts)
should be equal to two (since C' = 2C). A common approximation is
to set t = ss, which yields s = 2^(1/12) = 1.05946...

However, t is not exactly equal to ss (i.e. one tone is not equal
to two semitones). Instead, we wish G to be the pure fifth to C,
which is a musician's way of saying that G = 3/2C. This gives us
the following two equations:

        ttsttts = 2
        ttst    = 3/2

The roots are:

        t       = 9/8           = 1.12500
        s       = 256/243       = 1.05350...


Modifying the basic notes
=========================

Let's see what happens when we multiply each of the base
frequencies with ttts:

    C       D       E       F       G       A       B

becomes:

    G       A       B       C'      D'      E'     ???

Since the total distance between C and G is three t's and one s
(G = Cttts), the C becomes a G. Similar relations hold for each
of the basic notes except B, which doesn't become F' (the only
note that's left) since F' = Bttss, not Bttts.

So, when we multiply each of the seven base notes by ttts (music
speak: transpose them one fifth upwards), and then disregard any
octave marks ('), we end up with the same set of notes, except
that the F is missing, and a new note has appeared, which is
actually Ft/s. This note is called F sharp.

Of course, we can perform the same operation on the new set of
notes:

    G       A       B       C       D       E       Ft/s

        becomes

    D       E       Ft/s    G       A       B       Ct/s

and so on. This can go on forever. At one point all of the notes
will be sharp, and then, when we multiply B sharp with ttts, we
end up with F*(t/s)*(t/s), which is called F double-sharp.

The inverse of this operation would be to divide each of the
seven frequencies by ttts. Starting with our initial set of
notes, we would end up with:

    F       G       A       Bs/t    C       D       E

This time, we end up without the B, and with a new note, Bs/t,
called B flat.


Some definitions
================

At this point, we could define a Haskell data type to represent a
note:

> type Note             = (Int, Int)

where the first Int is in the range 0 to 6, and tells us which of
the seven basic notes to start from, and the second Int tells us
how many times this note should be sharpened (if positive) or
flattened (if negative).

We could also define a function, showNote, that converts Note
objects into human readable strings. We'll use the conventional
"#" and "b" suffices to indicate sharp and flat notes
respectively.

> showNote :: Note -> String

> showNote (base, offs) = ("CDEFGAB" !! mod base 7)
>                       : replicate (abs offs)
>                               (if offs > 0 then '#' else 'b')

The set of basic notes would be:

> baseSet               = zip [0..6] [0,0..] :: [Note]


The circle of fifths
====================

The operation introduced above, in which all of the seven
frequencies in our current working set are multiplied or divided
by ttts (3/2), is called modulation. Multiplying by ttts is
called upwards modulation, and dividing by ttts is called
downwards modulation.

Imagine an infinite sequence of note sets:

...
D   E   F#  G   A   B   C#
G   A   B   C   D   E   F#
C   D   E   F   G   A   B      <- starting point
F   G   A   Bb  C   D   E
Bb  C   D   Eb  F   G   A
...

To simplify things, we'll give each note set in this sequence a
name, consisting of the first note in the set, followed by the
word "major". In other words, "C D E F G A B" is the note set of
C major, and "Bb C D Eb F G A" is the note set of B flat major.

To make things complicated, every note set has an additional
name, consisting of the sixth note in the set followed by the
word "minor". So, C major is the same note set as A minor. (NB:
There is also a scale called C major, and a scale called A minor,
and these scales differ. More about this later.)

After six modulations in each direction, we end up with the sets
of F# major and Gb major. F# and Gb would have the same frequency
using the aforementioned t = ss approximation, but in reality
they don't. If we assume that they are equal, however, we can
fold the infinite sequence of note sets into a circle:

                          C maj = a min
            F maj = d min               G maj = e min


    Bb maj = g min                              D maj = b min


 Eb maj = c min                                    A maj = f# min


    Ab maj = f min                              E maj = c# min

            Db maj = bb min             B maj = g# min
                          F# maj = d# min 
                       or Gb maj = eb min

This is called the circle of fifths. Moving clockwise in the
circle corresponds to modulating upwards, and moving
anti-clockwise corresponds to modulating downwards.

In our program, we don't use the t = ss approximation (yet), so
we'll stick to the infinite sequence representation. Let's first
define some useful functions:

> upFifth, dnFifth :: Note -> Note

> upFifth (base, offs)  = (
>                               mod (base + 4) 7,
>                               offs + if base == 6 then 1 else 0)

> dnFifth (base, offs)  = (
>                               mod (base + 3) 7,
>                               offs - if base == 3 then 1 else 0)

The upFifth function will transpose a given note one fifth
upwards, and the dnFifth function will do the opposite. To
modulate a whole set of notes, we simply map these functions to
each element of the set:

> modulateUp, modulateDn :: [Note] -> [Note]

> modulateUp set        = map upFifth set
> modulateDn set        = map dnFifth set

Now we can play around a bit, with the functions defined so far:

        > map showNote baseSet

        ["C","D","E","F","G","A","B"]

        > map showNote (modulateUp baseSet)

        ["G","A","B","C","D","E","F#"]



Keys and scales
===============

Every piece of music that we will deal with has a key. The key
tells us what notes are generally used in the piece. So, if a
piece of music is in G major, we generally stick to the notes
found in the G major note set. If the key of the piece had been E
minor, we would have used the same note set, since G major and E
minor are different names for the same note set. (The key of the
piece carries other information as well, so it would be wrong to
say that a piece is written in G major if indeed it is written in
E minor. For our purposes, however, these keys are indifferent.)

Thus, a key is a set of notes:

> type Key              = [Note]

As we shall see, it's a good idea to form the infinite "circle of
fifths" using the key of the piece as a starting point, instead
of always starting from the set of C major. The following
function builds the "circle" around a given note set,
representing it as an infinite list of pairs:

> type Circle           = [(Key, Key)]

> circle :: Key -> Circle

> circle key            = zip
>                               (iterate modulateUp key)
>                               (iterate modulateDn key)

Example: If n is a note set, then (circle n !! 4) would be a pair
of note sets corresponding to n modulated upwards four times, and
n modulated downwards four times, respectively.

So far we've used the same naming scheme (note name followed by
"major" or "minor") for both keys and note sets. This naming
scheme is also used when naming scales. The difference between a
scale and a set of notes, is that the scale is ordered, starting
with a certain note and going strictly upwards frequency-wise.
For every note set, there are seven scales (starting from each of
the seven notes), of which two are more important to us than the
rest: the major scale and the melodic minor scale.

The major scale is ordered so that the ratios between the
successive notes are:

        t  t  s  t  t  t (s)

The melodic minor scale is ordered so that the ratios between the
successive notes are:

        t  s  t  t  s  t (t)

Notes may be transposed by whole octaves at will, to make the
above predicates hold. The last ratio (in parenthesis) is the
ratio between the last note of the scale and the base note. It is
included merely to illustrate an important fact: The two
sequences above are the same, except that they are out of phase
with each other.

This means, that if we make sure to always keep the note sets in
the same order as the original set (the seven base notes, which,
as you recall, were chosen so that they would form a major
scale), then the set itself would be equal to its corresponding
major scale, and if we were to rotate it two steps to the right,
it would be equal to its corresponding minor scale. Example: The
C major / a minor note set (C D E F G A B) corresponds to the
following scales:

        C major:        C D E F G A B
        a minor:        A B C D E F G

The following function builds a scale from a note set and a note
to start from. When necessary, it will transpose some notes one
octave upwards, by adding 7 to their first Int. This contradicts
what I wrote in the Note type definition, but it turns out to be
of practical value later on.

> set2scale :: Note -> [Note] -> [Note]

> set2scale (base, _) set
>                       = [(scale, o) |
>                               scale <- [base .. base + 6],
>                               (b, o) <- set,
>                               b == mod scale 7]

My implementation of this function assumes that the set contains
seven notes, each of which is based on a different base note.
This holds for any note set formed by modulating the basic C
major note set (baseSet).

Here's an example of how the function may be used (note that some
pairs have base note values larger than 6, and that showNote
knows how to handle this):

        > set2scale (3, 0) baseSet

        [(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),(9,0)]

        > map showNote (set2scale (3, 0) baseSet)

        ["F","G","A","B","C","D","E"]



Chords
======

By picking some notes from a note set, we may form chords. In the
music we're dealing with, mainly two types of chords are used:
The major triad and the minor triad. From a given note set
(ordered as above), the major triad consists of the first, third
and fifth notes, whereas the minor triad consists of the sixth,
first and third notes. The major triad is named after the first
note in the note set, but the minor triad is named after the
sixth note, much like the corresponding minor scale and key.

So, assuming we have a function, "chord subset pos base", which
picks a given subset of notes from the key containing a certain
note at a certain position, the following functions may be
defined:

> major, minor :: Note -> [Note]

> major                 = chord [0, 2, 4] 0
> minor                 = chord [5, 0, 2] 5

(Note that major and minor are functions, accepting the "base"
argument which is missing from the halfway applications of the
chord function.)

What this chord function must do, then, is to find the correct
note set and then pick out and return the wanted notes. To find
the correct note set, we'll use a helper function, to traverse
the infinite circle of fifths in both directions until a note set
matching a given predicate is found. This function is called
"nearest", because if there are several note sets matching the
predicate, the function will only return the one that is closest
to the starting point of the circle.

> nearest :: ([Note] -> Bool) -> Circle -> [Note]

> nearest pred ((sh, fl) : restCircle)
>
>       | pred sh       = sh
>       | pred fl       = fl
>       | otherwise     = nearest pred restCircle

Since the chord predicates (the first or the sixth note of the
set being equal to some given note) won't ever match more than
one note set, it might seem like "nearest" is an odd name for
this function. We will however use this function again in a
little while, and at that point this stay-near behaviour is
crucial.

The chord function looks like this:

> chord :: [Int] -> Int -> Note -> [Note]

> chord subset pos base = map (set !!) subset
>                               where set = nearest
>                                       ((base ==) . (!! pos))
>                                       (circle baseSet)

So, this is what an E flat major and a D double-sharp minor look
like:

        > map showNote (major (2, -1))

        ["Eb","G","Bb"]

        > map showNote (minor (1, 2))

        ["D##","F##","A##"]



Chord notation and accompaniment
================================

The music with which we are concerned consists of

        1) a melody, and
        2) a sequence of chords.

As mentioned, a chord is a set of notes, and the chords in the
chord sequence are supposed to be played along with the melody.
Usually, the chord sequence is written above the melody as a
series of chord mnemonics: A note name followed by "m" represents
the minor chord of that name, whereas a note name all by itself
represents the major chord of that name. Sometimes, lower case is
used to indicate minor chords. In Haskell, the following types
may be used instead, to represent chords:

> type ChordKind        = Note -> [Note]

> type Chord            = (Note, ChordKind)

The "major" and "minor" functions defined earlier are of the
ChordKind type, as they transform a base note into the set of
notes that make out the corresponding chord. 

Every chord in the sequence has a starting point in time, as well
as a duration. In a written score, the starting point is apparent
by the position of the chord mnemonic, and each chord is held
until the next chord mnemonic appears. We will use a different
approach: the duration of each chord is explicitly stated, and
the first chord is struck at the beginning of the piece.

The chord sequence for a piece of music could then be encoded in
the following way:

> type ChordProgression = [(Chord, Ratio Int)]

where the Ratio Int reflects the duration of each chord, measured
in bars.

Our grand task is to write a function, autoComp, which will
interpret a ChordProgression object, generating rudimentary
accompaniment in the form of a Haskore Music object. The
accompaniment will consist of two parts: bass line and chord
voicing. Let us begin with the bass line.


Bass patterns
=============

A bass line, in this exercise, is a series of notes picked using
a certain algorithm. Bass lines are governed by two parameters:
the current chord, and the overall style of the accompaniment.
The chord is transformed into what we'll refer to as a bass
scale, and the style is just a rythmified pattern of indices into
this scale.

Here are a few bass styles:

> type Style            = [(Int, Ratio Int)]

> basic, calypso, boogie :: Style

> basic                 = [(0, 1%2), (4, 1%2)]
> calypso               = [
>                               (-1, 1%4), (0, 1%8), (2, 1%8),
>                               (-1, 1%4), (0, 1%8), (2, 1%8)]
> boogie                = [
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8),
>                               (0, 1%8), (4, 1%8),
>                               (5, 1%8), (4, 1%8)]

Explanation: Every style is a list of bass notes, where every
note consists of an index into the bass scale and a duration. As
a special case, an index of -1 means that no bass note should be
played.

I wish I could tell you that the patterns above are just some
random patterns that happen to produce nice accomponiments.
Instead, I'll say that the patterns above are just some random
patterns.


Obtaining the bass scale
========================

How do we obtain the correct bass scale, given a chord? The
answer is not, as one might first think, that we simply pick the
major or melodic minor scale corresponding to the chord. It's
actually a bit trickier than that, but not much.

This is what we do: Looking at the circle of fifths, we start at
the note set corresponding to the key of the piece. Then, looking
in both directions, we search for the nearest note set, in which
all notes of the chord appear.

        Note that this is a very simple, operational approach,
        which is sufficient for our purposes. It doesn't always
        work; under some circumstances, for instance, when a
        piece of music is in a minor key, the sixth and/or
        seventh note in a note set may be temporarily raised. If
        -- as in one of my example files -- the seventh note is
        raised throughout the whole piece, we can raise it in the
        key set. (It would be the fifth element of the base set,
        since the minor scale is a rotated version of the base
        set.) But generally, we don't pay attention to this in
        our implementation.

        Indeed, any operational musical model must be inadequate,
        since writing good music is all about breaking the rules.

So anyway, if a piece of music is in C major, we stick to the key
note set whenever the following chords appear: C, F, G, am, dm,
em, because these chords only contain notes that can be found in
the C major note set. However, should a D major or B minor chord
appear, we'd be forced to modulate upwards once, and whenever we
get to an E flat major, for instance, we have to modulate
downwards twice.

The note set we end up with is then converted into a scale,
whose starting point is the base note of the chord.

Example: The key is C major. To get the bass pattern
corresponding to a B minor chord, we search the circle of fifths
for a note set containing the notes B, D and F#. There are three
note sets matching this predicate, but G major is the one closest
to C major. The G major note set contains the notes G, A, B, C,
D, E, F#. Since B is the base of the chord, we transform the note
set into a scale, starting with B:

        B, C, D, E, F#, G, A

Expressed as two Haskell functions:

> findChord :: [Note] -> Key -> [Note]

> findChord chord key   = nearest inSet (circle key)
>                               where
>                         inSet set = all (flip elem set) chord

> bassScale :: Chord -> Key -> [Note]

> bassScale (base, kind) key
>                       = set2scale base
>                               (findChord (kind base) key)

And this is what it looks like in action. Note that the bass
scale corresponding to a B minor chord is different from the one
corresponding to a C flat minor chord -- the C and the Db, as
well as the G and the Ab, are entirely different notes! This is a
very tangible example of the shortcomings of the t = ss
approximation.

        > bassScale ((6, 0), minor) baseSet

        [(6,0),(7,0),(8,0),(9,0),(10,1),(11,0),(12,0)]

        > map showNote $ bassScale ((6, 0), minor) baseSet

        ["B","C","D","E","F#","G","A"]

        > map showNote $ bassScale ((0, -1), minor) baseSet

        ["Cb","Db","Ebb","Fb","Gb","Ab","Bbb"]


Generating the bass line
========================

So, with a style, a chord progression, and information about the
key of the piece, we should now be able to generate a bass line.

Haskore uses a different note representation scheme, so we'll
need a routine to convert our Note objects into integers, where
the integers describe distances (in semitones) from the lowest
possible note in Haskore.

The list of integers used in this function (see below) is of
course a major scale, expressed as semitone distances from the
base of the scale. Hence, this is where we have to fall back to
the t = ss approximation.

> semitonify :: Note -> Int

> semitonify (b, o)     = [0, 2, 4, 5, 7, 9, 11] !! mod b 7
>                       + 12 * quot b 7 + o + 36

Likewise, we need a wrapper routine to carry out all the Haskore
specific stuff that needs to be done to the base line. There is
one important algorithmical detail in the following routine,
though: The style pattern is repeated ad infinitum.

> autoBass :: Style -> Key -> ChordProgression -> Music

> autoBass style key prog
>                       = foldr1 (:+:) $ map toHask (bassLine
>                               key
>                               (cycle style)
>                               prog)
>                       where
>                               toHask ((-1, 0), dur) = Rest dur
>                               toHask (note, dur)    = Note
>                                       (pitch (semitonify note))
>                                       dur
>                                       []

The actual workhorse function is called bassLine, and it operates
by zipping together the style with the chord progression. Looking
at the duration of the first element of the style pattern, as
well as the duration of the first chord, it generates a note with
the shortest of these durations. Then it applies itself
recursively to whatever's left in the style and chord lists,
shortening down note durations as necessary.

> bassLine :: Key -> Style -> ChordProgression ->
>                                       [(Note, Ratio Int)]

> bassLine _ _ []       = []

> bassLine
>       key
>       style@((index, sdur):srest)
>       chords@((chord, cdur):crest)
> 
>               | sdur == 0
>                       = bassLine key srest chords
> 
>               | cdur == 0
>                       = bassLine key style crest
> 
>               | otherwise
>                       = play dur : bassLine key
>                               ((index, sdur - dur):srest)
>                               ((chord, cdur - dur):crest)
> 
>       where
>               dur             = min cdur sdur
>               play dur        = if index == -1
>                       then ((-1, 0), dur)
>                       else (bassScale chord key !! index, dur)


Chord voicing
=============

To add some harmonic content to the, so far, very sparse
accompaniment we've generated, we'll introduce chord voicing. The
idea is simply to play the notes of the chords, but in doing so,
we're free to arrange the notes in any order we wish, and we may
transpose the notes any number of octaves in any direction. Some
configurations will sound more traditional (I won't say better)
than others, and the following rules of thumb may be used to that
effect:

* Keep all chord notes within the range 52 to 67 (as returned by
  the semitonify function).

* Keep the notes of every chord close to each other.

* When a new chord is to be played, the sum of the absolute
  changes (in semitones) of the top, middle and bottom note,
  compared to the last chord played, should be minimized.

Apparently, this time we're better off using the semitone values
than the Note objects, so the first thing we do is convert every
note in the chord:

> resolveChord :: [Note] -> [Int]

> resolveChord chord    = map semitonify chord

Next, we generate every possible chord whose notes are one or two
octaves higher than the corresponding note in the original chord:

> octaveCombinations :: [Int] -> [[Int]]

> octaveCombinations [] = [[]]

> octaveCombinations (c:rChord)
>                       = (++)
>                               (map ((c + 12) :) rest)
>                               (map ((c + 24) :) rest)
>                       where rest = octaveCombinations rChord

We can immediately get rid of any chord containing out-of-bounds
notes:

> validCombinations :: [Int] -> [[Int]]

> validCombinations rChord
>                       = [ c | c <- octaveCombinations rChord,
>                               all (>= 52) c,
>                               all (<= 67) c]

Then, we need a sorting routine to sort the notes of the
remaining chords. Here's a rather crude algorithm:

> sort :: [Int] -> [Int]

> sort []               = []

> sort list             = m: sort [ l | l <- list, l > m ]
>                               where m = minimum list

All of the above, applied to a chord:

> combinations :: [Note] -> [[Int]]

> combinations chord
>                       = map sort (validCombinations
>                               (resolveChord chord))

We end up with a list of chords (usually, at this point, there
are only two to four chords left to choose from), and using the
two latter rules in the rule set, we can put a disobediance score
on each chord. We then pick the chord with the minimum score, for
use in our chord voicing.


Calculating the disobedionce score of a chord
=============================================

First, every chord gets a static score, stemming from the
internal closeness of the notes:

> staticScore :: [Int] -> Int

> staticScore chord     = maximum chord - minimum chord

Then, every chord gets a dynamic disobedience score, being the
total semitone difference between each note in the preceding
chord and their corresponding notes in the current one:

> dynamicScore :: [Int] -> [Int] -> Int

> dynamicScore c1 c2    = sum $ map abs $ zipWith (-) c1 c2

The first chord in a piece doesn't have a predecessor, so given a
set of candidates, the following function returns the chord -- as
a list of Haskore semitone pitches -- to use, given a chord
expressed as a set of Note objects:

> firstChord :: [Note] -> [Int]

> firstChord chord      = minimize staticScore
>                               (combinations chord)

The remaining chords have predecessors, so their score is a
combination of the static and dynamic scores. I've chosen
non-weighted addition as the combination method.

> nextChord :: [Int] -> [Note] -> [Int]

> nextChord pre chord   = minimize score (combinations chord)
>                               where score c = (+)
>                                       (staticScore c)
>                                       (dynamicScore pre c)

Both these functions make use of a helper function, minimize,
which returns the least element of a list, where lessness is
judged by a caller-supplied function.

> minimize :: ([Int] -> Int) -> [[Int]] -> [Int]

> minimize func list    = fst $ head [ e | e <- doubleList,
>                               snd e == minimum
>                                       (map snd doubleList)]
>                               where doubleList = zip list
>                                       (map func list)


Generating the chord voicing
============================

Given a chord progression, we can use the functions defined in
the previous section to generate a list of (chord, duration)
pairs (where each chord is a list of semitone values). The
following functions will do that:

> voicing :: ChordProgression -> [([Int], Ratio Int)]

> voicing ((chord, dur):prog)
>                       = (first, dur) : nextVoicing first prog
>                               where
>                               first = firstChord (kind base)
>                               (base, kind) = chord

> nextVoicing :: [Int] -> ChordProgression ->
>                                       [([Int], Ratio Int)]

> nextVoicing _ []      = []

> nextVoicing pre ((chord, dur):prog)
>                       = (next, dur) : nextVoicing next prog
>                               where
>                               next = nextChord pre (kind base)
>                               (base, kind) = chord

Wrapping it all up in Haskore data types:

> autoChord :: ChordProgression -> Music

> autoChord prog        = foldr1 (:+:)
>                               (map toHask (voicing prog))
>                               where toHask (chord, dur) =
>                                       foldr1 (:=:)
>                                               (map note chord)
>                                       where note n = Note
>                                               (pitch n) dur []


I rest my case...
=================

Here's a single function to invoke all the others:

> autoComp melody chords key style
>                       = foldr1 (:=:) [
>                               (Instr "Lead 1 (square)" melody),
>                               (Instr "Acoustic Bass" bass),
>                               (Instr "Overdriven Guitar" comp)]
>                       where
>                               bass = autoBass style key chords
>                               comp = autoChord chords

To conclude this essay, let me reiterate that the operational
model presented here is nothing but a very crude description of a
harmonical system used in some music. The audible results
produced by this program are downright awful, and it would strike
me as very odd if anyone who's listened to them would actually
want to hear them again. And, as mentioned, no operational model,
no set of rules, can ever describe music, because if there were
such a set of rules, many composers would deliberately go ahead
and break them.

But you can't break the rules if you don't know them, and to know
the rules it is best to understand why they are there. In this
text, I've tried to explain a few basic concepts about Western
major/minor based music. It has been my intention to make as few
assumptions as possible, and to always explain new concepts in
terms of what has been defined earlier in the text. Hopefully,
you now have a general understanding of sharp and flat notes,
note sets, modulation and keys, major and minor, scales, chords,
and quite a few other harmonical ideas. I've also shown how to
use these ideas to generate automatic accompaniment, as a way of
putting this theory to the test.


-----------------------------------------------------------------
15 Aug 2002
