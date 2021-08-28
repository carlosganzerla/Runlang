# Runlang

Runlang is a DSL to generate running workouts that can be exported to devices such as Garmin. It ships with a command line tool that generates the workout program from an input string. Currently the tool supports only exporting to [Fit](https://developer.garmin.com/fit/overview/) files, which are supported by Garmin devices.

## The language

Runlang defines a workout definition language that is succint and simple to understand. It's based on the template used by the coaching staf of [G5 Esportes](https://g5esportes.com/). Currently, it only supports metric system units (meters, kilometers and minutes/km). The template that the language was based also define *running terms*. Each term corresponds to a degree on a scale of the [Subjective Effort Perception](), and is. The terms also have an approximate correspondence with a heart zone and a pace for a given running event. The terms are defined as:

- CL: Light walk (*Caminhada leve*). Heart zone 1 and pace should be lighter than an average walk.
- CA: Walking normally (*Caminhada*). Heart zone 1 and should feel like a normal walk.
- CV: Vigorous walk (*Caminhada vigorosa*). Heart zone 1 and should feel like a fast walk.
- TR: Lightest pace (*Trote*). Heart zone 1, and should be the "natural" pace when you start running.
- LVS: Very light pace (*Levíssimo*). Heart zone 1 or 2, and should be very light, just a little above your "natural" pace
- LE: Light pace (*Leve*). Heart zone 2 or 3, should feel tiring if sustained for very long, but should take much effort.
- MO: Moderate pace (*Moderado*). Heart zone 3 and should feel moderate. Usually you would run up to a half marathon at this pace.
- FO: Strong pace (*Forte*). Heart zone 3 or 4, should feel tiring. Usually you would run up to 10km at this pace.
- FTS: Very strong pace (*Fortíssimo*). Heart zone 4 or 5, should feel very difficult. You would run up to 3km-5km at this pace.
- MAX: Maximum pace (*Máximo*). Heart zone 5, and should be fiendishly difficult. At this pace you should be able to run at best 1km.

**These are approximate correspondences based on my personal experience and may not an absolute truth. The use of the terms is overall subject, given it's based on the Subjective Effort Perception. Take the 3000m test and talk to your coach or look up some articles how to calculate this**

The syntax is based on steps that are composed with addtion to compose a workout. Any composition of steps can be repeated a number of times. The workout AST is called workout tree.

Here are some examples of valid workout strings:

- Simple workout, absolute measures:
  ```
  5km 6:00/km
  ```
  It reads as 5 kilometers, at a pace of 6 minutes per kilometer
  
- Another simple workout, defining a step with time and pace, which is equivalent to the above:
  ```
  30min 6:00/km
  ```
- A composed workout of multiple steps, using terms
  ```
  1.5km TR + 1.5km LE + 1.5km FO + 1.5km LVS
  ```
- A workout of 400m repeats, with warmup and cooldown
  ```
  1km TR / 10x(400m FTS / 100m CL) / 1km LE
  ```
  
  For more examples see the [unit tests](https://github.com/carlosganzerla/Runlang/blob/main/src/Runlang.Tests/Parser/LangParserTests.fs) for the `LangParser` module. 
  
### Complete Grammar (BNF)

```bnf
<white_char> ::= " " | "\t" | "\r" | "\n"
<white> ::= <white_char>*
<white1> ::= <white_char>+
<zero_to_five> ::= "0" | "1" | "2" | "3" | "4" | "5"
<six_to_nine> ::= "6" | "7" | "8" | "9"

<digit> ::= <zero_to_five> | <six_to_nine>

<watch_digits> ::= <zero_to_five> <digit>
<base_sixty> ::= <watch_digits> | <digit>

<integer> ::= <digit> | <digit> <integer>

<decimal_separator> ::= "," | "."

<decimal> ::= <integer> <decimal_separator> <digit> | <integer>

<distance> ::= <integer> "m" | <decimal> "km"

<watch_time> ::= <base_sixty> ":" <watch_digits>
  | <integer> ":" <watch_digits> ":" <watch_digits>

<time> ::= <watch_time>
  | <base_sixty> ":" <watch_digits>
  | <base_sixty> "min"
  | <base_sixty> "s"
  | <base_sixty> "min" <base_sixty> "s"
  | <integer> "h" <base_sixty> "min" <base_sixty> "s"
  | <integer> "h" <base_sixty> "min"
  | <integer> "h" <base_sixty> "s"

<time_pace> ::= <watch_time> "/km"

<term_pace> ::= "CL"
  | "CA"
  | "CV"
  | "TR"
  | "LVS"
  | "LE"
  | "MO"
  | "FO"
  | "FTS"
  | "MAX"

<pace> ::= <time_pace> | <term_pace>

<progression> ::= <pace> "->" <pace> ":" <distance>

<plus> ::= "+" | "/"

<step> ::= <time> <white1> <distance>
  | <time> <white1> <pace>
  | <distance> <white1> <pace>
  | <distance> <white1> <progression>

<repeat> ::= <integer> "x" <white> "(" <white> <steps> <white> ")"

<workout_step> ::= <step> <white> | <repeat> <white>

<steps> ::= (<workout_step> (<plus> <white> <workout_step>)*)

<workout> ::= <white> <steps> <white>
```

## Using the CLI

The CLI is used to parse the input and export the workout. Additionally, you may print an interval tree, that reads your pace table (translation of your terms to a concrete pace value) and displays the stats of the input workout.

### Pace table file

## Results

## Questions
  
