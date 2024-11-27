-- TODO: start an "Imperative Programming in Lean" github repo.

/-
Imperative programming in Lean
- main function signature : (List String) -> IO Unit
- do syntax to sequence monadic functions
- imperative style : let mut stuff ; in monadic context only (see Id or IO)
- TODO: I have to think more about this. Can I use this mutable stuff NESTED?
  For example can I have a list of mutable stuff? And really program in the
  same mental model than Python?
- Question: How do I make printf-debugging in Lean? In imperative code?
  In functional code (is there a "trace" stuff that wraps a function call,
  like in Haskell?)
- imperative style : for x in y do ; in monadic context only (see Id or IO)
- imperative style : Id.run do (or return value Id wrapper)
  for imperative style in pure functions
  (BTW, how is not the Id in "Id Int" not eliminated in the type signature?
  Anyway, any stuff of type Id Int is silently coerced into Int when needed
  later)
- FFS, early return, break and continue exist! All that reinterpreted in a
  monadic functional programming style via the ForIn typeclass! (see https://leanprover-community.github.io/mathlib4_docs/Init/Core.html#ForIn)
- TODO: study ForIn and make my own "iterable", e.g. make a lazily computed
  "enumerate" a la Python.

Misc confort tricks.
- #eval actually display stuff ("prints") when the final program is executed.
  HOW NICE!!!
- The mathlib index page also reference all standard Lean stuff and
  it's the best way to find some info about it IMHO.


Strings: https://github.com/leanprover/lean4/blob/master/src/Init/Data/String/Basic.lean
- !s"{}" template strings
- String.split; configurable separator but won't work for strings, only char seps
- String.splitOn works more similarly to Python str.split (except for the default case
  that just splits on whitespace and removes the empty strings). This default case
  is easily handled with splitOn and a post-filter however.
- List.asString and String.toList to go String <-> List Char.
- ... but actually, string.data also provide a list of chars
  and String.mk chars builds a string (see source in the prelude :
  https://github.com/leanprover/lean4/blob/master/src/Init/Prelude.lean)
- Char is unicode code point (UCS-32 internally, which is simple since fixed length).
- ... but Strings are actually utf-8 encoded!
- TODO: search for the equivalent of the other usual string operations in Python
  (starts_with, "in", etc), read the String source to see what is there, etc.


Files:
- IO.FS.readFile
-

Random: ?
- ??? Good way to see how monads delay computations?

Hash Maps somewhere ?
- ???

- IO source code: https://github.com/leanprover/lean4/blob/master/src/Init/System/IO.lean

-/

-- #eval String.mk "shdjshds".data

def acc₁ (numbers : List Int) (sum : Int := 0) : Int :=
  match numbers with
  | [] => sum
  | number :: numbers => acc₁ numbers (sum + number)

def acc₂ (numbers : List Int) : Id Int := do
  let mut sum := 0
  for number in numbers do
    sum := sum + number
  return sum

def acc₃ (numbers : List Int) : Int := Id.run do
  let mut sum := 0
  for number in numbers do
    sum := sum + number
  return sum

def print_args (args : List String) : IO Unit := do
  for arg in args do
    IO.println arg

def _main (_args : List String) : IO Unit := do
  let sum : Int := acc₂ [0, 1, 2, 3]
  IO.println sum

def splitOnWhitespace (string : String) : List String :=
  let strings := string.split Char.isWhitespace
  strings.filter (fun s => !s.isEmpty)

def display_lines (src : String) (line_numbers : Bool := false) : IO Unit := do
  let _ := line_numbers
  let lines := String.splitOn src (sep := "\n")
  for line in lines do
    IO.println line

def display_src (args : List String) : IO Unit := do
  let filename := args[0]!
  let src <- IO.FS.readFile filename
  let _ := display_lines src
  return

def display_until_stop (args : List String) : IO Unit := do
  for arg in args do
    IO.print (arg ++ " ")
    if arg = "STOP" then
      break
  IO.println ""


def main (args : List String) : IO Unit :=
  display_until_stop args
