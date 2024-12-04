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


structure X1 where

def X1.forIn {β : Type u} [Monad m] (_ : X1) (b : β) (step : Int → β → m (ForInStep β)) : m β := do
  match (<- step 7 b) with
  | .yield _b' => return b   -- Not sure what should happen here.
  | .done _b'' => return b

instance : ForIn m X1 Int where
  forIn := X1.forIn

def it : X1 := {}

def use_iterable_constant : IO Unit := do
  let mut β := 1
  for elt in it do -- where does elt come from here?
    β := β + 1
    IO.println elt
  IO.println β

-- first, only generic wrt the element in the list, we'll deal with
-- the extra generality later that.
-- We should be able to wrap anything that goes into a ForIn loop, right?
structure Enumerate₁ (α :  Type u) where
  iterable : List α

def enumerate₁ (list : List α) : Enumerate₁ α :=
  { iterable := list }

def Enumerate₁.forIn (e : Enumerate₁ α) (b : β) (step : (Int × α) → β → IO (ForInStep β)) : IO β := do
  let mut b' := b
  let mut index := 0
  for elt in e.iterable do
    let step_result <- step (index, elt) b'
    match step_result with
    | .yield b'' => b' := b''
    | .done b'' => return b''
    index := index + 1
  return b'

instance (α : Type u) : ForIn IO (Enumerate₁ α) (Int × α) where
  forIn := Enumerate₁.forIn

/-
 For the record, here is how ChatGPT solves the problem:

    structure EnumerateSeq (α : Type) (seq : Seq α) where
      idx : Nat

    instance {α : Type} : ForIn m (EnumerateSeq α seq) (Nat × α) where
      forIn (es : EnumerateSeq α seq) init f := do
        let mut i := es.idx
        let mut result := init
        for x in seq do
          result ← f (i, x) result
          i := i + 1
        return result

    def enumerate {α : Type} (seq : Seq α) : EnumerateSeq α seq :=
      { idx := 0 }

So enumerate does not take a "forInable" but a Seq α, which kinda solves the
multiple extra parameter problem I guess. But 1) I don't know what it is
2) it's probably restrictive (?). Have a look at Seq to begin with I guess.

Mmmm this is weird how this is implemented. Does it deal with early breaks
dictated by f properly? I'd guess not (but that properly wouldn't be hard
to patch if that was the case).

 -/

/-

Alternate ChatGPT stuff:

    structure Enumerate₂ (m : Type u₁ → Type u₂) (ρ : Type u) (α : Type v) where
      seq : ρ
      idx : Nat := 0 -- Start index for enumeration

    -- Implement ForIn for Enumerate₂
    instance {m : Type u₁ → Type u₂} {ρ : Type u} {α : Type v} [ForIn m ρ α] :
        ForIn m (Enumerate₂ m ρ α) (Nat × α) where
      forIn (e : Enumerate₂ m ρ α) init f := do
        let mut i := e.idx
        let mut acc := init
        for x in e.seq do
          acc ← f (i, x) acc
          i := i + 1
        return acc

    -- Example usage
    def enumerate₂ {m : Type u₁ → Type u₂} {ρ : Type u} {α : Type v} [ForIn m ρ α]
        (seq : ρ) : Enumerate₂ m ρ α :=
      { seq := seq }

    def main : IO Unit :=
      let xs := [10, 20, 30]
      for (idx, val) in enumerate₂ xs do
        IO.println s!"Index: {idx}, Value: {val}"

 -/


structure Enumerate₂ (m : Type -> Type) (ρ : Type) (α : Type) [ForIn m ρ α] where
  it : ρ

def enumerate₂ {m : Type -> Type} {ρ : Type} {α : Type} [ForIn m ρ α] (it : ρ) : Enumerate₂ m ρ α :=
  {it := it}

instance {m : Type -> Type} {ρ : Type} {α : Type} [ForIn m ρ α] :
  ForIn m (Enumerate₂ m ρ α) (Nat × α) where
  forIn enumerator initial step := do
    let mut b := initial
    let mut index : Nat := 0
    for a in enumerator.it do
      match (<- step (index, a) b) with
      | .yield b' => do b := b' ; index := index + 1
      | .done b' => return b'
    return b

def main (args : List String) : IO Unit := do
  -- failed to synthesize instance for 'for_in%' notation
  -- ForIn (EIO IO.Error) (Enumerate₂ ?m.7013 (List String) String) ?m.7238
  -- for (i, arg) in enumerate₂ args do
  --  IO.println (i, arg)
  for (i, arg) in enumerate₂ (m := IO) args do
    IO.println (i, arg)
