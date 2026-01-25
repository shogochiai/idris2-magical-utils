||| EVM.Stack - EVM Stack operations
|||
||| EVM uses a max 1024-element stack of 256-bit words.
||| Stack operations are the core of EVM computation.
module EVM.Stack

import EVM.Word256
import Data.List
import Data.Maybe

%default covering

||| Get element at index (0-based)
indexAt : Nat -> List a -> Maybe a
indexAt _ [] = Nothing
indexAt 0 (x :: _) = Just x
indexAt (S n) (_ :: xs) = indexAt n xs

||| Maximum stack depth per EVM specification
public export
maxStackDepth : Nat
maxStackDepth = 1024

||| EVM Stack - list of 256-bit words
||| Head is top of stack
public export
data Stack : Type where
  MkStack : (items : List Word256) -> Stack

||| Empty stack
public export
empty : Stack
empty = MkStack []

||| Check if stack is empty
public export
isEmpty : Stack -> Bool
isEmpty (MkStack []) = True
isEmpty _ = False

||| Get stack depth
public export
depth : Stack -> Nat
depth (MkStack items) = length items

||| Check if stack would overflow
public export
wouldOverflow : Stack -> Bool
wouldOverflow s = depth s >= maxStackDepth

||| Stack operation result
public export
data StackResult : Type -> Type where
  Ok : a -> StackResult a
  Underflow : StackResult a
  Overflow : StackResult a

public export
Functor StackResult where
  map f (Ok x) = Ok (f x)
  map _ Underflow = Underflow
  map _ Overflow = Overflow

public export
Applicative StackResult where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Underflow <*> _ = Underflow
  _ <*> Underflow = Underflow
  Overflow <*> _ = Overflow
  _ <*> Overflow = Overflow

public export
Monad StackResult where
  Ok x >>= f = f x
  Underflow >>= _ = Underflow
  Overflow >>= _ = Overflow

||| Push a value onto the stack
public export
push : Word256 -> Stack -> StackResult Stack
push val (MkStack items) =
  if length items >= maxStackDepth
    then Overflow
    else Ok $ MkStack (val :: items)

||| Pop a value from the stack
public export
pop : Stack -> StackResult (Word256, Stack)
pop (MkStack []) = Underflow
pop (MkStack (x :: xs)) = Ok (x, MkStack xs)

||| Peek at top of stack without removing
public export
peek : Stack -> StackResult Word256
peek (MkStack []) = Underflow
peek (MkStack (x :: _)) = Ok x

||| Peek at nth element (0 = top)
public export
peekN : Nat -> Stack -> StackResult Word256
peekN n (MkStack items) =
  case indexAt n items of
    Nothing => Underflow
    Just x => Ok x

||| Pop n values from stack
public export
popN : (n : Nat) -> Stack -> StackResult (List Word256, Stack)
popN 0 s = Ok ([], s)
popN (S k) s = do
  (val, s') <- pop s
  (vals, s'') <- popN k s'
  Ok (val :: vals, s'')

||| Push multiple values (first in list pushed first, so last is on top)
public export
pushN : List Word256 -> Stack -> StackResult Stack
pushN [] s = Ok s
pushN (x :: xs) s = do
  s' <- push x s
  pushN xs s'

replaceAt : Nat -> a -> List a -> List a
replaceAt _ _ [] = []
replaceAt 0 v (_ :: xs) = v :: xs
replaceAt (S k) v (x :: xs) = x :: replaceAt k v xs

||| Swap top with nth element (1-indexed: SWAP1 swaps top with 2nd)
public export
swap : Nat -> Stack -> StackResult Stack
swap 0 s = Ok s  -- SWAP0 is identity
swap n (MkStack items) =
  if n >= length items
    then Underflow
    else case (head' items, indexAt n items) of
           (Just top, Just nth) =>
             let items' = replaceAt 0 nth (replaceAt n top items)
             in Ok (MkStack items')
           _ => Underflow

||| Duplicate nth element to top (0-indexed: DUP1 copies top)
public export
dup : Nat -> Stack -> StackResult Stack
dup n s@(MkStack items) = do
  val <- peekN n s
  push val s

||| Get all items (for debugging)
public export
toList : Stack -> List Word256
toList (MkStack items) = items

||| Create stack from list (head becomes top)
public export
fromList : List Word256 -> StackResult Stack
fromList items =
  if length items > maxStackDepth
    then Overflow
    else Ok (MkStack items)

||| Show instance for debugging
public export
Show Stack where
  show (MkStack items) =
    "[" ++ showItems items ++ "]"
    where
      showItems : List Word256 -> String
      showItems [] = ""
      showItems [x] = show x
      showItems (x :: xs) = show x ++ ", " ++ showItems xs
