---
title: Nondeterministic Programming in Haskell
date: January 26, 2013
tags: haskell, functional programming
---

Warning, here be monads. Nondeterministic programming involves automatic searching for an answer given some constraints. [Sicp](http://mitpress.mit.edu/sicp/full-text/) 
a book you should read if you haven't already) introduces this concept with the [amb operator](http://mitpress.mit.edu/sicp/full-text/sicp/book/node89.html/). 
(amb e1 e2 ... en) returns one of e1 ... en ambiguously based on a set of rules. These rules are specified by
using the require operator. (require <cond>) will stop amb from selecting values that prevent <cond> from being true. For example we can state

~~~~~{.scheme}
(define (get-even)
    (let ((a (amb 1 2 3))
          (b (amb 4 5 6)))
        (require (= 8 (+ a b)))
        (list a b)))
~~~~~

This will return a and b sum to 8 and are from certain sets. The problem with amb is that it requires either a complex evaluator or macro to get it to work. Haskell
gives an alternative to this: the list monad. The list monad works almost identically to amb except that it will return all possible values instead of just one. 
So how does this work? The instance of list is as follows:

~~~~~{.haskell}
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
~~~~~

When we return a value we just wrap it in a list. Binding works as follows xs >>= f will map all items of xs to f then concat them together. When we fail we just get an
empty list. For example

~~~~~{.haskell}
[1,2,3] >>= \x -> [4,5,6] >>= \y -> return (x,y)
~~~~~

will return all pairs (x,y) where x is from [1,2,3] and y is from [4,5,6]. The above example is ugly because we need to chain the different parts together with >>= and 
lambda declarations. Haskell provides do notation which chains functions together automatically. 

~~~~~{.haskell}
pairs = do x <- [1,2,3]
           y <- [4,5,6]
           return (x,y)
~~~~~

The function (guard <cond>) will pass the value unchanged to the next function if <cond> is true and fails (passes []) otherwise. If for example we want all pairs (x,y)
such that x + y == 8 then we can add a guard to ensure this.

~~~~~{.haskell}
import Control.Monad

pairs = do x <- [1,2,3]
           y <- [4,5,6]
           guard (x+y==8)
           return (x,y)
~~~~~

As you might have noticed this is exactly what we wanted to do with amb in the scheme example. guard works just like require did, we just state it and it eliminates all
the values which don't satisfy the stated conditions. 

One cool thing to do with amb is solve logic problems. The Sicp chapter on non-deterministic values gives a number
of these, let's try translate some of these into do notation.

The first problem is simple and stated as follows in Sicp:

    Baker, Cooper, Fletcher, Miller, and Smith live on different floors 
    of an apartment house that contains only five floors. 
    Baker does not live on the top floor. 
    Cooper does not live on the bottom floor. 
    Fletcher does not live on either the top or the bottom floor. 
    Miller lives on a higher floor than does Cooper. 
    Smith does not live on a floor adjacent to Fletcher's. 
    Fletcher does not live on a floor adjacent to Cooper's. 
    Where does everyone live?

To solve this we need to assign a floor number from 1 to 5 for each person while still satisfying all the conditions.

~~~~~{.haskell}
import Data.List
import Control.Monad

apart x y = not $ abs (x - y) == 1
distinct xs = nub xs == xs
assignFloors = do baker <- [1..4]
                  cooper <- [2..5]
                  fletcher <- [2..4]
                  miller <- [cooper+1..5]
                  smith <- [1..5]
                  let people = [baker,cooper,fletcher,miller,smith]
                  guard (distinct people)
                  guard (apart fletcher smith)
                  guard (apart fletcher cooper)
                  return people
~~~~~

First we declare the possible values for each person. In the scheme version each number is selected from a range of 1 to 5 so that restrictions such as 
"Baker does not live on the top floor" need to be stated explicitly. I've baked these into the ranges of values. Then we only need to check Fletcher is apart from
Smith and Cooper and that no two people are given the same floor which is done by checking the list of floor values is distinct. When we run this we will get
a list of possible floor assignments that suit our conditions. One such assignment would be [3,2,4,5,1]. 

What's most impressive about using the list monad for
this is how elegant it is. The resulting code is almost identical to the Scheme solution using amb but without requiring extra leverage from interpreters or 
macros - it comes out naturally from the monad definition.
