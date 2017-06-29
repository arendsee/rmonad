[![Travis-CI Build Status](https://travis-ci.org/arendsee/monadR.svg?branch=master)](https://travis-ci.org/arendsee/monadR)
[![Coverage Status](https://img.shields.io/codecov/c/github/arendsee/monadR/master.svg)](https://codecov.io/github/arendsee/monadR?branch=master)

# MonadR

`monadR` offers a pure means to propagate errors, warnings and notes through
a pipeline.


# What is a monad?

## A little background syntax

To describe what a monad is I first need to introduce a little syntax for
specifying functions. I am pulling this syntax roughly from Haskell (the
language where monads are most well developed). The function `max`, which takes
the maximum of two numbers can be specified as follows:

```
max :: Numeric -> Numeric -> Numeric
           ^         ^           ^
          /         /           /
 arg1 ---'  arg2 --'  result --'
```

While the function `max` takes specifically numeric arguments, some functions
can take arguments of any type. These type variables can be expressed with
single, lowercase letters. For example, a function that takes two arguments and
return the first argument would have the signature `a -> b -> a`.

Some types are also parameterized. For example, a homogenous list is
a parameterized type. A function that returns the nth element from a list might
have the signature:

```
nth :: List a -> Integer -> a
```

Where `List a` means a list of elements of type `a`, where `a` can be anything.
Now `List` could be replaced with generic value:

```
nth :: OrderedContainer l => l a -> Integer -> a
```

The term `OrderedContainer l` is a *constraint* on what values `l` can take. So
in R, this `l` might refer to a list, an atomic vector, a data.frame, or any of
the other types that are indexed.

## The monad definition

A monad is a parameterized type `m` for which the following two functions are
defined:

```
wrap :: a -> m a
bind :: m a -> (a -> m b) -> m b
```

The function `wrap` (or `return` in Haskell) takes a value and lifts it into
the monad. The `bind` function takes the value of type `a` wrapped in the monad
`m`, applies a function to it that produces `b` wrapped in a new monad. Note
that this is not the same as

```
bind :: m a -> (a -> b) -> m b   ### WRONG
```

We are not just applying a function to the value wrapped in the monad. `bind`
can be broken down into two pieces:

```
fmap :: m a -> (a -> m b) -> m (m b)
join :: m (m b) -> m b
```

`fmap` looks into `m a` and transforms `a` with the function `a -> m b`, which
results in something of type `m (m b)`. `join` then takes this nested monad and
converts it into the final form `m b`.

# The report monad

The monad implemented in this package is designed to allow errors, warnings,
and other messages to be propagated through a program without a invalid values
being passed on.
