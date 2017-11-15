Note to future self and/or you: we should definitely be able to (generically, I'm pretty sure?) write a `Gen (a -> ())` for any `a` whose structure is non-opaque (that is, a normal ADT).

The tricky thing about picking a good default `Gen` for the `Arbitrary` instance will be selecting weights so that it's unlikely that we'll generate a very big demand. Although, I think this can be fixed with the `sized` combinator in QuickCheck—I've never used it, but I think it's what we'll want.

At some point, we should also probably end up making `type Context a = a -> ()` into a newtype, so we can write instances for it. For one thing, it's a `Monoid`, where `mappend = (!!!)`, and likewise an `Alternative`. It's also a contravariant functor, the typeclass for which lives in some Kmett package..

Aha: https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html

So, it's `Contravariant`, `Divisible`, and `Decidable`, the contravariant equivalents of `Functor`, `Applicative`, and `Alternative`, respectively.

Another thought: for our final thing, we're going to want a function that gives us *two* demands: the demand on the input caused by the context (currently what we get), and the demand on the *output* caused by it too. We can do this by instrumenting the output from the function before feeding it into the context.

This justifies even more so  why we split the context and function apart!

User facing type should use something like this version of `Maybe`:

```
data Thunk a = E a  -- evaluated
             | T    -- thunk
```

And if they really want to make use of some function on `Maybe`s, we can give them:

```
toMaybe  Thunk a -> Maybe a
toThunk :: Maybe a -> Thunk a
asMaybe :: (Maybe a -> Maybe b)
                -> (Thunk a -> Thunk b)
```

If a field is strict, our generic demand type should omit the `Thunk` wrapper. We can detect strictness/unpackedness at the type level via Generics. :)

So the demand type for `data Foo = Foo !Int Bool` would be iso. to `data FooD = FooD IntD (Thunk BoolD)`

A question: should the field in `E` be strict? Should the fields in all demand types be strict? Does it matter, semantically?

(My intuition says yes / yes / no)


```
data Thunk a = T | E !a

type family Args (function :: *) :: [*] where
  Args (a -> b) = a ': Args b
  Args r        = '[]

type family WithSpecs (args :: [*]) :: [*] where
  WithSpecs (arg ': args) = (arg, Spec arg) ': WithSpecs args
  WithSpecs '[]           = '[]

type family Result (function :: *) :: * where
  Result (a -> b) = Result b
  Result r        = r

type family Demands (xs :: [*]) :: [*] where
  Demands (x ': xs) = Demand x ': Demands xs
  Demands '[]       = '[]

type family Thunks (xs :: [*]) :: [*] where
  Thunks (x ': xs) = Thunk x ': Thunks xs
  Thunks '[]       = '[]

type family Demand (x :: *) :: * where
  Demand (a -> b)  = FuncDemand
  Demand (a :+: b) = Demand a :+: Demand b
  Demand (a :*: b) = Thunk (Demand a) :*: Thunk (Demand b)
  ...

type family Spec (x :: *) :: * where
  Spec (a :*: b) = Spec a :*: Spec b
  Spec (a :+: b) = Spec a :+: Spec b
  Spec (a -> b) = Tuple (WithSpecs (Args (a -> b)))
                  -> Demand (Result (a -> b))
                  -> Tuple (Thunks (Demands (Args (a -> b))))
  ...
```

We do not need to use `atomicModifyIORef`, because even Par-style parallelism can't mess with things. In order for something to have a data race, it needs to read and write to the same location, but our algorithm for instrumentation does not ever read from the IORefs it writes to (that is, until the instrumented computation is completely finished). Therefore, we can safely drop the expense of atomicity.

We should catch all pattern-match failure exceptions and nicely report them to the user as a specification failure. Better yet, we should find a way to modify the user-written spec to insert information-carrying exceptions at every potential point of partiality so that we can pinpoint for them where they didn't cover a case in their specification. This might be overkill, because shrinking might give them a pretty good idea of how they screwed up.

TITLE: "Keep Your Laziness In Check"

We can make a trace function which does not affect the laziness of the function, using a similar IORef technique to how we instrument things. Instead of printing immediately, we send each value to an IORef holding the cumulative trace. Attention must be paid to: exceptions, infinite computations.

Specs really should take actual arguments, no matter how nested. However, the fuzzer is never going to generate dependent specs (it doesn't need to, to test random demands). So we can write a wrapper that elides needing to provide those arguments, so that users don't have to re-implement as much of the original function in most cases (or pass undefined themselves).

We can use CoArbitrary, if we are "completely horrible." We use frequency to determine whether to force down each individual substructure. Caveat: You lose probabilistic injectivity; but we don't care what the values *are*, we only care that a function forces parts of its input. We need to interleave the production of output constructors and the destruction of input constructors. This is fine.

Shrinking strategy: shrink inputs first, then fix inputs and shrink output. Compare to the theory where lattices are upward-infinite: this is never true, because we always have finite lists as the top of the lattice.
