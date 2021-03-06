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

Specs really should take actual arguments, no matter how nested. However, the fuzzer is never going to generate dependent specs (it doesn't need to, to test random demands). So we can write a wrapper that elides needing to provide those arguments, so that users don't have to re-implement as much of the original function in most cases (or pass undefined themselves). (KWF: I don't think this is true anymore.)

We can use CoArbitrary, if we are "completely horrible." We use frequency to determine whether to force down each individual substructure. Caveat: You lose probabilistic injectivity; but we don't care what the values *are*, we only care that a function forces parts of its input. We need to interleave the production of output constructors and the destruction of input constructors. This is fine.

Shrinking strategy: shrink inputs first, then fix inputs and shrink output. Compare to the theory where lattices are upward-infinite: this is never true, because we always have finite lists as the top of the lattice.

In general, a correct specification should be monotonic in their demand, but specifications generated by CoArbitrary will almost surely violate that contract. Shall we tweak CoArbitrary such that specifications have such properties? Turns out, it actually matters! Specifications that violate this contract can't be realized, and that's a problem because we also need to fuzz the input functions to higher-order functions. Perhaps we should generate specifications from fuzzed higher-order functions? Which direction shall we go? (KWF: We should generate functions, then synthesize correct-by-construction specs.)

We can simplify the Spec type family drastically, if we shift the burden of generating spec to the user, but provide them with an operator `getSpec` on funcions. The `getSpec` operator under the hood will use the same observation mechanism to first turn demand on the ouputs to a partial Context function on the output, and reify the demand on the input. (KWF: See above: this is the right solution, I'm very sure.)

A nice debugging utility is to observe what pattern matches a spec makes on the input to the function under specification, and inspect if it demands more information than the function under spec (because a spec should not really need to). And these extra information could be the source of errors in specs. But note that a spec which inspects more information than it needs to is not necessarily a wrong one! For instance, if a spec says `length list == 0`, that may be more information than the specified function gets access to, but it's another way to write something that could be accomplished via a case statement that would not violate this information requirement. Determining if an arbitrary user-written spec is equivalent to some spec which does not violate the causality constraints we calculate is, of course, undecidable.

Section title: "A Race To The Bottom"

Maybe use unsafe ref updates to generate things? Need heterogenous list of pointers.

Maybe heterogenous list of multi-hole tree zippers?

Left-recursive fixpoints for continuations

We should try to generalize all the operations in Observe.hs using a single higher-order "fold". One approach is try to use a "functor functor" of king (* -> *) -> *, see the blog post here: https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html

PUT THIS IN THE PAPER:

λ: (x, y) = entangle ()
λ: x
()
λ: y
E ()
λ: (x, y) = entangle ()
λ: y
T
λ: x
()
λ: y
T
