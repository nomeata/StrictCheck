Note to future self and/or you: we should definitely be able to (generically, I'm pretty sure?) write a `Gen (a -> ())` for any `a` whose structure is non-opaque (that is, a normal ADT).

The tricky thing about picking a good default `Gen` for the `Arbitrary` instance will be selecting weights so that it's unlikely that we'll generate a very big demand. Although, I think this can be fixed with the `sized` combinator in QuickCheck—I've never used it, but I think it's what we'll want.

At some point, we should also probably end up making `type Context a = a -> ()` into a newtype, so we can write instances for it. For one thing, it's a `Monoid`, where `mappend = (!!!)`, and likewise an `Alternative`. It's also a contravariant functor, the typeclass for which lives in some Kmett package..

Aha: https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html

So, it's `Contravariant`, `Divisible`, and `Decidable`, the contravariant equivalents of `Functor`, `Applicative`, and `Alternative`, respectively.

Another thought: for our final thing, we're going to want a function that gives us *two* demands: the demand on the input caused by the context (currently what we get), and the demand on the *output* caused by it too. We can do this by instrumenting the output from the function before feeding it into the context.

This justifies even more so  why we split the context and function apart!

I know what the demand type for higher order functions has to be, and I know how to implement / instrument it!

Note mostly to self: Args really ought to take a nat parameter, cause there *are* use cases for testing under-saturated functions

User facing type should use something like this version of `Maybe`:

```
data Thunk a = E a  -- evaluated
             | T    -- thunk
```

This'll make writing demand functions less verbose by reducing the line noise of `Just`/`Nothing`

And if they really want to make use of some function on `Maybe`s, we can give them:

```
toMaybe  Thunk a -> Maybe a
toThunk :: Maybe a -> Thunk a
asMaybe :: (Maybe a -> Maybe b)
                -> (Thunk a -> Thunk b)
```

If a field is strict, our generic demand type should omit the `Thunk` wrapper. We can detect strictness/unpackedness at the type level via Generics. :)

So the demand type for `data Foo = Foo !Int Bool` would be iso. to `data FooD = FooD IntD (Thunk BoolD)`

I'm now separating out the demand types for different primitives, because I think it's actually wrong to have them be iso. to unit. I think, if forced, they should contain the value they actually evaluated to!

A question: should the field in `E` be strict? Should the fields in all demand types be strict? Does it matter, semantically?

(My intuition says yes / yes / no)
