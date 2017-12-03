{-# language BangPatterns, TupleSections, RankNTypes, TypeOperators, GADTs,
  FlexibleContexts, DefaultSignatures #-}

module Test.StrictCheck.Generate
  ( Input
  , Inputs
  , Consume(..)
  , Produce(..)
  , fields
  , recur
  , consumeFlat
  , produceFlat
  , Lazy(..)
  , lazy
  ) where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe ( promote )

import           Data.Urn ( Urn, Weight )
import qualified Data.Urn          as Urn
import qualified Data.Urn.Internal as Urn ( uninsert )

import Data.Monoid
import GHC.Generics


--------------------------------------------------
-- The core user-facing types: Input and Inputs --
--------------------------------------------------

-- | A tree representing all possible destruction sequences for a value
-- Unfolding the contained urns forces a particular random control path
-- for destructing the datatype.
newtype Input =
  Input (Maybe (Urn (Variant, Input)))

-- | A list of inputs given to a function, in abstract form. This lazy structure
-- is evaluated piecewise during the course of producing a function, thus
-- triggering the partial evaluation of the original input to the function.
newtype Inputs = Inputs [Input]


----------------------------------------------------------
-- The two user-facing typeclasses: Consume and Produce --
----------------------------------------------------------

-- | Generate a tree of all possible ways to destruct the input value.
class Consume a where
  consume :: a -> Input
  default consume :: (Generic a, GFields (Rep a)) => a -> Input
  consume x = fields (gFields (from x))

-- | Produce an arbitrary construction, but using Inputs to drive the
-- implicit destruction of the original input value.
class Produce b where
  produce :: Inputs -> Gen b


------------------------------------------------------------------
-- The user interface for writing Produce and Consume instances --
------------------------------------------------------------------

-- | Destruct some inputs to generate an output. This function handles the
-- interleaving of input destruction with output construction. It should always
-- be immediately called (on the supplied Inputs) at every recursive position
recur :: Produce a => Inputs -> Gen a
recur (Inputs is) = do
  (vs, is') <- unzip <$> mapM draws is
  vary (mconcat vs) $ produce (Inputs is')

-- | Reassemble pieces of input into a larger Input. The Words are weights which
-- determine the relative probability of continuing to pattern match in that
-- subpart of the input.
fields :: [Input] -> Input
fields =
  Input . Urn.fromList .
    zipWith (\v input ->
               (1, (Variant (variant v), input)))
            [(0 :: Int) ..]

-- | If a non-algebraic input contains entropy needed to randomize output, use
-- some function to capture this entropy. In most cases, this will be a thinly
-- wrapped call to variant.
consumeFlat :: (forall b. a -> Gen b -> Gen b) -> a -> Input
consumeFlat varyA !a =
  Input . Just . Urn.singleton 1 $
    (Variant (varyA a), Input Nothing)

-- | If something is opaque and all we know is how to generate an arbitrary one,
-- we can fall back on its Arbitrary instance.
produceFlat :: Arbitrary b => Inputs -> Gen b
produceFlat _ = arbitrary


-------------------------------------------------------------------------
-- Random destruction of the original input, as transformed into Input --
-------------------------------------------------------------------------

-- | A variant which can be applied to any generator--kept in a newtype to get
-- around lack of impredicativity.
newtype Variant =
  Variant { vary :: forall a. Gen a -> Gen a }

instance Monoid Variant where
  v `mappend` w = Variant $ vary v . vary w
  mempty = Variant id

-- | Pattern-match on a randomly chosen single constructor of the input, and
-- produce the corresponding Variant, whose value depends on which constructor
-- was matched.
draw :: Input -> Gen (Variant, Input)
draw (Input i) =
  case i of
    Nothing  -> return $ (mempty, Input i)
    Just urn -> do
      (_, (v, Input inner), outer) <- Urn.remove urn
      return $ (v, Input $ merge inner outer)
  where
    merge :: Maybe (Urn a) -> Maybe (Urn a) -> Maybe (Urn a)
    merge left right =
      case (left, right) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just r)  -> Just r
        (Just l, Nothing)  -> Just l
        (Just l, Just r)   -> Just $ Urn.addToUrn l (contents r)

    contents :: Urn a -> [(Weight, a)]
    contents urn =
      case Urn.uninsert urn of
        (weight, a, _, Just urn') -> (weight, a) : contents urn'
        (weight, a, _, Nothing)   -> [(weight, a)]

-- | Destruct some randomly chosen subparts of the input, and return a composite
-- Variant whose entropy is derived from all the inputs destructed. The
-- probability of n pieces of input being consumed decreases as n increases.
draws :: Input -> Gen (Variant, Input)
draws i =
  oneof [ return (mempty, i)
        , do (v, i')   <- draw i
             (v', i'') <- draws i'
             return (v <> v', i'') ]


---------------------------------------
-- How to make random lazy functions --
---------------------------------------

-- NOTE: You may be tempted to call produce instead of recur here, but this will
-- mean that all of your functions will be 1st-output-lazy. Thus, we use recur.

-- NOTE: This instance must be defined in this module, as it has to break the
-- abstraction of the Inputs type. No other instance needs to break this.
-- Incidentally, it also must break Gen's abstraction barrier, because it needs
-- to use promote to make a function.

instance (Consume a, Produce b) => Produce (a -> b) where
  produce (Inputs inputs) =
    promote $ \a ->
      recur (Inputs (consume a : inputs))

instance Consume (a -> b) where
  consume = consumeFlat (const id)


---------------------------------------------
-- Integration with QuickCheck's Arbitrary --
---------------------------------------------

-- | We hook into QuickCheck's existing Arbitrary infrastructure by using
-- a newtype to differentiate our special way of generating things.
newtype Lazy a = Lazy { runLazy :: a }

instance Produce a => Arbitrary (Lazy a) where
  arbitrary = Lazy <$> lazy

-- | A universal generator for all that can be produced (including functions).
lazy :: Produce a => Gen a
lazy = produce (Inputs [])


--------------------------------------------
-- Deriving Consume instances generically --
--------------------------------------------

class GFields f where
  gFields :: f p -> [Input]

instance GFields V1 where
  gFields _ = []

instance GFields U1 where
  gFields !U1 = []

instance (GFields f, GFields g) => GFields (f :+: g) where
  gFields (L1 x) = gFields x
  gFields (R1 x) = gFields x

instance (GFields f, GFields g) => GFields (f :*: g) where
  gFields (x :*: y) = gFields x ++ gFields y

instance (GFields f) => GFields (M1 i t f) where
  gFields (M1 x) = gFields x

instance (Consume c) => GFields (K1 i c) where
  gFields (K1 x) = [consume x]
