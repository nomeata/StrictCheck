module Test.StrictCheck.Produce
  ( Produce(..)
  , recur
  , producePrimitive
  , producing
  , returning
  , variadic
  , Lazy(..)
  , freely
  , Input
  , Inputs
  ) where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

import           Data.Urn ( Urn, Weight )
import qualified Data.Urn          as Urn
import qualified Data.Urn.Internal as Urn ( uninsert )

import Data.Monoid ((<>))
import Control.Monad

import Test.StrictCheck.Internal.Inputs
import Test.StrictCheck.Consume

import Test.StrictCheck.Curry
import Test.StrictCheck.Curry.Function
import Generics.SOP


-------------------------------------------------------
-- The user interface for creating Produce instances --
-------------------------------------------------------

-- | Produce an arbitrary construction, but using Inputs to drive the
-- implicit destruction of the original input value.
class Produce b where
  produce :: Inputs -> Gen b

-- | Destruct some inputs to generate an output. This function handles the
-- interleaving of input destruction with output construction. It should always
-- be immediately called (on the supplied Inputs) at every recursive position
producing :: (Inputs -> Gen a) -> Inputs -> Gen a
producing part (Inputs inputs) = do
  (variants, inputs') <-
     foldM accumInput mempty =<< shuffle inputs
  vary variants $ part (Inputs inputs')
  where
    accumInput (vs, is) i = do
      (v, i') <- vary vs $ draws i
      return (v <> vs, i' : is)

recur :: Produce a => Inputs -> Gen a
recur = producing produce

-- | Use the Arbitrary instance for a type to produce it. This should only be
-- used for "flat" types, i.e. those which contain no interesting substructure.
producePrimitive :: Arbitrary b => Inputs -> Gen b
producePrimitive _ = arbitrary

returning :: Consume a => (Inputs -> Gen b) -> Inputs -> Gen (a -> b)
returning out =
  \(Inputs inputs) ->
    promote $ \a ->
      producing out (Inputs (consume a : inputs))

variadic :: forall args result. (All Consume args, Curry args result, SListI args )
         => (Inputs -> Gen result) -> Inputs -> Gen (args ⋯-> result)
variadic out =
  \(Inputs inputs) ->
    fmap (curryFunction @args . toFunction) . promote $ \args ->
      producing out . Inputs . (++ inputs) $
        hcollapse $ hcliftA (Proxy :: Proxy Consume) (K . consume . unI) args


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


-------------------------------------------------------------------------
-- Random destruction of the original input, as transformed into Input --
-------------------------------------------------------------------------

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
             (v', i'') <- vary v $ draws i'
             return (v <> v', i'') ]


---------------------------------------------
-- Integration with QuickCheck's Arbitrary --
---------------------------------------------

-- | We hook into QuickCheck's existing Arbitrary infrastructure by using
-- a newtype to differentiate our special way of generating things.
newtype Lazy a = Lazy { runLazy :: a }

instance Produce a => Arbitrary (Lazy a) where
  arbitrary = Lazy <$> freely produce

-- | A universal generator for all that can be produced (including functions).
freely :: (Inputs -> Gen a) -> Gen a
freely p = p (Inputs [])
