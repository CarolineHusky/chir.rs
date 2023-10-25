module Control.ToEither where

import Data.Validation (Validation (..))

class (Applicative m) => ToEither m a b | m -> a, m b -> b where
  toEither :: m b -> Either a b

instance ToEither Maybe () a where
  toEither :: Maybe a -> Either () a
  toEither Nothing = Left ()
  toEither (Just v) = Right v

instance ToEither (Either a) a b where
  toEither :: Either a b -> Either a b
  toEither = id

instance (Semigroup a) => ToEither (Validation a) a b where
  toEither :: Validation a b -> Either a b
  toEither (Success v) = Right v
  toEither (Failure e) = Left e
