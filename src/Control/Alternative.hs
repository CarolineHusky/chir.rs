module Control.Alternative where

import Control.ToEither (ToEither (toEither))

(?) :: (Monad m, ToEither n a b) => m (n b) -> (a -> m b) -> m b
m ? n =
  m
    >>= ( \case
            Left a -> n a
            Right a -> return a
        )
      . toEither

(?!) :: (Monad m, ToEither n a b) => m (n b) -> m b -> m b
m ?! n = m ? const n

(???) :: (Monad m, ToEither n a b) => m (n b) -> (a -> m (n b)) -> m (n b)
m ??? n =
  m
    >>= ( \case
            Left a -> n a
            Right a -> pure $ pure a
        )
      . toEither

(???!) :: (Monad m, ToEither n a b) => m (n b) -> m (n b) -> m (n b)
m ???! n = m ??? const n
