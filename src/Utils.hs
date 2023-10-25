module Utils (
  tailOrEmpty,
  capitalize,
  headOr,
  repeatM,
  whileM,
  timeoutM,
  forkM,
  catchM,
  (<<<$>>>),
  intersperseAfter,
) where

import Control.Concurrent (forkIO)
import Control.Exception (catch)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Data.Char (toUpper)
import System.Timeout (timeout)
import Yesod (MonadUnliftIO (withRunInIO))

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_ : xs) = Just xs

tailOr :: [a] -> [a] -> [a]
tailOr xs ys = fromMaybe ys $ maybeTail xs

headOr :: [a] -> a -> a
headOr xs y = fromMaybe y $ maybeHead xs

tailOrEmpty :: [a] -> [a]
tailOrEmpty xs = tailOr xs []

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize s = s

(<<<$>>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <<<$>>> v = (f <$>) <$> v

whileM :: (Monad m) => m Bool -> m ()
whileM m = do
  cont <- m
  if cont then whileM m else pass

repeatM :: (Monad m) => Int -> m () -> m ()
repeatM 0 _ = pass
repeatM n m = m >> repeatM (n - 1) m

timeoutM :: (MonadUnliftIO m) => Int -> m a -> m (Maybe a)
timeoutM duration monad = withRunInIO (\run' -> timeout duration $ run' monad)

forkM :: (MonadUnliftIO m) => m a -> m ()
forkM m = withRunInIO (\run' -> forkIO $ run' m >> pass) >> pass

catchM :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
catchM m = withRunInIO (\run' -> catch (Right <$> run' m) (return . Left))

intersperseAfter :: a -> [a] -> [a]
intersperseAfter _ [] = []
intersperseAfter x (y : ys) = y : x : intersperseAfter x ys
