module Utils (fallbackAll, tailOrEmpty, capitalize, headOr, (>$>)) where

import Data.Char (toUpper)

fallback :: (Monad m) => m (Either a b) -> m (Either a b) -> m (Either a b)
fallback e1 e2 = do
  e1' <- e1
  case e1' of
    Right v -> return $ Right v
    Left _ -> e2

fallbackAll :: (Monad m, Foldable t) => t (m (Either a b)) -> m (Either a b) -> m (Either a b)
fallbackAll es def_err = foldr fallback def_err es

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

-- | cursed map operator, like fmap but the function is a functor, and the value is not
(>$>) :: (Functor f) => f (a -> b) -> a -> f b
f >$> v = (\f' -> f' v) <$> f
