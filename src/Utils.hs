module Utils (fallbackAll, tailOrEmpty, capitalize) where

import Data.Char (toUpper)

fallback :: (Monad m) => m (Either a b) -> m (Either a b) -> m (Either a b)
fallback e1 e2 = do
  e1' <- e1
  case e1' of
    Right v -> return $ Right v
    Left _ -> e2

fallbackAll :: (Monad m, Foldable t) => t (m (Either a b)) -> m (Either a b) -> m (Either a b)
fallbackAll es def_err = foldr fallback def_err es

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_ : xs) = Just xs

tailOr :: [a] -> [a] -> [a]
tailOr xs ys = fromMaybe ys $ maybeTail xs

tailOrEmpty :: [a] -> [a]
tailOrEmpty xs = tailOr xs []

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize s = s