module Utils where

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

headOr :: [a] -> a -> a
headOr xs def = fromMaybe def $ maybeHead xs

tailOr :: [a] -> [a] -> [a]
tailOr xs ys = fromMaybe ys $ maybeTail xs

tailOrEmpty :: [a] -> [a]
tailOrEmpty xs = tailOr xs []