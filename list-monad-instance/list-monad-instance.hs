-- Implements the list monad instance, and therefore also needs the functor and applicative

infixr 5 :.
data List a = Nil | a :. (List a) deriving (Eq, Ord, Show, Read)

infixr 5 ++.
(++.) :: List a -> List a -> List a
Nil ++. ys = ys
(x :. xs) ++. ys = x :. (xs ++. ys)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap g (x :. xs) = g x :. fmap g xs


instance Monad List where
  return :: a -> List a
  return a = a :. Nil

  -- -XInstanceSigs
  (>>=) :: List a -> (a -> List b) -> List b
  (x :. xs) >>= f = (f x) ++. (xs >>= f)
  Nil >>= _ = Nil


instance Applicative List where
  pure :: a -> List a
  pure x = x :. Nil

  (<*>) :: List (a -> b) -> List a -> List b
  gs <*> xs = do g <- gs
  		 x <- xs
		 return (g x)
