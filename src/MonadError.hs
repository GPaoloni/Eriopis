module MonadError where

import AST
import Control.Monad (liftM, ap)


newtype MErr a = MErr (Either Error a)
  deriving (Show)

instance Functor MErr where
  fmap = liftM

instance Applicative MErr where
  pure = return
  (<*>) = ap

instance Monad MErr where
  return x = MErr (Right x)
  m >>= f = case m of
    MErr (Left e)  -> MErr (Left e)
    MErr (Right x) -> f x
  -- case where m is MErr (Left e) is handled by the monadic machinery
  -- m >>= f = do 
  --   x <- m
  --   f x

class Monad m => MonadError m where
    throw :: Error -> m a

instance MonadError MErr where
  throw e = MErr (Left e)
