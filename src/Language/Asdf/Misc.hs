{-# LANGUAGE FlexibleContexts #-}
module Language.Asdf.Misc where

import Control.Monad hiding (sequence, mapM)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe

import Data.Foldable -- hiding (elem, concat)
import Data.Traversable 

import Data.Maybe

import qualified Data.Map as Map

import Prelude hiding (sequence, foldr, concat, mapM, or, and)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \a b -> f (g a b)

infix 1 ?, ??

(?) :: Bool -> a -> a -> a
(c ? t) e = if c then t else e

(??) :: (Monad m) => m Bool -> m a -> m a -> m a
(c ?? t) e = do
  v <- c
  if v then t else e
  
bindM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f a b = join $ liftM2 f a b --return f `ap` a `ap` b

insertWithM :: (Monad m, Ord k) => (a -> a -> m a) -> k -> a -> Map.Map k a -> m (Map.Map k a)
insertWithM f k a t = sequence $ Map.insertWith (bindM2 f) k (return a) (fmap return t)

unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> Map.Map k a -> Map.Map k a -> m (Map.Map k a)
unionWithM m aMap bMap = unionWithKeyM (const m) aMap bMap

unionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> Map.Map k a -> Map.Map k a -> m (Map.Map k a)
unionWithKeyM m aMap bMap  = sequence $ Map.unionWithKey (\k -> bindM2 (m k)) (fmap return aMap) (fmap return bMap)

mergeMap :: (Ord k, Ord a) => Map.Map k a -> Map.Map a b -> Map.Map k b
mergeMap a b = Map.fromList $ catMaybes $ fmap lookupValue $ Map.keys a
  where
    lookupValue k = Map.lookup k a >>= flip Map.lookup b >>= return . ((,) k)

deletes :: (Ord a) => [a] -> Map.Map a b -> Map.Map a b
deletes keys t = foldr Map.delete t keys


anyM :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m Bool
-- anyM p t = liftM or $ mapM p xs  -- this almost works, but does not short circuit
anyM p t = anyM' p $ toList t
  where
    anyM' _ [] = return False
    anyM' p' (x:xs) = do
      eq <- p' x
      case eq of
        True -> return True
        False -> anyM' p' xs

allM :: (Traversable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM p xs = liftM and $ mapM p xs -- doesn't short circuit, need to fix this

nubByM :: (Monad m) => (a -> a -> m Bool) -> [a] -> m [a]
nubByM _ []             =  return []
nubByM eq (x:xs)         =  liftM (x :) $ nubByM eq =<< filterM (\y -> liftM not $ eq x y) xs

gZipWithM :: (Traversable f, Eq (f ()), Monad m) =>  (a -> b -> m c) -> f a -> f b -> m (Maybe (f c))
gZipWithM _ a b | not $ headEq a b = return Nothing
gZipWithM f a b = liftM (fromFoldable a) $ zipWithM f (toList a) (toList b)

gZipWithMT :: (Traversable f, Eq (f ()), Monad m) =>  (a -> b -> MaybeT m c) -> f a -> f b -> MaybeT m (f c)
gZipWithMT _ a b | not $ headEq a b = MaybeT $ return Nothing
gZipWithMT f a b = MaybeT . return . (fromFoldable a) =<< zipWithM f (toList a) (toList b)

fromFoldable :: (Traversable f, Foldable g) => f b -> g a -> Maybe (f a)
fromFoldable a t = g $ mapAccumL f (toList t) a
  where
    f [] _ = ([], Nothing)
    f (x:xs) _ = (xs, Just x)
    g (xs, x) | null xs = sequenceA x
    g _ = Nothing

unitize :: (Functor f) => f a -> f ()
unitize x = fmap (const ()) x

headEq :: (Functor f, Eq (f ())) => f a -> f b -> Bool
headEq a b = unitize a == unitize b

maybeM :: (Monad m) => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM m b f = m >>= maybe b f

getT :: (Monad m) => StateT s m s
getT = StateT $ \s -> return (s, s)

putT :: (Monad m) => s -> StateT s m ()
putT s = StateT $ \_ -> return ((), s)

modifyT :: (Monad m) => (s -> s) -> StateT s m ()
modifyT f = do
  s <- getT
  putT (f s)

getAndModifyT :: (Monad m) => (s -> s) -> StateT s m s
getAndModifyT f = do
  a <- getT
  modifyT f
  return a

modifyAndGetT :: (Monad m) => (s -> s) -> StateT s m s
modifyAndGetT f = modifyT f >> getT

askT :: (Monad m) => ReaderT r m r
askT = ReaderT return

localT :: (r -> r) -> ReaderT r m a -> ReaderT r m a
localT f m = ReaderT $ \r -> runReaderT m (f r)

tellT :: (Monad m) => w -> WriterT w m ()
tellT w = WriterT $ return ((), w)

makeSupply :: [[a]] -> [[a]] -> [[a]]
makeSupply inits tails = let vars = inits ++ (liftM2 (++) vars tails) in vars

varNames :: [String]
varNames = makeSupply (words "a b c d e f g h i j k") (words "1 2 3 4 5")

genSym :: (Monad m) => StateT [a] m a
genSym = do
  x:xs <- getT
  putT xs
  return x

both :: (Monad m) => (a -> m b) -> a -> m (b, a)
both m a = do
  b <- m a
  return (b, a)
  
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM f t = liftM concat $ mapM f t


