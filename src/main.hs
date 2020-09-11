{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (replicateM_)
import Foreign.C.Types (CFloat (..), CUInt (..))

foreign import ccall "srand" c_seed :: CUInt -> IO ()

seed :: Int -> IO ()
seed = c_seed . fromIntegral . (+ 1)

foreign import ccall "randf" c_rand :: IO CFloat

rand :: IO Float
rand = realToFrac <$> c_rand

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x : xs) = do
  p <- f x
  (l, r) <- partitionM f xs
  if p
    then return (x : l, r)
    else return (l, x : r)

{- NOTE: Implementation via
 - `https://discuss.ocaml.org/t/more-natural-preferred-way-to-shuffle-an-array/217`.
 -}
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle [x] = return [x]
shuffle xs = do
  (l, r) <- partitionM (\_ -> (< 0.5) <$> rand) xs
  (++) <$> shuffle l <*> shuffle r

main :: IO ()
main = do
  seed 0
  replicateM_ 10 $ rand >>= print
  partitionM (\_ -> (< 0.5) <$> rand) xs >>= print
  replicateM_ 3 $ shuffle xs >>= print
  where
    xs = [1 .. 10 :: Int]
