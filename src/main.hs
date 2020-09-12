{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (replicateM_)
import Foreign.C.Types (CFloat (..), CULong (..))

foreign import ccall "set_seed" c_set_seed :: CULong -> CULong -> IO ()

foreign import ccall "get_random_f32" c_get_random_f32 :: IO CFloat

setSeed :: Int -> Int -> IO ()
setSeed s i = c_set_seed (fromIntegral s) (fromIntegral i)

getRandom :: IO Float
getRandom = realToFrac <$> c_get_random_f32

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x : xs) = do
  (l, r) <- partitionM f xs
  p <- f x
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
  (l, r) <- partitionM (\_ -> (< 0.5) <$> getRandom) xs
  (++) <$> shuffle l <*> shuffle r

main :: IO ()
main = do
  setSeed 0 0
  replicateM_ 10 $ getRandom >>= print
  partitionM (\_ -> (< 0.5) <$> getRandom) xs >>= print
  replicateM_ 3 $ shuffle xs >>= print
  where
    xs = [1 .. 10 :: Int]
