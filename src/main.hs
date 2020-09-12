{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Monad (replicateM, replicateM_)
import Foreign.C.Types (CFloat (..), CULong (..))

foreign import ccall "set_seed" c_set_seed :: CULong -> CULong -> IO ()

setSeed :: Int -> Int -> IO ()
setSeed s i = c_set_seed (fromIntegral s) (fromIntegral i)

foreign import ccall "get_random_f32" c_get_random_f32 :: IO CFloat

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

printIndent :: String -> IO ()
printIndent x = do
  putStr "\t"
  putStrLn x

main :: IO ()
main = do
  setSeed 0 0
  putStrLn "\nreplicateM_ 10 $ getRandom"
  replicateM_ 10 $ getRandom >>= printIndent . show
  putStrLn $ "\nreplicateM " ++ show n ++ " getRandom"
  xs' <- replicateM n getRandom
  printIndent $ "mean : " ++ show (sum xs' / (fromIntegral n :: Float))
  printIndent $ "max  : " ++ show (maximum xs')
  printIndent $ "min  : " ++ show (minimum xs')
  putStrLn $ "\npartitionM (\\_ -> (< 0.5) <$> getRandom) " ++ show xs
  partitionM (\_ -> (< 0.5) <$> getRandom) xs >>= printIndent . show
  putStrLn $ "\nreplicateM_ 3 $ shuffle " ++ show xs
  replicateM_ 3 $ shuffle xs >>= printIndent . show
  where
    xs = [1 .. 10 :: Int]
    n = 100000 :: Int
