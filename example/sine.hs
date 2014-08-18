{-# LANGUAGE ViewPatterns, LambdaCase #-}
import Bindings.PortAudio
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign
import qualified Data.Vector as V
import System.Environment

period :: Int
period = 128

table :: V.Vector CFloat
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

callback phase _ (castPtr -> o) (fromIntegral -> n) info _ _ = do
  i0 <- takeMVar phase
  go i0 0
  putMVar phase $ i0 + n
  return c'paContinue
  where
    go i0 i
      | i == n = return ()
      | otherwise = do
        let v = table V.! ((i0 + i) `mod` period)
        pokeElemOff o (2 * i) v
        pokeElemOff o (2 * i + 1) v
        go i0 (i + 1)

dbg s m = do
  e <- m
  putStrLn $ s ++ ": " ++ show e
  unless (e == 0) $ fail "Failed."

main = getArgs >>= \case
  ((read -> rate) : (read -> buf) : _) -> do
    dbg "Initialization" c'Pa_Initialize
    n <- c'Pa_GetHostApiCount
    putStrLn "Available APIs: "
    forM_ [0..n - 1] $ \i -> do
      info <- c'Pa_GetHostApiInfo i >>= peek
      name <- peekCAString $ c'PaHostApiInfo'name info
      print (i, name)

    ref <- newMVar 0
    cb <- mk'PaStreamCallback $ callback ref
    
    ps <- malloc
    dbg "Opening the default stream" $ c'Pa_OpenDefaultStream ps 0 2 1 rate buf cb nullPtr
    s <- peek ps

    dbg "Starting the stream" $ c'Pa_StartStream s
    c'Pa_Sleep $ 1000 * 1000
    dbg "Stopping the stream" $ c'Pa_StopStream s
    dbg "Closing the stream" $ c'Pa_CloseStream s
    c'Pa_Terminate