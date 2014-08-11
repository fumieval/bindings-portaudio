{-# LANGUAGE ViewPatterns #-}
import Bindings.PortAudio
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector as V

period :: Int
period = 128

table :: V.Vector CFloat
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

callback phase _ o_ (fromIntegral -> n) info _ _ = do
  let o = castPtr o_
  i0 <- takeMVar phase
  forM_ [0..n-1] $ \i -> do
    let v = table V.! ((i0 + i) `mod` period)
    pokeElemOff o (2 * i) v
    pokeElemOff o (2 * i + 1) v
  putMVar phase $ i0 + n
  return c'paContinue

main = do
  _ <- c'Pa_Initialize
  n <- c'Pa_GetHostApiCount
  forM_ [0..n - 1] $ \i -> do
    info <- c'Pa_GetHostApiInfo i >>= peek
    name <- peekCAString $ c'PaHostApiInfo'name info
    print (i, name)

  ref <- newMVar 0
  cb <- mk'PaStreamCallback $ callback ref
  
  ps <- malloc
  c'Pa_OpenDefaultStream ps 0 2 1 44100 1024 cb nullPtr
  s <- peek ps

  c'Pa_StartStream s
  c'Pa_Sleep 5000
  c'Pa_StopStream s
  c'Pa_CloseStream s
  c'Pa_Terminate