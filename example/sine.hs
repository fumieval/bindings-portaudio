{-# LANGUAGE ViewPatterns, LambdaCase #-}
import System.PortAudio
import Control.Concurrent
import Linear (V2(..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as MV
import System.Environment
import System.Exit

period :: Int
period = 128

table :: V.Vector Float
table = V.fromList [sin t | i <- [0..period - 1], let t = fromIntegral i / fromIntegral period * 2 * pi]

callback :: MVar Int -> Status -> input -> MV.IOVector (V2 Float) -> IO StreamCallbackResult
callback phase _ _ o = do
  i0 <- takeMVar phase
  go i0 0
  putMVar phase $ i0 + n
  return Continue
  where
    n = MV.length o
    go :: Int -> Int -> IO ()
    go i0 i
      | i == n = return ()
      | otherwise = do
        let v = table V.! ((i0 + i) `mod` period)
        MV.write o i (V2 v v)
        go i0 (i + 1)

main :: IO ()
main = getArgs >>= \case
  ((read -> rate) : (read -> buf) : _) -> withPortAudio $ do
    (_, dev : _) <- getDevices
    phase <- newMVar 0
    let output = streamParameters dev 0
    withStream rate buf noConnection output mempty (callback phase)
      $ threadDelay $ 1000 * 1000
  _ -> usageExit

usageExit :: IO ()
usageExit = do
  pname <- getProgName
  putStrLn $ "\nUsage: " ++ pname ++ " <sample rate> <buffer size>\n"
  exitSuccess
