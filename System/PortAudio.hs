{-# LANGUAGE CPP, ViewPatterns, FlexibleContexts, ScopedTypeVariables, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module System.PortAudio(
  -- * Initialization
  withPortAudio
  , Error(..)
  -- * Devices
  , getDevices
  , Device(..)
  -- * Opening a stream
  , withStream
  -- * Stream parameters
  , StreamParameters
  , streamParameters
  , PortAudioSample
  -- * Timestamps and status flags
  , Status(..)
  -- * Stream flags
  , StreamFlags
  , clipOff
  , ditherOff
  , neverDropInput
  , primeOutputBuffersUsingStreamCallback
  ) where

import Bindings.PortAudio
import Foreign.C.Types
import Foreign.C.String
import Foreign
import Control.Monad
import Control.Exception
import Data.Typeable
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Data.Foldable as F
#if !MIN_VERSION_base(4,8,0)
import Data.Int
import Data.Word
import Data.Proxy
#endif

data Error = NotInitialized
  | UnanticipatedHostError
  | InvalidChannelCount
  | InvalidSampleRate
  | InvalidDevice
  | InvalidFlag
  | SampleFormatNotSupported
  | BadIODeviceCombination
  | InsufficientMemory
  | BufferTooBig
  | BufferTooSmall
  | NullCallback
  | BadStreamPtr
  | TimedOut
  | InternalError
  | DeviceUnavailable
  | IncompatibleHostApiSpecificStreamInfo
  | StreamIsStopped
  | StreamIsNotStopped
  | InputOverflowed
  | OutputUnderflowed
  | HostApiNotFound
  | InvalidHostApi
  | CanNotReadFromACallbackStream
  | CanNotWriteToACallbackStream
  | CanNotReadFromAnOutputOnlyStream
  | CanNotWriteToAnInputOnlyStream
  | IncompatibleStreamHostApi
  | BadBufferPtr
  deriving (Show, Eq, Ord, Enum, Typeable)

instance Exception Error

fromErrorCode :: CInt -> Error
fromErrorCode n = toEnum (fromIntegral n + 10000)

data Device t = Device {
  deviceIndex :: CInt
  , deviceName :: String
  , deviceMaxChannels :: Int
  , deviceLowLatency :: Double
  , deviceHighLatency :: Double
  , deviceDefaultSampleRate :: Double
  } deriving (Show, Eq, Ord)

data Input
data Output

getDevices :: IO ([Device Input], [Device Output])
getDevices = do
  n <- c'Pa_GetDeviceCount
  foldM addDevice ([], []) [0..n-1]
  where
    addDevice (xs, ys) i = do
      info <- c'Pa_GetDeviceInfo i >>= peek
      name <- peekCAString $ c'PaDeviceInfo'name info
      let chi = c'PaDeviceInfo'maxInputChannels info
      let cho = c'PaDeviceInfo'maxOutputChannels info
      return (if chi > 0
        then Device i name (fromIntegral chi)
          (realToFrac $ c'PaDeviceInfo'defaultLowInputLatency info)
          (realToFrac $ c'PaDeviceInfo'defaultHighInputLatency info)
          (realToFrac $ c'PaDeviceInfo'defaultSampleRate info) : xs
        else xs
        , if cho > 0
          then Device i name (fromIntegral cho)
            (realToFrac $ c'PaDeviceInfo'defaultLowOutputLatency info)
            (realToFrac $ c'PaDeviceInfo'defaultHighOutputLatency info)
            (realToFrac $ c'PaDeviceInfo'defaultSampleRate info) : ys
          else ys)

class PortAudioSample a where
  paSampleFormat :: proxy a -> CULong

instance PortAudioSample Float where
  paSampleFormat _ = 1

instance PortAudioSample Int32 where
  paSampleFormat _ = 2

instance PortAudioSample Int16 where
  paSampleFormat _ = 8

instance PortAudioSample Int8 where
  paSampleFormat _ = 16

instance PortAudioSample Word8 where
  paSampleFormat _ = 32

newtype StreamParameters t a = StreamParameters C'PaStreamParameters deriving Storable

streamParameters :: forall t f a. (Applicative f, Foldable f, PortAudioSample a)
  => Device t
  -> Double
  -> Maybe (StreamParameters t (f a))
streamParameters dev t
  | n > fromIntegral (deviceMaxChannels dev) = Nothing
  | otherwise = Just $ StreamParameters $ C'PaStreamParameters
    (deviceIndex dev)
    n
    (paSampleFormat (Proxy :: Proxy a))
    (CDouble t)
    nullPtr
  where
    n = F.sum (pure 1 :: f CInt)

newtype StreamFlags = StreamFlags CULong

instance Monoid StreamFlags where
  mempty = StreamFlags 0
  StreamFlags a `mappend` StreamFlags b = StreamFlags (a .|. b)

clipOff :: StreamFlags
clipOff = StreamFlags 0x00000001

ditherOff :: StreamFlags
ditherOff = StreamFlags 0x00000002

neverDropInput :: StreamFlags
neverDropInput = StreamFlags 0x00000004

primeOutputBuffersUsingStreamCallback :: StreamFlags
primeOutputBuffersUsingStreamCallback = StreamFlags 0x00000008

w :: IO CInt -> IO ()
w n = do
  r <- n
  unless (r == 0) $ throwIO $ fromErrorCode r

withPortAudio :: IO a -> IO a
withPortAudio = bracket_ (w c'Pa_Initialize) (w c'Pa_Terminate)

data Status = Status
  { currentTime :: !Double
  , inputBufferAdcTime :: !Double
  , outputBufferDacTime :: !Double
  , inputUnderflow :: !Bool
  , inputOverflow :: !Bool
  , outputUnderflow :: !Bool
  , outputOverflow :: !Bool
  , primingOutput :: !Bool }

withStream :: (Storable i, Storable o)
  => Double -- ^ sampling rate
  -> Int -- ^ buffer size
  -> Maybe (StreamParameters Input i)
  -> Maybe (StreamParameters Output o)
  -> StreamFlags
  -> (Status -> V.Vector i -> MV.IOVector o -> IO ()) -- ^ callback
  -> IO r
  -> IO r
withStream rate buf paramI paramO (StreamFlags flags) f m =
  withMaybe paramI $ \pin -> withMaybe paramO $ \pout -> do
    cb <- mk'PaStreamCallback $ callback f
    let opener = do
          ps <- malloc
          w $ c'Pa_OpenStream ps
              (castPtr pin)
              (castPtr pout)
              (CDouble rate)
              (fromIntegral buf)
              flags
              cb
              nullPtr
          peek ps
    bracket opener (\s -> w (c'Pa_CloseStream s) >> free s)
      $ \s -> bracket_ (w $ c'Pa_StartStream s) (w $ c'Pa_StopStream s) m

callback :: (Storable a, Storable b) => (Status -> V.Vector a -> MV.IOVector b -> IO ()) -> Ptr () -> Ptr () -> CULong -> Ptr C'PaStreamCallbackTimeInfo -> CULong -> z -> IO CUInt
callback f (castPtr -> pin) (castPtr -> pout) (fromIntegral -> n) pinfo flags _ = do
  ip <- newForeignPtr_ pin
  op <- newForeignPtr_ pout
  info <- peek pinfo
  f (Status (realToFrac $ c'PaStreamCallbackTimeInfo'currentTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'inputBufferAdcTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'outputBufferDacTime info)
      (testBit flags 0)
      (testBit flags 1)
      (testBit flags 2)
      (testBit flags 3)
      (testBit flags 4))
    (V.unsafeFromForeignPtr0 ip n)
    (MV.unsafeFromForeignPtr0 op n)
  return c'paContinue

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing c = c nullPtr
withMaybe (Just a) c = with a c