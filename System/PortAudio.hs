{-# LANGUAGE CPP, ViewPatterns, FlexibleContexts, ScopedTypeVariables, KindSignatures, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module System.PortAudio(
  -- * Initialization
  withPortAudio
  , Error(..)
  -- * Devices
  , getDevices
  , Device(..)
  , Input
  , Output
  -- * Opening a stream
  , Stream
  , withStream
  , StreamCallbackResult(..)
  , startStream
  , stopStream
  , withStartStream
  , isStreamStopped
  , setStreamFinishedCallback
  -- * Stream parameters
  , StreamParameters
  , streamParameters
  , PortAudioSample
  , noConnection
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

instance Semigroup StreamFlags where
  StreamFlags a <> StreamFlags b = StreamFlags (a .|. b)

instance Monoid StreamFlags where
  mempty = StreamFlags 0

clipOff :: StreamFlags
clipOff = StreamFlags 0x00000001

ditherOff :: StreamFlags
ditherOff = StreamFlags 0x00000002

neverDropInput :: StreamFlags
neverDropInput = StreamFlags 0x00000004

primeOutputBuffersUsingStreamCallback :: StreamFlags
primeOutputBuffersUsingStreamCallback = StreamFlags 0x00000008

wrap :: IO CInt -> IO ()
wrap n = do
  r <- n
  unless (r == 0) $ throwIO $ fromErrorCode r

withPortAudio :: IO a -> IO a
withPortAudio = bracket_ (wrap c'Pa_Initialize) (wrap c'Pa_Terminate)

data StreamCallbackResult = Continue | Complete | Abort deriving (Show, Eq, Ord, Enum)

instance Semigroup StreamCallbackResult where
  Complete <> x = x
  x <> Complete = x
  Abort <> _ = Abort
  _ <> Abort = Abort
  Continue <> Continue = Continue

instance Monoid StreamCallbackResult where
  mempty = Complete

data Status = Status
  { currentTime :: !Double
  , inputBufferAdcTime :: !Double
  , outputBufferDacTime :: !Double
  , inputUnderflow :: !Bool
  , inputOverflow :: !Bool
  , outputUnderflow :: !Bool
  , outputOverflow :: !Bool
  , primingOutput :: !Bool } deriving (Show, Eq, Ord)

newtype Stream = Stream { unStream :: Ptr C'PaStream }

withStream :: (Storable i, Storable o)
  => Double -- ^ sampling rate
  -> Int -- ^ buffer size
  -> Maybe (StreamParameters Input i)
  -> Maybe (StreamParameters Output o)
  -> StreamFlags
  -> (Status -> V.Vector i -> MV.IOVector o -> IO StreamCallbackResult) -- ^ callback
  -> (Stream -> IO r)
  -> IO r
withStream rate buf paramI paramO (StreamFlags flags) f m =
  withMaybe paramI $ \pin -> withMaybe paramO $ \pout -> do
    cb <- mk'PaStreamCallback $ callback f
    let opener ps = do
          wrap $ c'Pa_OpenStream ps
              (castPtr pin)
              (castPtr pout)
              (CDouble rate)
              (fromIntegral buf)
              flags
              cb
              nullPtr
          peek ps
    alloca $ \ps -> bracket (opener ps) (wrap . c'Pa_CloseStream) $ m . Stream

startStream :: Stream -> IO ()
startStream = wrap . c'Pa_StartStream . unStream

stopStream :: Stream -> IO ()
stopStream = wrap . c'Pa_StopStream . unStream

setStreamFinishedCallback :: Stream -> IO () -> IO ()
setStreamFinishedCallback (Stream s) m = do
  cb <- mk'PaStreamFinishedCallback (const m)
  wrap $ c'Pa_SetStreamFinishedCallback s cb

isStreamStopped :: Stream -> IO Bool
isStreamStopped = fmap (==1) . c'Pa_IsStreamStopped . unStream

withStartStream :: Stream -> IO r -> IO r
withStartStream s = bracket_ (startStream s) (stopStream s)

-- | This is 'Nothing', but it explicitly specifies the stream type with zero-width unit type.
noConnection :: Maybe (StreamParameters t ())
noConnection = Nothing

callback :: (Storable a, Storable b) => (Status -> V.Vector a -> MV.IOVector b -> IO StreamCallbackResult) -> Ptr () -> Ptr () -> CULong -> Ptr C'PaStreamCallbackTimeInfo -> CULong -> z -> IO CUInt
callback f (castPtr -> pin) (castPtr -> pout) (fromIntegral -> n) pinfo flags _ = do
  ip <- newForeignPtr_ pin
  op <- newForeignPtr_ pout
  info <- peek pinfo
  fmap (toEnum . fromEnum) $ f (Status (realToFrac $ c'PaStreamCallbackTimeInfo'currentTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'inputBufferAdcTime info)
      (realToFrac $ c'PaStreamCallbackTimeInfo'outputBufferDacTime info)
      (testBit flags 0)
      (testBit flags 1)
      (testBit flags 2)
      (testBit flags 3)
      (testBit flags 4))
    (V.unsafeFromForeignPtr0 ip n)
    (MV.unsafeFromForeignPtr0 op n)

withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing c = c nullPtr
withMaybe (Just a) c = with a c
