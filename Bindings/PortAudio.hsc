{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <portaudio.h>
module Bindings.PortAudio where
import Foreign.Ptr
#strict_import

#synonym_t PaStream, ()

#ccall Pa_GetVersion , IO CInt
#ccall Pa_GetVersionText , IO CString
{- typedef int PaError; -}
#synonym_t PaError , CInt
{- typedef enum PaErrorCode {
            paNoError = 0,
            paNotInitialized = -10000,
            paUnanticipatedHostError,
            paInvalidChannelCount,
            paInvalidSampleRate,
            paInvalidDevice,
            paInvalidFlag,
            paSampleFormatNotSupported,
            paBadIODeviceCombination,
            paInsufficientMemory,
            paBufferTooBig,
            paBufferTooSmall,
            paNullCallback,
            paBadStreamPtr,
            paTimedOut,
            paInternalError,
            paDeviceUnavailable,
            paIncompatibleHostApiSpecificStreamInfo,
            paStreamIsStopped,
            paStreamIsNotStopped,
            paInputOverflowed,
            paOutputUnderflowed,
            paHostApiNotFound,
            paInvalidHostApi,
            paCanNotReadFromACallbackStream,
            paCanNotWriteToACallbackStream,
            paCanNotReadFromAnOutputOnlyStream,
            paCanNotWriteToAnInputOnlyStream,
            paIncompatibleStreamHostApi,
            paBadBufferPtr
        } PaErrorCode; -}
#integral_t enum PaErrorCode
#num paNoError
#num paNotInitialized
#num paUnanticipatedHostError
#num paInvalidChannelCount
#num paInvalidSampleRate
#num paInvalidDevice
#num paInvalidFlag
#num paSampleFormatNotSupported
#num paBadIODeviceCombination
#num paInsufficientMemory
#num paBufferTooBig
#num paBufferTooSmall
#num paNullCallback
#num paBadStreamPtr
#num paTimedOut
#num paInternalError
#num paDeviceUnavailable
#num paIncompatibleHostApiSpecificStreamInfo
#num paStreamIsStopped
#num paStreamIsNotStopped
#num paInputOverflowed
#num paOutputUnderflowed
#num paHostApiNotFound
#num paInvalidHostApi
#num paCanNotReadFromACallbackStream
#num paCanNotWriteToACallbackStream
#num paCanNotReadFromAnOutputOnlyStream
#num paCanNotWriteToAnInputOnlyStream
#num paIncompatibleStreamHostApi
#num paBadBufferPtr
#ccall Pa_GetErrorText , CInt -> IO CString
#ccall Pa_Initialize , IO CInt
#ccall Pa_Terminate , IO CInt
{- typedef int PaDeviceIndex; -}
#synonym_t PaDeviceIndex , CInt
{- typedef int PaHostApiIndex; -}
#synonym_t PaHostApiIndex , CInt
#ccall Pa_GetHostApiCount , IO CInt
#ccall Pa_GetDefaultHostApi , IO CInt
{- typedef enum PaHostApiTypeId {
            paInDevelopment = 0,
            paDirectSound = 1,
            paMME = 2,
            paASIO = 3,
            paSoundManager = 4,
            paCoreAudio = 5,
            paOSS = 7,
            paALSA = 8,
            paAL = 9,
            paBeOS = 10,
            paWDMKS = 11,
            paJACK = 12,
            paWASAPI = 13,
            paAudioScienceHPI = 14
        } PaHostApiTypeId; -}
#integral_t enum PaHostApiTypeId
#num paInDevelopment
#num paDirectSound
#num paMME
#num paASIO
#num paSoundManager
#num paCoreAudio
#num paOSS
#num paALSA
#num paAL
#num paBeOS
#num paWDMKS
#num paJACK
#num paWASAPI
#num paAudioScienceHPI
{- typedef struct PaHostApiInfo {
            int structVersion;
            PaHostApiTypeId type;
            const char * name;
            int deviceCount;
            PaDeviceIndex defaultInputDevice;
            PaDeviceIndex defaultOutputDevice;
        } PaHostApiInfo; -}
#starttype struct PaHostApiInfo
#field structVersion , CInt
#field type , <enum PaHostApiTypeId>
#field name , CString
#field deviceCount , CInt
#field defaultInputDevice , CInt
#field defaultOutputDevice , CInt
#stoptype
#ccall Pa_GetHostApiInfo , CInt -> IO (Ptr <struct PaHostApiInfo>)
#ccall Pa_HostApiTypeIdToHostApiIndex , <enum PaHostApiTypeId> -> IO CInt
#ccall Pa_HostApiDeviceIndexToDeviceIndex , CInt -> CInt -> IO CInt
{- typedef struct PaHostErrorInfo {
            PaHostApiTypeId hostApiType;
            long errorCode;
            const char * errorText;
        } PaHostErrorInfo; -}
#starttype struct PaHostErrorInfo
#field hostApiType , <enum PaHostApiTypeId>
#field errorCode , CLong
#field errorText , CString
#stoptype
#ccall Pa_GetLastHostErrorInfo , IO (Ptr <struct PaHostErrorInfo>)
#ccall Pa_GetDeviceCount , IO CInt
#ccall Pa_GetDefaultInputDevice , IO CInt
#ccall Pa_GetDefaultOutputDevice , IO CInt
{- typedef double PaTime; -}
#synonym_t PaTime , CDouble
{- typedef unsigned long PaSampleFormat; -}
#synonym_t PaSampleFormat , CULong
{- typedef struct PaDeviceInfo {
            int structVersion;
            const char * name;
            PaHostApiIndex hostApi;
            int maxInputChannels;
            int maxOutputChannels;
            PaTime defaultLowInputLatency;
            PaTime defaultLowOutputLatency;
            PaTime defaultHighInputLatency;
            PaTime defaultHighOutputLatency;
            double defaultSampleRate;
        } PaDeviceInfo; -}
#starttype struct PaDeviceInfo
#field structVersion , CInt
#field name , CString
#field hostApi , CInt
#field maxInputChannels , CInt
#field maxOutputChannels , CInt
#field defaultLowInputLatency , CDouble
#field defaultLowOutputLatency , CDouble
#field defaultHighInputLatency , CDouble
#field defaultHighOutputLatency , CDouble
#field defaultSampleRate , CDouble
#stoptype
#ccall Pa_GetDeviceInfo , CInt -> IO (Ptr <struct PaDeviceInfo>)
{- typedef struct PaStreamParameters {
            PaDeviceIndex device;
            int channelCount;
            PaSampleFormat sampleFormat;
            PaTime suggestedLatency;
            void * hostApiSpecificStreamInfo;
        } PaStreamParameters; -}
#starttype struct PaStreamParameters
#field device , CInt
#field channelCount , CInt
#field sampleFormat , CULong
#field suggestedLatency , CDouble
#field hostApiSpecificStreamInfo , Ptr ()
#stoptype
#ccall Pa_IsFormatSupported , Ptr <struct PaStreamParameters> -> Ptr <struct PaStreamParameters> -> CDouble -> IO CInt
{- typedef void PaStream; -}
{- typedef unsigned long PaStreamFlags; -}
#synonym_t PaStreamFlags , CULong
{- typedef struct PaStreamCallbackTimeInfo {
            PaTime inputBufferAdcTime;
            PaTime currentTime;
            PaTime outputBufferDacTime;
        } PaStreamCallbackTimeInfo; -}
#starttype struct PaStreamCallbackTimeInfo
#field inputBufferAdcTime , CDouble
#field currentTime , CDouble
#field outputBufferDacTime , CDouble
#stoptype
{- typedef unsigned long PaStreamCallbackFlags; -}
#synonym_t PaStreamCallbackFlags , CULong
{- typedef enum PaStreamCallbackResult {
            paContinue = 0, paComplete = 1, paAbort = 2
        } PaStreamCallbackResult; -}
#integral_t enum PaStreamCallbackResult
#num paContinue
#num paComplete
#num paAbort

#callback PaStreamCallback, Ptr () -> Ptr () -> CULong -> Ptr C'PaStreamCallbackTimeInfo -> C'PaStreamCallbackFlags -> Ptr () -> IO <PaStreamCallbackResult>

#callback PaStreamFinishedCallback, Ptr () -> IO ()

#ccall Pa_OpenStream , Ptr (Ptr <PaStream>) -> Ptr <struct PaStreamParameters> -> Ptr <struct PaStreamParameters> -> CDouble -> CULong -> CULong -> <PaStreamCallback> -> Ptr () -> IO CInt
#ccall Pa_OpenDefaultStream , Ptr (Ptr <PaStream>) -> CInt -> CInt -> CULong -> CDouble -> CULong -> <PaStreamCallback> -> Ptr () -> IO CInt
#ccall Pa_CloseStream , Ptr <PaStream> -> IO CInt
#ccall Pa_SetStreamFinishedCallback , Ptr <PaStream> -> <PaStreamFinishedCallback> -> IO CInt
#ccall Pa_StartStream , Ptr <PaStream> -> IO CInt
#ccall Pa_StopStream , Ptr <PaStream> -> IO CInt
#ccall Pa_AbortStream , Ptr <PaStream> -> IO CInt
#ccall Pa_IsStreamStopped , Ptr <PaStream> -> IO CInt
#ccall Pa_IsStreamActive , Ptr <PaStream> -> IO CInt
{- typedef struct PaStreamInfo {
            int structVersion;
            PaTime inputLatency;
            PaTime outputLatency;
            double sampleRate;
        } PaStreamInfo; -}
#starttype struct PaStreamInfo
#field structVersion , CInt
#field inputLatency , CDouble
#field outputLatency , CDouble
#field sampleRate , CDouble
#stoptype
#ccall Pa_GetStreamInfo , Ptr <PaStream> -> IO (Ptr <struct PaStreamInfo>)
#ccall Pa_GetStreamTime , Ptr <PaStream> -> IO CDouble
#ccall Pa_GetStreamCpuLoad , Ptr <PaStream> -> IO CDouble
#ccall Pa_ReadStream , Ptr <PaStream> -> Ptr () -> CULong -> IO CInt
#ccall Pa_WriteStream , Ptr <PaStream> -> Ptr () -> CULong -> IO CInt
#ccall Pa_GetStreamReadAvailable , Ptr <PaStream> -> IO CLong
#ccall Pa_GetStreamWriteAvailable , Ptr <PaStream> -> IO CLong
#ccall Pa_GetSampleSize , CULong -> IO CInt
#ccall Pa_Sleep , CLong -> IO ()
