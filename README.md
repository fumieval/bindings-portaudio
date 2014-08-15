bindings-portaudio
==================

Why not [portaudio](http://hackage.haskell.org/portaudio) package?
---
bindings-portaudio contains the original [portaudio](http://www.portaudio.com/) itself so that you don't need to build portaudio by hand.

Supported API Matrix
----
           | Windows | Linux  | Mac
-----------| ------- | -------| --------
WASAPI     | OK      |        |
DirectSound| Unknown |        |
WMME       | OK      |        |
WDM K.S.   | WIP     |        |
ALSA       |         | OK     |
OSS        |         | WIP    |
JACK       |         | WIP    |
CoreAudio  |         |        | Uncertain
