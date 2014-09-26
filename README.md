bindings-portaudio
==================

Why not [portaudio](http://hackage.haskell.org/package/portaudio) package?
----
On Windows, bindings-portaudio contains the original [portaudio](http://www.portaudio.com/) so that you don't need to build portaudio by hand.

Compilation
----
Windows:

    cabal install [-fWASAPI or -fWMME or -fWDMKS]

or

    cabal install -fMinGW-External \
        --extra-include-dirs=C:\Path\To\portaudio\include \
        --extra-lib-dirs=\C:\Path\To\portaudio\lib\.libs \


Mac & Linux:

    cabal install

Supported API Matrix
----
           | Windows | Linux  | Mac
-----------| ------- | -------| --------
WASAPI     | OK      |        |
DirectSound| WIP     |        |
WMME       | OK      |        |
WDM K.S.   | OK      |        |
