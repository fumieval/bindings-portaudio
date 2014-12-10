bindings-portaudio
==================

Installation on Windows
------------------------
1. Download and unpack the latest [portaudio](http://www.portaudio.com/download.html) at a clear directory (e.g. `C:\portaudio`).
2. Run `bash configure`, then `make`.
3. Edit `portaudio-2.0` as follows:
```
--- prefix=/usr/local
+++ prefix=C:/portaudio

--- libdir=${exec_prefix}/lib
+++ libdir=${exec_prefix}/lib/.libs
```
4. If you don't have pkgconfig, download `pkg-config` from [GTK+ Download: Windows (32-bit)](http://www.gtk.org/download/win32.php) and make sure `pkg-config.exe` is in your `PATH`.
5. `set PKG_CONFIG_PATH=C:/portaudio`
6. Run `cabal update && cabal install bindings-portaudio`.