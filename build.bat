if "%VSCMD_VER%"=="" call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build\vcvarsall.bat" x64
cl *.c /D_CRT_SECURE_NO_DEPRECATE /Fe: prettysharp.exe /W4 /O2 /WX
