@echo off
IF "%1"=="clean" (
  @echo del ltc_11.exe
  del ltc_11.exe
) ELSE (
  IF "%1"=="setup" (
     SET COMMAND_TO_CHECK=fbc.exe
     WHERE %COMMAND_TO_CHECK% >NUL 2>&1

     IF %ERRORLEVEL% NEQ 0 (
         ECHO %COMMAND_TO_CHECK% was not found in the PATH.
         REM PATH=%PATH%;C:\Users\LabRat\Documents\Ltc\Sources\FreeBASIC-1.10.1-win64
     ) ELSE (
         ECHO %COMMAND_TO_CHECK% already exists in the PATH.
     )
  ) ELSE (
    IF "%1"=="test" (
        REM ltc_11.exe #FDL075 WelcomeBF.led #FRM 
        ltc_c.exe #FDL075 WelcomeBF.led #FRM 
    ) ELSE (
      @echo gcc LTC_UDP_C.c -o ltc_c.exe -lws2_32
      gcc LTC_UDP_C.c -o ltc_c.exe -lws2_32
      REM @echo fbc ltc_11.bas
      REM  ..\FreeBASIC-1.10.1-win64\fbc ltc_11.bas
    )
  )
)
