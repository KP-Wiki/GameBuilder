unit KromUtils;
interface


function CreateProcessSimple(const aFilename: string; aShowWindow, aWait, aLowPriority: Boolean): NativeUInt;
procedure TerminateProcessSimple(aProcessHandle: NativeUInt);
function CaptureConsoleOutput(const aFolder, aString: WideString): string;


implementation
uses
  System.Math, System.SysUtils, Winapi.Windows;


function CreateProcessSimple(const aFilename: string; aShowWindow, aWait, aLowPriority: Boolean): NativeUInt;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  res: Cardinal;
  CmdLine: array[0..255] of WideChar;
begin
  // We need strings to be modifiable, since CreateProcessW plays "dirty" and crashes otherwise
  // Hence we have to make local copy
  StringToWideChar(aFilename, CmdLine, Length(CmdLine));

  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := IfThen(aShowWindow, SW_SHOWDEFAULT, SW_HIDE);

  if not CreateProcess(
    nil,
    CmdLine,
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS or (BELOW_NORMAL_PRIORITY_CLASS * Ord(aLowPriority)),
    nil,
    nil,
    StartupInfo,
    ProcessInfo) then
    RaiseLastOSError;

  Result := ProcessInfo.hProcess;

  if aWait then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, res);
    Result := 0;
  end;
end;


procedure TerminateProcessSimple(aProcessHandle: NativeUInt);
begin
  TerminateProcess(aProcessHandle, 0);
end;


function CaptureConsoleOutput(const aFolder, aString: WideString): string;
  function IfThen(aCondition: Boolean; aTrue, aFalse: PWideChar): PWideChar;
  begin
    if aCondition then
      Result := aTrue
    else
      Result := aFalse;
  end;
const
  CReadBuffer = 2400;
var
  thisFolder, thisString: WideString;
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWord;
  dRunning: DWord;
begin
  Result := '';

  // We need strings to be modifiable, since CreateProcessW plays "dirty" and crashes otherwise
  // Hence we have to make local copy
  thisFolder := aFolder;
  thisString := aString;

  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := True;
  saSecurity.lpSecurityDescriptor := nil;

  if not CreatePipe(hRead, hWrite, @saSecurity, 0) then
    RaiseLastOSError;

  suiStartup := default(TStartupInfo);
  suiStartup.cb := SizeOf(TStartupInfo);
  suiStartup.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  suiStartup.hStdOutput := hWrite;
  suiStartup.hStdError := hWrite;
  suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  suiStartup.wShowWindow := SW_SHOW;//SW_HIDE;

  piProcess := default(TProcessInformation);

  try
    if not CreateProcessW(nil, PWideChar(thisString), @saSecurity, @saSecurity, True, CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS,
      nil, IfThen(thisFolder <> '', PWideChar(thisFolder), nil), suiStartup, piProcess) then
      RaiseLastOSError;

    // Freezes without the pause ..
    Sleep(100);
    CloseHandle(hWrite);

    try
      repeat
        dRunning := WaitForSingleObject(piProcess.hProcess, 100);

        repeat
          dRead := 0;
          if ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil) then
          begin
            pBuffer[dRead] := #0;
            //OemToAnsi(pBuffer, pBuffer);
            Result := Result + String(pBuffer);
          end;
        until (dRead < CReadBuffer);
      until (dRunning <> WAIT_TIMEOUT);
    finally
      CloseHandle(piProcess.hProcess);
      CloseHandle(piProcess.hThread);
    end;
  finally
    CloseHandle(hRead);
  end;
end;


end.
