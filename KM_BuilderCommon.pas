unit KM_BuilderCommon;
interface
uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Collections;


type
  TKMBuildConfiguration = (
    bcUndefined, // Default uninitialized value
    bcDebug,
    bcRelease
  );

const
  BUILD_CONFIG_NAME: array [TKMBuildConfiguration] of string = ('Undefined', 'Debug', 'Release');

type
  TKMBuilderClass = class of TKMBuilder;

  TKMEvent = procedure (aConfig: TKMBuildConfiguration) of object;
  TKMEventStepBegin = reference to procedure (aStep: Integer);
  TKMEventStepDone = reference to procedure (aStep: Integer; aTimeMsec: Integer);

  TKMBuildStep = record
  public
    Caption: string;
    Method: TKMEvent;
    class function New(const aCaption: string; aMethod: TKMEvent): TKMBuildStep; static;
  end;

  TKMBuildScenario = class
  private
    fCaption: string;
    fBuildConfig: TKMBuildConfiguration;
    fSteps: TList<Integer>; // Reference steps
  public
    constructor Create(const aCaption: string; aBuildConfig: TKMBuildConfiguration; aSteps: TArray<Integer>);
    function Contains(aStep: Integer): Boolean;
    property Caption: string read fCaption;
    property BuildConfig: TKMBuildConfiguration read fBuildConfig;
  end;

  TKMBuilder = class
  protected
    fBuildScenarios: TList<TKMBuildScenario>;

    // It is important to keep all steps in a single list for simpler debug. Configurations just reference them
    fBuildSteps: TList<TKMBuildStep>;

    fOnLog: TProc<string>;
    fOnStepBegin: TKMEventStepBegin;
    fOnStepDone: TKMEventStepDone;
    fOnDone: TProc;
    fWorker: TThread;

    procedure DeleteFileIfExists(const aFilename: string);
    procedure DeleteRecursive(const aPath: string; const aFilters: array of string; aAvoid: array of string);
    procedure CopyFile(const aPathFrom, aPathTo: string);
    procedure CopyFilesRecursive(const aPathFrom, aPathTo: string; const aFilter: string; aRecursive: Boolean);
    procedure CopyFolder(const aPathFrom, aPathTo: string);

    procedure CheckFileExists(const aAppName, aFilename: string);
    procedure CheckFileCRC(const aFilename: string; aExpectedCRC: Cardinal);
    procedure CheckFolderExists(const aTitle, aFolder: string);
    function CheckTerminated: Boolean;

    procedure BuildWin(const aRSVars, aProject: string; aConfig: TKMBuildConfiguration; const aExe: string);
    procedure BuildWinGroup(const aRSVars, aGroup: string; aConfig: TKMBuildConfiguration);
    procedure BuildFpc(const aFpcUpDeluxe, aProject, aExe: string);
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc); virtual;
    destructor Destroy; override;

    procedure ExecuteConfig(aScenario: Integer);
    procedure ExecuteStep(aStep: Integer; aConfig: TKMBuildConfiguration);
    procedure ExecuteWholeProjectGroup(aConfig: TKMBuildConfiguration); virtual; abstract;

    // Utility getters
    function GetInfo: string; virtual; abstract;
    function GetScenarioCount: Integer;
    function GetScenarioName(aScenario: Integer): string;
    function GetScenarioBuildConfig(aScenario: Integer): TKMBuildConfiguration;
    function GetScenarioContainsStep(aScenario, aStep: Integer): Boolean;
    function GetStepCount: Integer;
    function GetStepName(aStep: Integer): string;
  end;


implementation
uses
  System.IOUtils, System.Masks, System.DateUtils, System.StrUtils,
  KromUtils;


{ TKMBuildStep }
class function TKMBuildStep.New(const aCaption: string; aMethod: TKMEvent): TKMBuildStep;
begin
  Result.Caption := aCaption;
  Result.Method := aMethod;
end;


{ TKMBuildScenario }
constructor TKMBuildScenario.Create(const aCaption: string; aBuildConfig: TKMBuildConfiguration; aSteps: TArray<Integer>);
begin
  inherited Create;

  fCaption := aCaption;
  fBuildConfig := aBuildConfig;
  fSteps := TList<Integer>.Create;
  fSteps.AddRange(aSteps);
end;


function TKMBuildScenario.Contains(aStep: Integer): Boolean;
begin
  Result := fSteps.Contains(aStep);
end;


{ TKMBuilder }
constructor TKMBuilder.Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited Create;

  fOnLog := aOnLog;
  fOnStepBegin := aOnStepBegin;
  fOnStepDone := aOnStepDone;
  fOnDone := aOnDone;

  fBuildScenarios := TList<TKMBuildScenario>.Create;
  fBuildSteps := TList<TKMBuildStep>.Create;
end;


procedure TKMBuilder.CheckFileExists(const aAppName, aFilename: string);
begin
  // Check that file is available at given path
  if not FileExists(aFilename) then
    raise Exception.Create(Format('%s not found at "%s"', [aAppName, aFilename]));
end;


procedure TKMBuilder.CheckFileCRC(const aFilename: string; aExpectedCRC: Cardinal);
var
  stream: TMemoryStream;
  actualCRC: Cardinal;
begin
  stream := TMemoryStream.Create;
  stream.LoadFromFile(aFilename);
  actualCRC := Adler32CRC(stream);
  if actualCRC <> aExpectedCRC then
    raise Exception.Create(Format('CRC does not match for "%s". Expected %8x, actual %8x', [aFilename, aExpectedCRC, actualCRC]));
  stream.Free;
end;


procedure TKMBuilder.CheckFolderExists(const aTitle, aFolder: string);
begin
  // Check that folder is available at given path
  if not DirectoryExists(aFolder) then
    raise Exception.Create(Format('%s not found at "%s"', [aTitle, aFolder]));
end;


function TKMBuilder.CheckTerminated: Boolean;
begin
  Result := TThread.CheckTerminated;

  if Result then
    fOnLog(Format('Terminated %d', [TThread.CurrentThread.ThreadID]));
end;


procedure TKMBuilder.CopyFile(const aPathFrom, aPathTo: string);
begin
  ForceDirectories(ExtractFilePath(aPathTo));
  TFile.Copy(aPathFrom, aPathTo, True);
  fOnLog('Copied ' + ExtractFileName(aPathTo));
end;


procedure TKMBuilder.CopyFilesRecursive(const aPathFrom, aPathTo, aFilter: string; aRecursive: Boolean);
  procedure Internal(const aPathFrom, aPathTo, aFilter: string; aRecursive: Boolean; var aCount: Integer);
  var
    SearchRec: TSearchRec;
  begin
    if not DirectoryExists(aPathFrom) then Exit;

    if FindFirst(aPathFrom + '*', faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if aRecursive then
            Internal(aPathFrom + SearchRec.Name + '\', aPathTo + SearchRec.Name + '\', aFilter, aRecursive, aCount);
        end else
        begin
          if MatchesMask(SearchRec.Name, aFilter) then
          begin
            ForceDirectories(aPathTo);
            TFile.Copy(aPathFrom + SearchRec.Name, aPathTo + SearchRec.Name, True);

            fOnLog(aPathFrom + SearchRec.Name);
            Inc(aCount);
          end;
        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
begin
  var cnt := 0;
  Internal(aPathFrom, aPathTo, aFilter, aRecursive, cnt);
  fOnLog(Format('Copied %d files', [cnt]));
end;


procedure TKMBuilder.CopyFolder(const aPathFrom, aPathTo: string);
begin
  TDirectory.Copy(aPathFrom, aPathTo);
  fOnLog('Copied ' + ExtractFileName(ExcludeTrailingPathDelimiter(aPathFrom)) + '\');
end;


procedure TKMBuilder.DeleteFileIfExists(const aFilename: string);
begin
  if FileExists(aFilename) then
  begin
    fOnLog('Deleting ' + aFilename);

    if not DeleteFile(aFilename) then
      raise Exception.Create(Format('Failed to delete "%s". Is it locked by something?', [aFilename]));
  end;
end;

procedure TKMBuilder.DeleteRecursive(const aPath: string; const aFilters: array of string; aAvoid: array of string);
  procedure Internal(const aPath: string; const aFilters: array of string; aAvoid: array of string; var aCount: Integer);
  var
    SearchRec: TSearchRec;
  begin
    if not DirectoryExists(aPath) then Exit;

    if FindFirst(aPath + '*', faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        if MatchText(SearchRec.Name, aAvoid) then
          Continue;

        var fullName := aPath + SearchRec.Name;

        // SymLinks need to be unrolled
        var s := '';
        var b := TFile.GetSymLinkTarget(fullName, s);
        if b then fullName := s;

        for var Filter in aFilters do
        if MatchesMask(SearchRec.Name, Filter) then
        begin
          if (SearchRec.Attr and faDirectory) <> 0 then
            TDirectory.Delete(fullName, True)
          else
            TFile.Delete(fullName);

          fOnLog(Format('Deleted "%s"', [fullName]));
          Inc(aCount);
        end;

        if (SearchRec.Attr and faDirectory) <> 0 then
          Internal(fullName + '\', aFilters, aAvoid, aCount);
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
begin
  var cnt := 0;
  Internal(aPath, aFilters, aAvoid, cnt);
  fOnLog(Format('Deleted %d files and folders', [cnt]));
end;


destructor TKMBuilder.Destroy;
begin
  FreeAndNil(fWorker);
  inherited;
end;


procedure TKMBuilder.BuildWin(const aRSVars, aProject: string; aConfig: TKMBuildConfiguration; const aExe: string);
begin
  DeleteFileIfExists(aExe);
  CheckFileExists('RSVars', aRSVars);

  Assert(aConfig <> bcUndefined);

  fOnLog(Format('Building %s (%s)', [aExe, BUILD_CONFIG_NAME[aConfig]]));
  begin
    var s := Format('cmd.exe /C "CALL "%s" && MSBUILD "%s" /p:Config=%s /t:Build /clp:ErrorsOnly /fl"', [aRSVars, aProject, BUILD_CONFIG_NAME[aConfig]]);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;

  CheckFileExists('Resulting exe', aExe);
end;


procedure TKMBuilder.BuildWinGroup(const aRSVars, aGroup: string; aConfig: TKMBuildConfiguration);
begin
  CheckFileExists('RSVars', aRSVars);

  fOnLog('Building group ' + aGroup);
  begin
    var s := Format('cmd.exe /C "CALL "%s" && MSBUILD "%s" /p:Config=%s /t:Build /clp:ErrorsOnly /fl"', [aRSVars, aGroup, BUILD_CONFIG_NAME[aConfig]]);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
end;


procedure TKMBuilder.BuildFpc(const aFpcUpDeluxe, aProject, aExe: string);
begin
  DeleteFileIfExists(aExe);

  CheckFileExists('FPCUPdeluxe', aFpcUpDeluxe);

  fOnLog('Building ' + aExe);
  begin
    var cmdFpc := Format('cmd.exe /C "CALL "%s" -q "%s""', [aFpcUpDeluxe, aProject]);
    var res := CaptureConsoleOutput('.\', cmdFpc);
    fOnLog(res);
  end;

  CheckFileExists('Resulting binary', aExe);
end;


function TKMBuilder.GetScenarioCount: Integer;
begin
  Result := fBuildScenarios.Count;
end;


function TKMBuilder.GetScenarioName(aScenario: Integer): string;
begin
  Result := fBuildScenarios[aScenario].Caption;
end;


function TKMBuilder.GetScenarioBuildConfig(aScenario: Integer): TKMBuildConfiguration;
begin
  if aScenario <> -1 then
    Result := fBuildScenarios[aScenario].BuildConfig
  else
    Result := bcUndefined;
end;


function TKMBuilder.GetScenarioContainsStep(aScenario, aStep: Integer): Boolean;
begin
  Result := fBuildScenarios[aScenario].Contains(aStep);
end;


function TKMBuilder.GetStepCount: Integer;
begin
  Result := fBuildSteps.Count;
end;


function TKMBuilder.GetStepName(aStep: Integer): string;
begin
  Result := fBuildSteps[aStep].Caption;
end;


procedure TKMBuilder.ExecuteConfig(aScenario: Integer);
var
  thisScenario: Integer;
begin
  // Try to capture into local variable, just in case
  thisScenario := aScenario;

  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fOnLog(Format('Starting builder thread %d', [TThread.CurrentThread.ThreadID]));

        for var I := 0 to fBuildSteps.Count - 1 do
        if GetScenarioContainsStep(thisScenario, I) then
        begin
          fOnStepBegin(I);

          var tickBegin := GetTickCount;

          fBuildSteps[I].Method(GetScenarioBuildConfig(thisScenario));

          fOnStepDone(I, GetTickCount - tickBegin);

          if CheckTerminated then
            Exit;
        end;
        fOnDone;
      except
        on E: Exception do
          fOnLog('ERROR: ' + E.Message);
      end;
    end);

  fWorker.FreeOnTerminate := False;
  fWorker.Start;
end;


procedure TKMBuilder.ExecuteStep(aStep: Integer; aConfig: TKMBuildConfiguration);
var
  thisStep: Integer;
  thisConfig: TKMBuildConfiguration;
begin
  // Try to capture into local variable, just in case
  thisStep := aStep;
  thisConfig := aConfig;

  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fOnLog(Format('Starting builder thread %d', [TThread.CurrentThread.ThreadID]));

        fOnStepBegin(thisStep);

        var tickBegin := GetTickCount;

        fBuildSteps[thisStep].Method(thisConfig);

        fOnStepDone(thisStep, GetTickCount - tickBegin);

        fOnDone;
      except
        on E: Exception do
          fOnLog('ERROR: ' + E.Message);
      end;
    end);

  fWorker.FreeOnTerminate := False;
  fWorker.Start;
end;


end.
