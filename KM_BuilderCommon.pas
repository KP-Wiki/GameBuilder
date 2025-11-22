unit KM_BuilderCommon;
interface
uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Collections;


type
  TKMBuilderGame = (
    bgKMR,
    bgKP
  );

  TKMEvent = procedure of object;
  TKMEventStepBegin = reference to procedure (aStep: Integer);
  TKMEventStepDone = reference to procedure (aStep: Integer; aTimeMsec: Integer);

  TKMBuildStep = record
  public
    Caption: string;
    Method: TKMEvent;
    class function New(const aCaption: string; aMethod: TKMEvent): TKMBuildStep; static;
  end;

  TKMBuildConfig = class
  private
    fCaption: string;
    fSteps: TList<Integer>; // Reference steps
  public
    constructor Create(const aCaption: string; aSteps: TArray<Integer>);
    function Contains(aStep: Integer): Boolean;
    property Caption: string read fCaption;
  end;

  TKMBuilder = class
  protected
    fBuildConfigs: TList<TKMBuildConfig>;

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
    function CheckTerminated: Boolean;
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
    procedure Perform(aConfig: Integer);
    procedure PerformStep(aStep: Integer);
    procedure Stop;

    function GetInfo: string; virtual; abstract;
    function GetConfigCount: Integer;
    function GetConfigName(aConfig: Integer): string;
    function GetConfigContainsStep(aConfig, aStep: Integer): Boolean;
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


{ TKMBuildConfig }
constructor TKMBuildConfig.Create(const aCaption: string; aSteps: TArray<Integer>);
begin
  inherited Create;

  fCaption := aCaption;
  fSteps := TList<Integer>.Create;
  fSteps.AddRange(aSteps);
end;


function TKMBuildConfig.Contains(aStep: Integer): Boolean;
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

  fBuildConfigs := TList<TKMBuildConfig>.Create;
  fBuildSteps := TList<TKMBuildStep>.Create;
end;


procedure TKMBuilder.CheckFileExists(const aAppName, aFilename: string);
begin
  // Check that file is available at given path
  if not FileExists(aFilename) then
    raise Exception.Create(Format('%s not found at "%s"', [aAppName, aFilename]));
end;


function TKMBuilder.CheckTerminated: Boolean;
begin
  Result := TThread.CheckTerminated;

  if Result then
    fOnLog('Terminated');
end;


procedure TKMBuilder.CopyFile(const aPathFrom, aPathTo: string);
begin
  ForceDirectories(ExtractFilePath(aPathTo));
  TFile.Copy(aPathFrom, aPathTo);
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
            TFile.Copy(aPathFrom + SearchRec.Name, aPathTo + SearchRec.Name);

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

          fOnLog(fullName);
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


function TKMBuilder.GetConfigCount: Integer;
begin
  Result := fBuildConfigs.Count;
end;


function TKMBuilder.GetConfigName(aConfig: Integer): string;
begin
  Result := fBuildConfigs[aConfig].Caption;
end;


function TKMBuilder.GetConfigContainsStep(aConfig, aStep: Integer): Boolean;
begin
  Result := fBuildConfigs[aConfig].Contains(aStep);
end;


function TKMBuilder.GetStepCount: Integer;
begin
  Result := fBuildSteps.Count;
end;


function TKMBuilder.GetStepName(aStep: Integer): string;
begin
  Result := fBuildSteps[aStep].Caption;
end;


procedure TKMBuilder.Perform(aConfig: Integer);
var
  thisConfig: Integer;
begin
  // Try to capture into local variable, just in case
  thisConfig := aConfig;

  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        for var I := 0 to fBuildSteps.Count - 1 do
        if GetConfigContainsStep(thisConfig, I) then
        begin
          fOnStepBegin(I);

          var t := GetTickCount;

          fBuildSteps[I].Method;

          fOnStepDone(I, GetTickCount - t);

          if CheckTerminated then
            Exit;
        end;
        fOnDone;
      except
        on E: Exception do
          fOnLog('ERROR: ' + E.Message);
      end;
    end);

  fWorker.Start;
end;


procedure TKMBuilder.PerformStep(aStep: Integer);
var
  thisStep: Integer;
begin
  // Try to capture into local variable, just in case
  thisStep := aStep;

  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fOnStepBegin(thisStep);

        var t := GetTickCount;

        fBuildSteps[thisStep].Method;

        fOnStepDone(thisStep, GetTickCount - t);

        fOnDone;
      except
        on E: Exception do
          fOnLog('ERROR: ' + E.Message);
      end;
    end);

  fWorker.Start;
end;


procedure TKMBuilder.Stop;
begin
  if fWorker = nil then Exit;

  fWorker.Terminate;
  fWorker := nil;

  // Worker will terminate when it can
end;


end.
