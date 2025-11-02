unit KM_Builder;
{$I ..\..\KM_CompilerDirectives.inc}
interface
uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Defaults;


type
  TKMBuilderStep = (
    bsStartBuild,
    bsCleanSource,
    bsBuildExe,
    bsPatchExe,
    bsPackData,
    bsCopy
  );
  TKMBuilderStepSet = set of TKMBuilderStep;

const
  BuilderStepName: array [TKMBuilderStep] of string = (
    'Start build',
    'Clean sources',
    'Build executables',
    'Patch game executable',
    'Packing data',
    'Copy into build folder'
  );

type
  TKMBuilder = class
  private
    fOnLog: TProc<string>;
    fOnStepBegin: TProc<TKMBuilderStep>;
    fOnStepDone: TProc<TKMBuilderStep, Integer>;
    fOnDone: TProc;
    fWorker: TThread;

    fBuildVersion: string;
    fBuildRevision: Integer;
    fBuildFolder: string;

    procedure DeleteRecursive(const aPath: string; const aFilters: array of string; aAvoid: array of string);
    procedure CopyFile(const aPathFrom, aPathTo: string);
    procedure CopyFilesRecursive(const aPathFrom, aPathTo: string; const aFilter: string; aRecursive: Boolean);
    procedure CopyFolder(const aPathFrom, aPathTo: string);

    function CheckTerminated: Boolean;

    procedure Step1_GetRevisionCommitAndTag;
    procedure Step2_CleanSource;
    procedure Step3_BuildGameExe;
    procedure Step4_PatchGameExe;
    procedure Step5_PackData;
    procedure Step6_CopyIntoBuildFolder;
  public
    constructor Create(const aBuildVersion: string; aOnLog: TProc<string>; aOnStepBegin: TProc<TKMBuilderStep>; aOnStepDone: TProc<TKMBuilderStep, Integer>; aOnDone: TProc);
    procedure Perform(aSteps: TKMBuilderStepSet);
    procedure Stop;
  end;


implementation
uses
  System.IOUtils, System.Masks, System.DateUtils,
  System.StrUtils, System.Generics.Collections,
  KromUtils;


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


{ TKMBuilder }
constructor TKMBuilder.Create(const aBuildVersion: string; aOnLog: TProc<string>; aOnStepBegin: TProc<TKMBuilderStep>; aOnStepDone: TProc<TKMBuilderStep, Integer>; aOnDone: TProc);
begin
  inherited Create;


  fOnLog := aOnLog;
  fOnStepBegin := aOnStepBegin;
  fOnStepDone := aOnStepDone;
  fOnDone := aOnDone;

  fBuildVersion := aBuildVersion;
end;


procedure TKMBuilder.Stop;
begin
  if fWorker = nil then Exit;

  fWorker.Terminate;
  fWorker := nil;
end;


procedure TKMBuilder.Step1_GetRevisionCommitAndTag;
begin
  fOnLog('rev-list ..');
  var cmdRevList := Format('cmd.exe /C "@FOR /F "USEBACKQ tokens=*" %%F IN (`git rev-list --count HEAD`) DO @ECHO %%F"', []);
  var res := CaptureConsoleOutput('.\', cmdRevList);
  fOnLog(res);
  fBuildRevision := StrToInt(Trim(res)) - 8500;
  fOnLog(Format('Rev number - %d', [fBuildRevision]));

  if CheckTerminated then Exit;

  // Write revision number for game exe and launcher/updater
  TFile.WriteAllText('.\KM_Revision.inc', #39 + 'r' + IntToStr(fBuildRevision) + #39);
  TFile.WriteAllText('.\version', fBuildVersion + ' r' + IntToStr(fBuildRevision));

  if CheckTerminated then Exit;

  fOnLog('commit ..');
  var cmdCommit := Format('git commit -m "New version %d" -- "KM_Revision.inc"', [fBuildRevision]);
  CreateProcessSimple(cmdCommit, False, True, False);

  if CheckTerminated then Exit;

  fOnLog('tag ..');
  var cmdTag := Format('git tag r%d', [fBuildRevision]);
  CreateProcessSimple(cmdTag, False, True, False);

  var dtNow := Now;
  fBuildFolder := Format('kp%.4d-%.2d-%.2d (%s r%d)\', [YearOf(dtNow), MonthOf(dtNow), DayOf(dtNow), fBuildVersion, fBuildRevision]);

  fOnLog(Format('BuildFolder - "%s"', [fBuildFolder]));
end;


procedure TKMBuilder.Step2_CleanSource;
begin
  // Delete folders
  DeleteRecursive(ExpandFileName('.\'), ['__history', '__recovery', 'backup', 'logs', 'dcu'], ['.git']);

  if CheckTerminated then Exit;

  // Delete files
  DeleteRecursive(ExpandFileName('.\'), [
    '*.~*', '*.ddp', '*.drc', '*.dcp', '*.dcu', '*.dsk', '*.o', '*.or', '*.ppu', '*.compiled', '*.local', '*.tmp', '*.log',
    'thumbd.db', 'descript.ion', 'bugreport.txt', '*.skincfg', '*.identcache', '*.tvsconfig', '*.mi', '*.log.txt', '*.stat', '*.bak'], ['.git']);
end;


procedure TKMBuilder.Step3_BuildGameExe;
begin
  // Delete previously built exes

  fOnLog('Building KnightsProvince.exe');
  begin
    var s := Format('cmd.exe /C "CALL bat_rsvars.bat && MSBUILD "%s" /p:Config=Release /t:Build /clp:ErrorsOnly /fl"', ['KnightsProvince.dproj']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('KnightsProvince.exe') then
    fOnLog('KnightsProvince.exe not found');

  if CheckTerminated then Exit;

  fOnLog('Building ScriptValidator.exe');
  begin
    var s := Format('cmd.exe /C "CALL bat_rsvars.bat && MSBUILD "%s" /p:Config=Release /t:Build /clp:ErrorsOnly /fl"', ['utils\ScriptValidator\ScriptValidator.dproj']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('ScriptValidator.exe') then
    fOnLog('ScriptValidator.exe not found');

  if CheckTerminated then Exit;

  fOnLog('Building TranslationManager.exe');
  begin
    var s := Format('cmd.exe /C "CALL bat_rsvars.bat && MSBUILD "%s" /p:Config=Release /t:Build /clp:ErrorsOnly /fl"', ['utils\TranslationManager (from kp-wiki)\TranslationManager.dproj']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('utils\TranslationManager (from kp-wiki)\TranslationManager.exe') then
    fOnLog('TranslationManager.exe not found');

  if CheckTerminated then Exit;

  fOnLog('Building KP_DedicatedServer.exe');
  begin
    var s := Format('cmd.exe /C "CALL bat_rsvars.bat && MSBUILD "%s" /p:Config=Release /t:Build /clp:ErrorsOnly /fl"', ['utils\KP_DedicatedServer\KP_DedicatedServer.dproj']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('utils\KP_DedicatedServer\KP_DedicatedServer.exe') then
    fOnLog('KP_DedicatedServer.exe not found');

  if CheckTerminated then Exit;

  fOnLog('Building KP_DedicatedServer x86');
  begin
    var s := Format('cmd.exe /C "CALL "C:\fpcupdeluxe\lazarus\lazbuild.exe" -q "%s""', ['utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86.lpi']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86') then
    fOnLog('KP_DedicatedServer_Linux_x86 not found');

  if CheckTerminated then Exit;

  fOnLog('Building KP_DedicatedServer x64');
  begin
    var s := Format('cmd.exe /C "CALL "C:\fpcupdeluxe\lazarus\lazbuild.exe" -q "%s""', ['utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64.lpi']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64') then
    fOnLog('KP_DedicatedServer_Linux_x64 not found');
end;


procedure TKMBuilder.Step4_PatchGameExe;
begin
  var exeSizeBefore := TFile.GetSize('KnightsProvince.exe');
  fOnLog(Format('Size before patch - %d bytes', [exeSizeBefore]));

  fOnLog('Patching KnightsProvince.exe');
  begin
    var s := Format('cmd.exe /C ""C:\Program Files (x86)\madCollection\madExcept\Tools\madExceptPatch.exe" "%s""', ['KnightsProvince.exe']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('KnightsProvince.exe') then
    fOnLog('KnightsProvince.exe not found');

  var exeSizeAfter := TFile.GetSize('KnightsProvince.exe');
  fOnLog(Format('Size after patch - %d bytes', [exeSizeAfter]));
end;


procedure TKMBuilder.Step5_PackData;
begin
  fOnLog('Packing data.pack');
  begin
    var s := Format('cmd.exe /C "CALL DataPacker.exe %s"', ['pack']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;
  if not FileExists('data.pack') then
    fOnLog('data.pack not found');

  var szAfter := TFile.GetSize('data.pack');
  fOnLog(Format('Size of data.pack - %d bytes', [szAfter]));
end;


procedure TKMBuilder.Step6_CopyIntoBuildFolder;
begin
  ForceDirectories('.\' + fBuildFolder);

  CopyFolder('.\campaigns\', fBuildFolder + 'campaigns\');
  //CopyFolder('.\ExtAI', fBuildFolder + 'ExtAI\');
  CopyFolder('.\maps\', fBuildFolder + 'maps\');
  CopyFolder('.\mapsdev\', fBuildFolder + 'mapsdev\');
  CopyFolder('.\mods\', fBuildFolder + 'mods\');
  CopyFolder('.\Win32\', fBuildFolder);

  if CheckTerminated then Exit;

  CopyFilesRecursive('.\data\', fBuildFolder + 'data\', '*.libx', True);
  CopyFilesRecursive('.\', fBuildFolder, 'Changelog*.txt', False);

  CopyFile('.\data\locales.xml', fBuildFolder + 'data\locales.xml');
  CopyFile('.\data\text\text_IDs.inc', fBuildFolder + 'data\text\text_IDs.inc');
  CopyFile('.\data.pack', fBuildFolder + 'data.pack');

  CopyFile('.\KnightsProvince.exe', fBuildFolder + 'KnightsProvince.exe');
  CopyFile('.\raudio_x86.dll', fBuildFolder + 'raudio_x86.dll');
  CopyFile('.\Launcher.exe', fBuildFolder + 'Launcher.exe');
  CopyFile('.\hdiffz.dll', fBuildFolder + 'hdiffz.dll');
  CopyFile('.\version', fBuildFolder + 'version');

  if CheckTerminated then Exit;

  CopyFile('.\utils\KP_DedicatedServer\KP_DedicatedServer.exe', fBuildFolder + 'KP_DedicatedServer.exe');
  CopyFile('.\utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86', fBuildFolder + 'KP_DedicatedServer_Linux_x86');
  CopyFile('.\utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64', fBuildFolder + 'KP_DedicatedServer_Linux_x64');

  CopyFile('.\ScriptValidator.exe', fBuildFolder + 'ScriptValidator.exe');
  CopyFile('.\utils\TranslationManager (from kp-wiki)\TranslationManager.exe', fBuildFolder + 'TranslationManager.exe');
end;


procedure TKMBuilder.Perform(aSteps: TKMBuilderStepSet);
var
  theSteps: TKMBuilderStepSet;
begin
  // Try to capture into local variable, just in case
  theSteps := aSteps;

  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
      if I in theSteps then
      begin
        fOnStepBegin(I);

        var t := GetTickCount;

        case I of
          bsStartBuild:  Step1_GetRevisionCommitAndTag;
          bsCleanSource: Step2_CleanSource;
          bsBuildExe:    Step3_BuildGameExe;
          bsPatchExe:    Step4_PatchGameExe;
          bsPackData:    Step5_PackData;
          bsCopy:        Step6_CopyIntoBuildFolder;
        end;

        fOnStepDone(I, GetTickCount - t);

        if CheckTerminated then
          Exit;
      end;
      fOnDone;
    end);

  fWorker.Start;
end;


end.
