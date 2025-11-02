unit KM_Builder;
{$I ..\..\KM_CompilerDirectives.inc}
interface
uses
  Winapi.Windows, System.Classes, System.SysUtils, System.Generics.Defaults;


type
  TKMBuilderStep = (
    bsPrerequirements,
    bsStartBuild,
    bsCleanSource,
    bsBuildExe,
    bsPatchExe,
    bsPackData,
    bsArrangeFolder,
    bsPack7zip,
    bsPackInstaller,
    bsCommitAndTag
  );
  TKMBuilderStepSet = set of TKMBuilderStep;

const
  BuilderStepName: array [TKMBuilderStep] of string = (
    'Prerequirements',
    'Start build',
    'Clean sources',
    'Build executables',
    'Patch game executable',
    'Pack data',
    'Arrange build folder',
    'Pack 7-zip',
    'Pack installer',
    'Commit and Tag'
  );

type
  TKMBuilder = class
  private
    fOnLog: TProc<string>;
    fOnStepBegin: TProc<TKMBuilderStep>;
    fOnStepDone: TProc<TKMBuilderStep, Integer>;
    fOnDone: TProc;
    fWorker: TThread;

    fGameName: string;
    fBuildVersion: string;
    fBuildRevision: Integer;
    fBuildFolder: string;

    procedure DeleteRecursive(const aPath: string; const aFilters: array of string; aAvoid: array of string);
    procedure CopyFile(const aPathFrom, aPathTo: string);
    procedure CopyFilesRecursive(const aPathFrom, aPathTo: string; const aFilter: string; aRecursive: Boolean);
    procedure CopyFolder(const aPathFrom, aPathTo: string);

    procedure CheckFileExists(const aAppName, aFilename: string);
    function CheckTerminated: Boolean;

    procedure Step0_Prerequirements;
    procedure Step1_GetRevision;
    procedure Step2_CleanSource;
    procedure Step3_BuildGameExe;
    procedure Step4_PatchGameExe;
    procedure Step5_PackData;
    procedure Step6_ArrangeFolder;
    procedure Step7_Pack7zip;
    procedure Step8_PackInstaller;
    procedure Step9_CommitAndTag;
  public
    constructor Create(const aGameName, aBuildVersion: string; aOnLog: TProc<string>; aOnStepBegin: TProc<TKMBuilderStep>; aOnStepDone: TProc<TKMBuilderStep, Integer>; aOnDone: TProc);
    procedure Perform(aSteps: TKMBuilderStepSet);
    procedure Stop;

    property BuildRevision: Integer read fBuildRevision;
    property BuildFolder: string read fBuildFolder;
  end;


implementation
uses
  System.IOUtils, System.Masks, System.DateUtils, System.StrUtils, System.Generics.Collections,
  KromUtils;


{ TKMBuilder }
constructor TKMBuilder.Create(const aGameName, aBuildVersion: string; aOnLog: TProc<string>; aOnStepBegin: TProc<TKMBuilderStep>; aOnStepDone: TProc<TKMBuilderStep, Integer>; aOnDone: TProc);
begin
  inherited Create;

  fGameName := aGameName;
  fBuildVersion := aBuildVersion;
  fOnLog := aOnLog;
  fOnStepBegin := aOnStepBegin;
  fOnStepDone := aOnStepDone;
  fOnDone := aOnDone;

  fBuildRevision := -1;
  fBuildFolder := '<no folder>';
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


procedure TKMBuilder.Stop;
begin
  if fWorker = nil then Exit;

  fWorker.Terminate;
  fWorker := nil;
end;


procedure TKMBuilder.Step0_Prerequirements;
begin
  CheckFileExists('Main project file', 'KnightsProvince.dproj');
end;


procedure TKMBuilder.Step1_GetRevision;
begin
  fOnLog('rev-list ..');
  var cmdRevList := Format('cmd.exe /C "@FOR /F "USEBACKQ tokens=*" %%F IN (`git rev-list --count HEAD`) DO @ECHO %%F"', []);
  var res := CaptureConsoleOutput('.\', cmdRevList);
  fBuildRevision := StrToInt(Trim(res)) - 8500;
  fOnLog(Format('Rev number - %d', [fBuildRevision]));

  if CheckTerminated then Exit;

  // Write revision number for game exe and launcher/updater
  TFile.WriteAllText('.\KM_Revision.inc', #39 + 'r' + IntToStr(fBuildRevision) + #39);
  TFile.WriteAllText('.\version', fBuildVersion + ' r' + IntToStr(fBuildRevision));

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
    'thumbs.db', 'descript.ion', 'bugreport.txt', '*.skincfg', '*.identcache', '*.tvsconfig', '*.mi', '*.log.txt', '*.stat', '*.bak'], ['.git']);
end;


procedure TKMBuilder.Step3_BuildGameExe;
  procedure BuildWin(const aProject, aExe: string);
  begin
    if FileExists(aExe) then
    begin
      fOnLog('Deleting old ' + aExe);
      DeleteFile(aExe);
    end;

    fOnLog('Building ' + aExe);
    begin
      var s := Format('cmd.exe /C "CALL bat_rsvars.bat && MSBUILD "%s" /p:Config=Release /t:Build /clp:ErrorsOnly /fl"', [aProject]);
      var s2 := CaptureConsoleOutput('.\', s);
      fOnLog(s2);
    end;

    CheckFileExists('Resulting Windows exe', aExe);
  end;
  procedure BuildFpc(const aProject, aExe: string);
  begin
    if FileExists(aExe) then
    begin
      fOnLog('Deleting old ' + aExe);
      DeleteFile(aExe);
    end;

    var fpcFilename := 'C:\fpcupdeluxe\lazarus\lazbuild.exe';
    CheckFileExists('FPCUpDeluxe', fpcFilename);

    fOnLog('Building ' + aExe);
    begin
      var cmdFpc := Format('cmd.exe /C "CALL "%s" -q "%s""', [fpcFilename, aProject]);
      var res := CaptureConsoleOutput('.\', cmdFpc);
      fOnLog(res);
    end;

    CheckFileExists('Resulting Linux binary', aExe);
  end;
begin
  BuildWin('KnightsProvince.dproj', 'KnightsProvince.exe');

  if CheckTerminated then Exit;

  BuildWin('utils\ScriptValidator\ScriptValidator.dproj', 'ScriptValidator.exe');

  if CheckTerminated then Exit;

  BuildWin('utils\TranslationManager (from kp-wiki)\TranslationManager.dproj', 'utils\TranslationManager (from kp-wiki)\TranslationManager.exe');

  if CheckTerminated then Exit;

  BuildWin('utils\KP_DedicatedServer\KP_DedicatedServer.dproj', 'utils\KP_DedicatedServer\KP_DedicatedServer.exe');

  if CheckTerminated then Exit;

  BuildFpc('utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86');

  if CheckTerminated then Exit;

  BuildFpc('utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64');
end;


procedure TKMBuilder.Step4_PatchGameExe;
begin
  var exeSizeBefore := TFile.GetSize('KnightsProvince.exe');
  fOnLog(Format('Size before patch - %d bytes', [exeSizeBefore]));

  fOnLog('Patching KnightsProvince.exe');
  begin
    var madExceptFilename := 'C:\Program Files (x86)\madCollection\madExcept\Tools\madExceptPatch.exe';
    CheckFileExists('madExcept', madExceptFilename);

    var s := Format('cmd.exe /C ""%s" "%s""', [madExceptFilename, 'KnightsProvince.exe']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;

  // Check that patching went well and file is still there
  CheckFileExists('Game exe', 'KnightsProvince.exe');

  var exeSizeAfter := TFile.GetSize('KnightsProvince.exe');
  fOnLog(Format('Size after patch - %d bytes', [exeSizeAfter]));

  if exeSizeAfter <= exeSizeBefore then
    raise Exception.Create('Patching produced smaller exe?');
end;


procedure TKMBuilder.Step5_PackData;
begin
  var dataPackerFilename := 'DataPacker.exe';
  CheckFileExists('DataPacker', dataPackerFilename);

  fOnLog('Packing data.pack');
  begin
    var cmdDataPacker := Format('cmd.exe /C "CALL "%s" %s"', [dataPackerFilename, 'pack']);
    var res := CaptureConsoleOutput('.\', cmdDataPacker);
    fOnLog(res);
  end;

  CheckFileExists('Resulting data.pack', 'data.pack');

  var szAfter := TFile.GetSize('data.pack');
  fOnLog(Format('Size of data.pack - %d bytes', [szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Data.pack size is too small?');
end;


procedure TKMBuilder.Step6_ArrangeFolder;
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


procedure TKMBuilder.Step7_Pack7zip;
begin
  var sevenZipFilename := 'C:\Program Files\7-Zip\7z.exe';
  CheckFileExists('7-zip', sevenZipFilename);

  var sevenZipResult := ExcludeTrailingPathDelimiter(fBuildFolder) + '.7z';

  // Delete old archive if we had it for some reason
  DeleteFile(sevenZipResult);

  var cmd7zip := Format('"%s" a -t7z -m0=lzma2 -mx=9 -mfb=64 -md=128m -ms=on "%s" "%s"', [sevenZipFilename, sevenZipResult, fBuildFolder]);
  var res := CaptureConsoleOutput('.\', cmd7zip);
  fOnLog(res);

  CheckFileExists('Resulting 7z archive', sevenZipResult);

  var szAfter := TFile.GetSize(sevenZipResult);
  fOnLog(Format('Size of 7z - %d bytes', [szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting 7z archive is too small?');
end;


procedure TKMBuilder.Step8_PackInstaller;
begin
  var appName := Format('%s (%s r%d)', [fGameName, fBuildVersion, fBuildRevision]);
  var installerName := appName + ' Installer';
  var installerNameExe := installerName + '.exe';

  // Delete old installer if we had it for some reason
  DeleteFile(installerNameExe);

  var sl := TStringList.Create;
  sl.Append('; REVISION (write into Registry)');
  sl.Append(Format('#define Revision '#39'r%d'#39, [fBuildRevision]));
  sl.Append('');
  sl.Append('; Folder from where files get taken');
  sl.Append(Format('#define SourceFolder '#39'..\%s'#39, [ExcludeTrailingPathDelimiter(fBuildFolder)]));
  sl.Append('');
  sl.Append('; How the installer executable will be named');
  sl.Append(Format('#define OutputInstallerName '#39'%s'#39, [installerName]));
  sl.Append('');
  sl.Append('; Application name used in many places');
  sl.Append(Format('#define MyAppName '#39'%s'#39, [appName]));
  sl.Append('');
  sl.Append(Format('#define MyAppExeName '#39'%s'#39, ['KnightsProvince.exe']));
  sl.Append(Format('#define Website '#39'%s'#39, ['http://www.knightsprovince.com/']));

  sl.SaveToFile('.\installer\Constants.iss');
  sl.Free;

  if CheckTerminated then Exit;

  var innoFilename := 'C:\Program Files (x86)\Inno Setup 6\iscc.exe';
  CheckFileExists('InnoSetup', innoFilename);
  var cmdInstaller := Format('"%s" ".\installer\InstallerFull.iss"', [innoFilename]);
  var res := CaptureConsoleOutput('.\', cmdInstaller);
  fOnLog(res);

  var szAfter := TFile.GetSize(installerNameExe);
  fOnLog(Format('Size of "%s" - %d bytes', [installerNameExe, szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting installer is too small?');
end;


procedure TKMBuilder.Step9_CommitAndTag;
begin
  fOnLog('commit ..');
  var cmdCommit := Format('git commit -m "New version %d" -- "KM_Revision.inc"', [fBuildRevision]);
  CreateProcessSimple(cmdCommit, False, True, False);

  if CheckTerminated then Exit;

  fOnLog('tag ..');
  var cmdTag := Format('git tag r%d', [fBuildRevision]);
  CreateProcessSimple(cmdTag, False, True, False);
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
      try
        for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
        if I in theSteps then
        begin
          fOnStepBegin(I);

          var t := GetTickCount;

          case I of
            bsPrerequirements:  Step0_Prerequirements;
            bsStartBuild:       Step1_GetRevision;
            bsCleanSource:      Step2_CleanSource;
            bsBuildExe:         Step3_BuildGameExe;
            bsPatchExe:         Step4_PatchGameExe;
            bsPackData:         Step5_PackData;
            bsArrangeFolder:    Step6_ArrangeFolder;
            bsPack7zip:         Step7_Pack7zip;
            bsPackInstaller:    Step8_PackInstaller;
            bsCommitAndTag:     Step9_CommitAndTag;
          end;

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


end.
