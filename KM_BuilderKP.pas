unit KM_BuilderKP;
interface
uses
  System.Classes,
  System.SysUtils,
  KM_BuilderCommon;


type
  TKMBuilderKP = class(TKMBuilder)
  private
    fGameName: string;
    fGameVersion: string;

    fBuildRevision: Integer;
    fBuildFolder: string;
    fBuildResult7zip: string;

    procedure Step00_Initialize;
    procedure Step01_CleanSource;
    procedure Step02_BuildGameExe;
    procedure Step03_PatchGameExe;
    procedure Step04_PackData;
    procedure Step05_ArrangeFolder;
    procedure Step06_Pack7zip;
    procedure Step07_PackInstaller;
    procedure Step08_CreatePatch;
    procedure Step09_RegisterOnKT;
    procedure Step10_CommitAndTag;
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);

    function GetInfo: string; override;
  end;


implementation
uses
  System.IOUtils, System.DateUtils,
  KromUtils;


{ TKMBuilderKP }
constructor TKMBuilderKP.Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited;
  
  // Builder constants
  fGameName := 'Knights Province';
  fGameVersion := 'Alpha 13 wip';

  // Component and Tool paths (will be moved into INI or XML settings)

  fBuildRevision := -1;
  fBuildFolder := '<no folder>';
  fBuildResult7zip := '<no filename>';

  fBuildSteps.Add(TKMBuildStep.New('Initialize',            Step00_Initialize));
  fBuildSteps.Add(TKMBuildStep.New('Clean sources',         Step01_CleanSource));
  fBuildSteps.Add(TKMBuildStep.New('Build executables',     Step02_BuildGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Patch game executable', Step03_PatchGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Pack data',             Step04_PackData));
  fBuildSteps.Add(TKMBuildStep.New('Arrange build folder',  Step05_ArrangeFolder));
  fBuildSteps.Add(TKMBuildStep.New('Pack 7-zip',            Step06_Pack7zip));
  fBuildSteps.Add(TKMBuildStep.New('Pack installer',        Step07_PackInstaller));
  fBuildSteps.Add(TKMBuildStep.New('Create patch',          Step08_CreatePatch));
  fBuildSteps.Add(TKMBuildStep.New('Register on KT',        Step09_RegisterOnKT));
  fBuildSteps.Add(TKMBuildStep.New('Commit and Tag',        Step10_CommitAndTag));

  fBuildConfigs.Add(TKMBuildConfig.Create('Nightly build (7z)',           [0,1,2,3,4,5,6,  8,9,10]));
  fBuildConfigs.Add(TKMBuildConfig.Create('Full build (7z + installer)',  [0,1,2,3,4,5,6,7,8,9,10]));
end;


function TKMBuilderKP.GetInfo: string;
begin
  var sb := TStringBuilder.Create;

  // Constants
  sb.AppendLine(Format('Game name:    %s', [fGameName]));
  sb.AppendLine(Format('Game version: %s', [fGameVersion]));

  // Paths

  // Properties
  sb.AppendLine('');
  sb.AppendLine(Format('Revision:     r%d', [fBuildRevision]));
  sb.AppendLine(Format('Folder:       %s', [fBuildFolder]));
  sb.AppendLine(Format('Archive:      %s', [fBuildResult7zip]));

  Result := sb.ToString;
  sb.Free;
end;


procedure TKMBuilderKP.Step00_Initialize;
begin
  CheckFileExists('Main project file', 'KnightsProvince.dproj');

  fOnLog('rev-list ..');
  var cmdRevList := Format('cmd.exe /C "@FOR /F "USEBACKQ tokens=*" %%F IN (`git rev-list --count HEAD`) DO @ECHO %%F"', []);
  var res := CaptureConsoleOutput('.\', cmdRevList);

  // KP history is slightly botched up, ~8500 commits got duplicated
  fBuildRevision := StrToInt(Trim(res)) - 8500;
  fOnLog(Format('Rev number - %d', [fBuildRevision]));

  if CheckTerminated then Exit;

  // Write revision number for game exe and launcher/updater
  TFile.WriteAllText('.\KM_Revision.inc', #39 + 'r' + IntToStr(fBuildRevision) + #39);
  TFile.WriteAllText('.\version', fGameVersion + ' r' + IntToStr(fBuildRevision));

  var dtNow := Now;
  fBuildFolder := Format('kp%.4d-%.2d-%.2d (%s r%d)\', [YearOf(dtNow), MonthOf(dtNow), DayOf(dtNow), fGameVersion, fBuildRevision]);
  fBuildResult7zip := ExcludeTrailingPathDelimiter(fBuildFolder) + '.7z';
end;


procedure TKMBuilderKP.Step01_CleanSource;
begin
  // Delete folders
  DeleteRecursive(ExpandFileName('.\'), ['__history', '__recovery', 'backup', 'logs', 'dcu'], ['.git']);

  if CheckTerminated then Exit;

  // Delete files
  DeleteRecursive(ExpandFileName('.\'), [
    '*.~*', '*.ddp', '*.drc', '*.dcp', '*.dcu', '*.dsk', '*.o', '*.or', '*.ppu', '*.compiled', '*.local', '*.tmp', '*.log',
    'thumbs.db', 'descript.ion', 'bugreport.txt', '*.skincfg', '*.identcache', '*.tvsconfig', '*.mi', '*.log.txt', '*.stat', '*.bak'], ['.git']);
end;


procedure TKMBuilderKP.Step02_BuildGameExe;
  procedure BuildWin(const aProject, aExe: string);
  begin
    DeleteFileIfExists(aExe);

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
    DeleteFileIfExists(aExe);

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


procedure TKMBuilderKP.Step03_PatchGameExe;
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
    raise Exception.Create('Patching failed?');
end;


procedure TKMBuilderKP.Step04_PackData;
begin
  var dataPackerFilename := 'DataPacker.exe';
  CheckFileExists('DataPacker', dataPackerFilename);

  DeleteFileIfExists('data.pack');

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


procedure TKMBuilderKP.Step05_ArrangeFolder;
begin
  if DirectoryExists('.\' + fBuildFolder) then
  begin
    fOnLog(Format('Deleting old build folder of "%s"', [fBuildFolder]));
    TDirectory.Delete('.\' + fBuildFolder, True);
  end;

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


procedure TKMBuilderKP.Step06_Pack7zip;
begin
  var sevenZipFilename := 'C:\Program Files\7-Zip\7z.exe';
  CheckFileExists('7-zip', sevenZipFilename);

  // Delete old archive if we had it for some reason
  DeleteFileIfExists(fBuildResult7zip);

  var cmd7zip := Format('"%s" a -t7z -m0=lzma2 -mx=9 -mfb=64 -md=128m -ms=on "%s" "%s"', [sevenZipFilename, fBuildResult7zip, fBuildFolder]);
  var res := CaptureConsoleOutput('.\', cmd7zip);
  fOnLog(res);

  CheckFileExists('Resulting 7z archive', fBuildResult7zip);

  var szAfter := TFile.GetSize(fBuildResult7zip);
  fOnLog(Format('Size of 7z - %d bytes', [szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting 7z archive is too small?');
end;


procedure TKMBuilderKP.Step07_PackInstaller;
begin
  var appName := Format('%s (%s r%d)', [fGameName, fGameVersion, fBuildRevision]);
  var installerName := appName + ' Installer';
  var installerNameExe := installerName + '.exe';

  // Delete old installer if we had it for some reason
  DeleteFileIfExists(installerNameExe);

  var sl := TStringList.Create;
  sl.Append('; REVISION (write into Registry)');
  sl.Append(Format('#define Revision '#39'r%d'#39, [fBuildRevision]));
  sl.Append('');
  sl.Append('; Folder from where files get taken');
  sl.Append(Format('#define SourceFolder '#39'..\%s'#39, [fBuildFolder]));
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


procedure TKMBuilderKP.Step08_CreatePatch;
begin
  var launcherFilename := ExpandFileName('.\Launcher.exe');
  var result7zipFilename := ExpandFileName('.\' + fBuildResult7zip);
  CheckFileExists('Launcher', launcherFilename);
  CheckFileExists('7-zip package', result7zipFilename);

  // Example: .\Launcher.exe ".\kp2025-10-29 (Alpha 13 wip r17455).7z"
  var cmdPatch := Format('%s "%s"', [launcherFilename, result7zipFilename]);
  CreateProcessSimple(cmdPatch, False, True, False);
end;


procedure TKMBuilderKP.Step09_RegisterOnKT;
begin
  var ktAdminFilename := '.\KT_Admin.exe';
  CheckFileExists('KT Admin', ktAdminFilename);

  // Example: ".\KT_Admin.exe" register "kp2025-10-29 (Alpha 13 wip r17455)" 13 17492
  var cmdKtAdmin := Format('"%s" register "%s" %d %d', [ktAdminFilename, fBuildFolder, 13, fBuildRevision]);
  CreateProcessSimple(cmdKtAdmin, True, True, False);
end;


procedure TKMBuilderKP.Step10_CommitAndTag;
begin
  fOnLog('commit ..');
  var cmdCommit := Format('git commit -m "New version %d" -- "KM_Revision.inc"', [fBuildRevision]);
  CreateProcessSimple(cmdCommit, False, True, False);

  if CheckTerminated then Exit;

  fOnLog('tag ..');
  var cmdTag := Format('git tag r%d', [fBuildRevision]);
  CreateProcessSimple(cmdTag, False, True, False);
end;


end.
