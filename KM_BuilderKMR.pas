unit KM_BuilderKMR;
interface
uses
  System.Classes,
  System.SysUtils,
  KM_BuilderCommon;


type
  TKMBuilderKMR = class(TKMBuilder)
  private
    fGameName: string;
    fGameVersion: string;

    fPrivateRepoPath: string;
    fResourcesRepoPath: string;
    fPandocPath: string;

    fBuildRevision: Integer;
    fBuildFolder: string;
    fBuildResultInstaller: string;

    procedure Step00_CheckRepositories;
    procedure Step01_Initialize;
    procedure Step02_CopyNetAuthSecure;
    procedure Step03_CleanSource;
    procedure Step04_GenerateDocs;
    procedure Step05_BuildGameExe;
    procedure Step06_PatchGameExe;
    procedure Step07_PackData;
    procedure Step08_ArrangeFolder;
    procedure Step09_PackInstaller;
    procedure Step10_CommitAndTag;
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);

    function GetInfo: string; override;
  end;


implementation
uses
  System.IOUtils, System.DateUtils,
  KromUtils;


{ TKMBuilderKMR }
constructor TKMBuilderKMR.Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited;

  fGameName := 'KaM Remake';
  fGameVersion := 'Beta';

  fPrivateRepoPath:= '..\kam_remake_private.rey\';
  fResourcesRepoPath := '..\kam_remake.rey.resources\';
  fPandocPath := 'C:\pandoc-3.8.2.1\pandoc.exe';

  fBuildRevision := -1;
  fBuildFolder := '<no folder>';
  fBuildResultInstaller := '<no filename>';

  fBuildSteps.Add(TKMBuildStep.New('Check repositories',    Step00_CheckRepositories));
  fBuildSteps.Add(TKMBuildStep.New('Initialize',            Step01_Initialize));
  fBuildSteps.Add(TKMBuildStep.New('Copy NetAuthSecure',    Step02_CopyNetAuthSecure));
  fBuildSteps.Add(TKMBuildStep.New('Clean sources',         Step03_CleanSource));
  fBuildSteps.Add(TKMBuildStep.New('Generate docs',         Step04_GenerateDocs));
  fBuildSteps.Add(TKMBuildStep.New('Build executables',     Step05_BuildGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Patch game executable', Step06_PatchGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Pack data',             Step07_PackData));
  fBuildSteps.Add(TKMBuildStep.New('Arrange build folder',  Step08_ArrangeFolder));
  fBuildSteps.Add(TKMBuildStep.New('Pack installer',        Step09_PackInstaller));
  fBuildSteps.Add(TKMBuildStep.New('Commit and Tag',        Step10_CommitAndTag));

  fBuildConfigs.Add(TKMBuildConfig.Create('Installer)',  [0,1,2,3,4,5,6,7,8,9,10]));
end;


function TKMBuilderKMR.GetInfo: string;
begin
  var sb := TStringBuilder.Create;
  sb.AppendLine(Format('Game name:      %s', [fGameName]));
  sb.AppendLine(Format('Game version:   %s', [fGameVersion]));
  sb.AppendLine(Format('Private repo:   %s', [fPrivateRepoPath]));
  sb.AppendLine(Format('Resources repo: %s', [fResourcesRepoPath]));
  sb.AppendLine(Format('Pandoc:         %s', [fPandocPath]));
  sb.AppendLine('');
  sb.AppendLine(Format('Revision:       r%d', [fBuildRevision]));
  sb.AppendLine(Format('Folder:         %s', [fBuildFolder]));
  sb.AppendLine(Format('Installer:      %s', [fBuildResultInstaller]));
  Result := sb.ToString;
  sb.Free;
end;


procedure TKMBuilderKMR.Step00_CheckRepositories;
begin
  fOnLog('Update submodules ..');
  var cmdSubmoduleUpdate := 'git submodule update --init --recursive --remote --progress';
  var resSubmoduleUpdate := CaptureConsoleOutput('.\', cmdSubmoduleUpdate);
  fOnLog(resSubmoduleUpdate);
  fOnLog('Update submodules done' + sLineBreak);

  fOnLog('Checkout master ..');
  var cmdMasterCheckuot := 'git checkout master';
  var resMasterCheckout := CaptureConsoleOutput('.\', cmdMasterCheckuot);
  fOnLog(resMasterCheckout);
  fOnLog('Checkout master done' + sLineBreak);

  fOnLog('Pull master ..');
  var cmdMasterPull := 'git pull';
  var resMasterPull := CaptureConsoleOutput('.\', cmdMasterPull);
  fOnLog(resMasterPull);
  fOnLog('Pull master done' + sLineBreak);

  fOnLog('Pull private ..');
  var cmdPrivatePull := 'git pull';
  var resPrivatePull := CaptureConsoleOutput(fPrivateRepoPath, cmdPrivatePull);
  fOnLog(resPrivatePull);
  fOnLog('Pull private done' + sLineBreak);

  fOnLog('Pull resources ..');
  var cmdResourcesPull := 'git pull';
  var resResourcesPull := CaptureConsoleOutput(fResourcesRepoPath, cmdResourcesPull);
  fOnLog(resResourcesPull);
  fOnLog('Pull resources done' + sLineBreak);
end;


procedure TKMBuilderKMR.Step01_Initialize;
begin
  CheckFileExists('Main project file', 'KaM_Remake.dproj');

  fOnLog('rev-list ..');
  // Get revision number from git
  var cmdRevList := Format('cmd.exe /C "@FOR /F "USEBACKQ tokens=*" %%F IN (`git rev-list --count HEAD`) DO @ECHO %%F"', []);
  var res := CaptureConsoleOutput('.\', cmdRevList);

  // We increment revision number by 1, as we are going to make one more commit for KM_Revision.inc
  fBuildRevision := StrToInt(Trim(res)) + 1;
  fOnLog(Format('Rev number - %d', [fBuildRevision]));

  if CheckTerminated then Exit;

  // Write revision number for game exe
  TFile.WriteAllText('.\KM_Revision.inc', 'GAME_REVISION_NUM=' + IntToStr(fBuildRevision));

  fBuildFolder := Format('%s %s r%d\', [fGameName, fGameVersion, fBuildRevision]);
  fBuildResultInstaller := ExcludeTrailingPathDelimiter(fBuildFolder) + '.exe';
end;


procedure TKMBuilderKMR.Step02_CopyNetAuthSecure;
begin
  var nsaSource := fPrivateRepoPath + 'src\net\KM_NetAuthSecure.pas';
  var nsaDest := '.\src\net\KM_NetAuthSecure.pas';

  var szSource := TFile.GetSize(nsaSource);
  var szDest := TFile.GetSize(nsaDest);

  if szSource = szDest then
    raise Exception.Create('KM_NetAuthSecure is already there?');

  fOnLog(Format('Size of old - %d bytes', [szDest]));
  DeleteFileIfExists(nsaDest);

  CopyFile(nsaSource, nsaDest);
  szDest := TFile.GetSize(nsaDest);
  fOnLog(Format('Size of new - %d bytes', [szDest]));
end;


procedure TKMBuilderKMR.Step03_CleanSource;
begin
  // Delete folders
  DeleteRecursive(ExpandFileName('.\'), ['__history', '__recovery', 'backup', 'logs', 'dcu'], ['.git']);

  if CheckTerminated then Exit;

  // Delete files
  DeleteRecursive(ExpandFileName('.\'), [
    '*.~*', '*.ddp', '*.drc', '*.dcp', '*.dcu', '*.dsk', '*.o', '*.or', '*.ppu', '*.compiled', '*.local', '*.tmp', '*.log',
    'thumbs.db', 'descript.ion', 'bugreport.txt', '*.skincfg', '*.identcache', '*.tvsconfig', '*.mi', '*.log.txt', '*.stat', '*.bak'], ['.git']);
end;


procedure TKMBuilderKMR.Step04_GenerateDocs;
const
  LANG: array [0..3] of string = ('eng', 'ger', 'pol', 'rus');
begin
  CheckFileExists('Pandoc', fPandocPath);

  fOnLog('Generating docs ..');
  for var I := Low(LANG) to High(LANG) do
  begin
    //todo: Folder structure should be simplified
    var cmd := Format('%s -s -f markdown .\Docs\Readme\getting-started_%s.md -o .\Docs\Readme\Readme_%s.html --metadata-file=".\Docs\Readme\Readme\metadata_%s.yml"', [fPandocPath, LANG[I], LANG[I], LANG[I]]);
    var res := CaptureConsoleOutput('.\', cmd);
    fOnLog(res);
  end;
  fOnLog('Generating docs done' + sLineBreak);
end;


procedure TKMBuilderKMR.Step05_BuildGameExe;
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


procedure TKMBuilderKMR.Step06_PatchGameExe;
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


procedure TKMBuilderKMR.Step07_PackData;
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


procedure TKMBuilderKMR.Step08_ArrangeFolder;
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


procedure TKMBuilderKMR.Step09_PackInstaller;
begin
  var installerName := ChangeFileExt(fBuildResultInstaller, '');

  // Delete old installer if we had it for some reason
  DeleteFileIfExists(fBuildResultInstaller);

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
  sl.Append(Format('#define MyAppName '#39'%s'#39, [installerName]));
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

  var szAfter := TFile.GetSize(fBuildResultInstaller);
  fOnLog(Format('Size of "%s" - %d bytes', [fBuildResultInstaller, szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting installer is too small?');
end;


procedure TKMBuilderKMR.Step10_CommitAndTag;
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
