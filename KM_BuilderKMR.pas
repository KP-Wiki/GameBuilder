unit KM_BuilderKMR;
interface
uses
  System.SysUtils,
  KM_BuilderCommon;


type
  TKMBuilderKMR = class(TKMBuilder)
  private
    fGameName: string;
    fGameVersion: string;

    fTPRPath: string;
    fPreviousVersionPath: string;
    fMapsRepoPath: string;
    fPrivateRepoPath: string;

    fDelphiRSVarsPath: string;
    fFPCUPdeluxePath: string;
    fMadExceptPath: string;
    fPandocPath: string;
    fInnoSetupPath: string;

    fBuildRevision: Integer;
    fBuildFolder: string;
    fBuildResultInstaller: string;

    procedure Step00_CheckRepositories;
    procedure Step01_Initialize;
    procedure Step02_CopyNetAuthSecure;
    procedure Step03_DeleteTempFiles;
    procedure Step04_GenerateDocs;
    procedure Step05_CopyPrePack;
    procedure Step06_RxxPack;
    procedure Step07_BuildGameExe;
    procedure Step08_PatchGameExe;
    procedure Step09_ArrangeFolder;
    procedure Step10_PackInstaller;
    procedure Step11_CommitAndTag;
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);

    procedure ExecuteWholeProjectGroup; override;

    function GetInfo: string; override;
  end;


implementation
uses
  System.Classes,
  System.IOUtils, System.DateUtils,
  KromUtils;


{ TKMBuilderKMR }
constructor TKMBuilderKMR.Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited;

  // Builder constants
  fGameName := 'KaM Remake';
  fGameVersion := 'Beta';

  // Component paths (will be moved into INI or XML settings)
  fTPRPath := '..\KaM TPR\';
  fPreviousVersionPath := 'C:\KaM Remake Beta r15472\';
  fMapsRepoPath := '..\kam_remake_maps.rey\';
  fPrivateRepoPath := '..\kam_remake_private.rey\';

  // External apps
  fDelphiRSVarsPath := 'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat';
  fFPCUPdeluxePath := 'C:\fpcupdeluxe\lazarus\lazbuild.exe';
  fMadExceptPath := 'C:\Program Files (x86)\madCollection\madExcept\Tools\madExceptPatch.exe';
  fPandocPath := 'C:\pandoc-3.8.2.1\pandoc.exe';
  fInnoSetupPath := 'C:\Program Files (x86)\Inno Setup 6\iscc.exe';

  fBuildRevision := -1;
  fBuildFolder := '<no folder>';
  fBuildResultInstaller := '<no filename>';

  fBuildSteps.Add(TKMBuildStep.New('Check repositories',    Step00_CheckRepositories));
  fBuildSteps.Add(TKMBuildStep.New('Initialize',            Step01_Initialize));
  fBuildSteps.Add(TKMBuildStep.New('Copy NetAuthSecure',    Step02_CopyNetAuthSecure));
  fBuildSteps.Add(TKMBuildStep.New('Delete temp files',     Step03_DeleteTempFiles));
  fBuildSteps.Add(TKMBuildStep.New('Generate docs',         Step04_GenerateDocs));
  fBuildSteps.Add(TKMBuildStep.New('Copy pre-pack',         Step05_CopyPrePack));
  fBuildSteps.Add(TKMBuildStep.New('RXX pack',              Step06_RxxPack));
  fBuildSteps.Add(TKMBuildStep.New('Build executables',     Step07_BuildGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Patch game executable', Step08_PatchGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Arrange build folder',  Step09_ArrangeFolder));
  fBuildSteps.Add(TKMBuildStep.New('Pack installer',        Step10_PackInstaller));
  fBuildSteps.Add(TKMBuildStep.New('Commit and Tag',        Step11_CommitAndTag));

  fBuildConfigs.Add(TKMBuildConfig.Create('Build folder (no commit)', [0,1,2,3,4,5,6,7,8,9      ]));
  fBuildConfigs.Add(TKMBuildConfig.Create('Installer',                [0,1,2,3,4,5,6,7,8,9,10,11]));
end;


procedure TKMBuilderKMR.ExecuteWholeProjectGroup;
begin
  BuildWinGroup(fDelphiRSVarsPath, 'KaMProjectGroup.groupproj');
end;


function TKMBuilderKMR.GetInfo: string;
begin
  var sb := TStringBuilder.Create;

  // Constants
  sb.AppendLine(Format('Game name:      %s', [fGameName]));
  sb.AppendLine(Format('Game version:   %s', [fGameVersion]));

  // Component paths
  sb.AppendLine('');
  sb.AppendLine(Format('Previous build: %s', [fPreviousVersionPath]));
  sb.AppendLine(Format('Maps repo:      %s', [fMapsRepoPath]));
  sb.AppendLine(Format('TPR:            %s', [fTPRPath]));
  sb.AppendLine(Format('Private repo:   %s', [fPrivateRepoPath]));

  // Thirdparty apps
  sb.AppendLine('');
  sb.AppendLine(Format('Delphi rsvars:  %s', [fDelphiRSVarsPath]));
  sb.AppendLine(Format('FPCUPdeluxe:    %s', [fFPCUPdeluxePath]));
  sb.AppendLine(Format('madExcept:      %s', [fMadExceptPath]));
  sb.AppendLine(Format('Pandoc:         %s', [fPandocPath]));
  sb.AppendLine(Format('Inno Setup:     %s', [fInnoSetupPath]));

  // Properties
  sb.AppendLine('');
  sb.AppendLine(Format('Revision:       r%d', [fBuildRevision]));
  sb.AppendLine(Format('Folder:         %s', [fBuildFolder]));
  sb.AppendLine(Format('Installer:      %s', [fBuildResultInstaller]));

  Result := Trim(sb.ToString);
  sb.Free;
end;


procedure TKMBuilderKMR.Step00_CheckRepositories;
begin
  fOnLog('Update submodules ..');
  var cmdSubmoduleUpdate := 'git submodule update --init --merge --recursive --remote --progress';
  var resSubmoduleUpdate := CaptureConsoleOutput('.\', cmdSubmoduleUpdate);
  fOnLog(resSubmoduleUpdate);
  fOnLog('Update submodules done' + sLineBreak);

  if CheckTerminated then Exit;

  fOnLog('Checkout master ..');
  var cmdMasterCheckuot := 'git checkout master';
  var resMasterCheckout := CaptureConsoleOutput('.\', cmdMasterCheckuot);
  fOnLog(resMasterCheckout);
  fOnLog('Checkout master done' + sLineBreak);

  if CheckTerminated then Exit;

  fOnLog('Pull master ..');
  var cmdMasterPull := 'git pull';
  var resMasterPull := CaptureConsoleOutput('.\', cmdMasterPull);
  fOnLog(resMasterPull);
  fOnLog('Pull master done' + sLineBreak);

  if CheckTerminated then Exit;

  fOnLog('Pull private ..');
  var cmdPrivatePull := 'git pull';
  var resPrivatePull := CaptureConsoleOutput(fPrivateRepoPath, cmdPrivatePull);
  fOnLog(resPrivatePull);
  fOnLog('Pull private done' + sLineBreak);

  if CheckTerminated then Exit;

  fOnLog('Pull maps ..');
  var cmdMapsPull := 'git pull';
  var resMapsPull := CaptureConsoleOutput(fMapsRepoPath, cmdMapsPull);
  fOnLog(resMapsPull);
  fOnLog('Pull maps done' + sLineBreak);
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

  var szDest := TFile.GetSize(nsaDest);
  fOnLog(Format('Size of old - %d bytes', [szDest]));
  CopyFile(nsaSource, nsaDest);
  szDest := TFile.GetSize(nsaDest);
  fOnLog(Format('Size of new - %d bytes', [szDest]));
end;


procedure TKMBuilderKMR.Step03_DeleteTempFiles;
begin
  // Delete folders
  fOnLog('Deleting temp folders ..');
  DeleteRecursive(ExpandFileName('.\'), ['__history', '__recovery', 'backup', 'logs', 'dcu'], ['.git']);

  if CheckTerminated then Exit;

  // Delete files
  fOnLog('Deleting temp files ..');
  DeleteRecursive(ExpandFileName('.\'), [
    '*.~*', '*.ddp', '*.drc', '*.dcp', '*.dcu', '*.dsk', '*.o', '*.or', '*.ppu', '*.compiled', '*.local', '*.tmp', '*.log',
    'thumbs.db', 'descript.ion', 'bugreport.txt', '*.skincfg', '*.identcache', '*.tvsconfig', '*.mi', '*.log.txt', '*.stat', '*.bak'], ['.git']);

  if CheckTerminated then Exit;

  // Delete all count.dat files from sfx folders recursively, they are temp game files
  fOnLog('Deleting sfx\count.dat');
  DeleteRecursive(ExpandFileName('.\data\sfx\'), ['count.dat'], []);
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


procedure TKMBuilderKMR.Step05_CopyPrePack;
begin
  // Copy palettes and fonts
  CheckFolderExists('Data GFX', fPrivateRepoPath + 'data\gfx\');
  CopyFolder(fPrivateRepoPath + 'data\gfx\', '.\data\gfx\');

  // It is important that KMR build process requires original TPR
  fOnLog('Copy data files from original KaM TPR game');

  // We need to check CRCs because KaM is inconsistent:
  //  - Steam version has 18 for Scout range, where's older TPR has it at 9 and some versions at 12
  CheckFileExists('TPR houses.dat', fTPRPath + 'data\defines\houses.dat');
  CheckFileCRC(fTPRPath + 'data\defines\houses.dat', $2D7A7842);
  CopyFile(fTPRPath + 'data\defines\houses.dat', '.\data\defines\houses.dat');

  CheckFileExists('TPR unit.dat', fTPRPath + 'data\defines\unit.dat');
  CheckFileCRC(fTPRPath + 'data\defines\unit.dat', $9129140C);
  CopyFile(fTPRPath + 'data\defines\unit.dat', '.\data\defines\unit.dat');
end;


procedure TKMBuilderKMR.Step06_RxxPack;
begin
  BuildWin(fDelphiRSVarsPath, '.\Utils\RXXPacker\RXXPacker.dproj', '.\Utils\RXXPacker\RXXPacker.exe');

  if CheckTerminated then Exit;

  TDirectory.Delete('.\data\sprites\', True);

  if CheckTerminated then Exit;

  CheckFolderExists('SpriteResource', fPrivateRepoPath + 'SpriteResource\');
  CheckFolderExists('SpriteResource', fPrivateRepoPath + 'SpriteInterp\Output\');

  fOnLog('RXX Pack ..');
  // Pack rx textures to rxx
  var cmdRxxPack := Format('cmd.exe /C ".\Utils\RXXPacker\RXXPacker.exe srx "%s" sint "%s" d ".\%s" rxa all"', [
    fPrivateRepoPath + 'SpriteResource\',       // Source RX
    fPrivateRepoPath + 'SpriteInterp\Output\',  // Source interpolated
    '.\data\sprites\'                           // Destination
    ]);

  CaptureConsoleOutput2('.\', cmdRxxPack, procedure (const aMsg: string) begin fOnLog(aMsg); end);

  fOnLog('RXX Pack done' + sLineBreak);
end;


procedure TKMBuilderKMR.Step07_BuildGameExe;
begin
  BuildWin(fDelphiRSVarsPath, 'KaM_Remake.dproj', 'KaM_Remake.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'Utils\Campaign builder\CampaignBuilder.dproj', 'Utils\Campaign builder\CampaignBuilder.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'Utils\DedicatedServer\KaM_DedicatedServer.dproj', 'Utils\DedicatedServer\KaM_DedicatedServer.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'Utils\DedicatedServerGUI\KaM_DedicatedServerGUI.dproj', 'Utils\DedicatedServerGUI\KaM_DedicatedServerGUI.exe');

  if CheckTerminated then Exit;

  //todo: Fails to build in "Release" configuration
  //BuildWin(fDelphiRSVarsPath, 'Utils\ScriptValidator\ScriptValidator.dproj', 'Utils\ScriptValidator\ScriptValidator.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'Utils\TranslationManager (from kp-wiki)\TranslationManager.dproj', 'Utils\TranslationManager (from kp-wiki)\TranslationManager.exe');

  if CheckTerminated then Exit;

  //todo: Add ScriptingEditor as submodule
  //BuildWin(fDelphiRSVarsPath, 'Utils\ScriptingEditor (from kp-wiki)\ScriptingEditor.dproj', 'Utils\ScriptingEditor (from kp-wiki)\ScriptingEditor.exe');

  if CheckTerminated then Exit;

  BuildFpc(fFPCUPdeluxePath, 'Utils\DedicatedServer\KaM_DedicatedServer_win32-linux_x86.lpi', 'Utils\DedicatedServer\KaM_Remake_Server_linux_x86');

  if CheckTerminated then Exit;

  BuildFpc(fFPCUPdeluxePath, 'Utils\DedicatedServer\KaM_DedicatedServer_win32-linux_x86_64.lpi', 'Utils\DedicatedServer\KaM_Remake_Server_linux_x86_64');
end;


procedure TKMBuilderKMR.Step08_PatchGameExe;
begin
  var exeSizeBefore := TFile.GetSize('KaM_Remake.exe');
  fOnLog(Format('Size before patch - %d bytes', [exeSizeBefore]));

  fOnLog('Patching KaM_Remake.exe');
  begin
    CheckFileExists('madExcept', fMadExceptPath);

    var s := Format('cmd.exe /C ""%s" "%s""', [fMadExceptPath, 'KaM_Remake.exe']);
    var s2 := CaptureConsoleOutput('.\', s);
    fOnLog(s2);
  end;

  // Check that patching went well and file is still there
  CheckFileExists('Game exe', 'KaM_Remake.exe');

  var exeSizeAfter := TFile.GetSize('KaM_Remake.exe');
  fOnLog(Format('Size after patch - %d bytes', [exeSizeAfter]));

  if exeSizeAfter <= exeSizeBefore then
    raise Exception.Create('Patching failed?');
end;


procedure TKMBuilderKMR.Step09_ArrangeFolder;
begin
  if DirectoryExists('.\' + fBuildFolder) then
  begin
    fOnLog(Format('Deleting old build folder of "%s"', [fBuildFolder]));
    TDirectory.Delete('.\' + fBuildFolder, True);
  end;

  ForceDirectories('.\' + fBuildFolder);

  CopyFolder('.\data\defines\', fBuildFolder + 'data\defines\');
  CopyFolder('.\data\cursors\', fBuildFolder + 'data\cursors\');
  CopyFolder('.\data\text\', fBuildFolder + 'data\text\');
  CopyFolder('.\data\sprites\', fBuildFolder + 'data\sprites\');
  CopyFile('.\data\locales.txt', fBuildFolder + 'data\locales.txt');

  CopyFilesRecursive('.\Docs\Readme\Readme\', fBuildFolder + 'Readme\', '*.gif', False);
  CopyFilesRecursive('.\Docs\Readme\', fBuildFolder + '.\', '*.html', False);

  CopyFolder('.\Sounds\', fBuildFolder + 'Sounds\');
  CopyFolder('.\Music\', fBuildFolder + 'Music\');
  CopyFolder('.\lib\', fBuildFolder + 'lib\');

  CopyFile('.\Modding graphics\Readme.txt', fBuildFolder + 'Modding graphics\Readme.txt');

  CopyFolder(fMapsRepoPath + 'Campaigns\', fBuildFolder + 'Campaigns\');
  CopyFolder(fMapsRepoPath + 'Maps\', fBuildFolder + 'Maps\');
  CopyFolder(fMapsRepoPath + 'MapsMP\', fBuildFolder + 'MapsMP\');
  CopyFolder(fMapsRepoPath + 'Tutorials\', fBuildFolder + 'Tutorials\');

  CopyFolder(fPrivateRepoPath + 'data\', fBuildFolder + 'data\');
  CopyFolder(fPrivateRepoPath + 'Video\Campaigns\', fBuildFolder + 'Campaigns\');
  CopyFolder(fPrivateRepoPath + 'Video\data\', fBuildFolder + 'data\');

  //todo: Stop relying on previous build
  CopyFolder(fPreviousVersionPath + 'data\sfx\', fBuildFolder + 'data\sfx\');
  CopyFolder(fPreviousVersionPath + 'Music\', fBuildFolder + 'Music\');
  CopyFilesRecursive(fPreviousVersionPath + 'Campaigns\', fBuildFolder + 'Campaigns\', '*.mp3', True);

  // Copy selected executable files
  CopyFile('.\KaM_Remake.exe', fBuildFolder + 'KaM_Remake.exe');
  CopyFile('.\bass.dll', fBuildFolder + 'bass.dll');
  CopyFile('.\ogg.dll', fBuildFolder + 'ogg.dll');
  CopyFile('.\vorbis.dll', fBuildFolder + 'vorbis.dll');
  CopyFile('.\vorbisfile.dll', fBuildFolder + 'vorbisfile.dll');
  CopyFile('.\Utils\AVIPlayer\OpenAL32.dll', fBuildFolder + 'OpenAL32.dll');
  CopyFile('.\Installer\uninst_clean.bat', fBuildFolder + 'uninst_clean.bat');
  CopyFile('.\Installer\oalinst.exe', fBuildFolder + 'oalinst.exe');

  //todo: Copy Scripting Editor
  //Utils\ScriptingEditor (from kp-wiki)\ScriptingEditor.exe

  // copy utility applications exe files
  CopyFile('.\KM_TextIDs.inc', fBuildFolder + 'Utils\KM_TextIDs.inc');
  CopyFile('.\KaM_Remake_Settings_ini_readme.txt', fBuildFolder + 'Utils\KaM_Remake_Settings_ini_readme.txt');
  CopyFile('.\Utils\Campaign builder\CampaignBuilder.exe', fBuildFolder + 'Utils\CampaignBuilder.exe');
  CopyFile('.\Utils\DedicatedServer\KaM_DedicatedServer.exe', fBuildFolder + 'Utils\KaM_Remake_Server_win32.exe');
  CopyFile('.\Utils\DedicatedServerGUI\KaM_DedicatedServerGUI.exe', fBuildFolder + 'Utils\KaM_Remake_ServerGUI_win32.exe');

  //todo: Fails to build in "Release" configuration
  //CopyFile('.\Utils\ScriptValidator\ScriptValidator.exe', fBuildFolder + 'Utils\ScriptValidator.exe');

  CopyFile('.\Utils\TranslationManager (from kp-wiki)\TranslationManager.exe', fBuildFolder + 'Utils\TranslationManager.exe');

  // copy linux dedicated servers
  CopyFile('.\Utils\DedicatedServer\KaM_Remake_Server_linux_x86', fBuildFolder + 'Utils\KaM_Remake_Server_linux_x86');
  CopyFile('.\Utils\DedicatedServer\KaM_Remake_Server_linux_x86_64', fBuildFolder + 'Utils\KaM_Remake_Server_linux_x86_64');
end;


procedure TKMBuilderKMR.Step10_PackInstaller;
begin
  CheckFileExists('Installer secret', fPrivateRepoPath + 'Installer\CheckKaM.iss');

  CopyFile(fPrivateRepoPath + 'Installer\CheckKaM.iss', '.\Installer\CheckKaM.iss');

  // Delete old installer if we had it for some reason
  DeleteFileIfExists(fBuildResultInstaller);

  var swConstants := TStreamWriter.Create('.\Installer\Constants_local.iss');
  // Folders are relative to ".\Installer\"
  swConstants.WriteLine(Format('#define BuildFolder '#39'%s'#39, ['..\' + fBuildFolder]));
  swConstants.WriteLine(Format('#define OutputFolder '#39'%s'#39, ['..\']));
  swConstants.Free;

  var swRevision := TStreamWriter.Create('.\Installer\Revision.iss');
  swRevision.WriteLine(Format('#define Revision '#39'r%d'#39, [fBuildRevision]));
  swRevision.Free;

  if CheckTerminated then Exit;

  CheckFileExists('InnoSetup', fInnoSetupPath);
  var cmdInstaller := Format('"%s" ".\installer\InstallerFull.iss"', [fInnoSetupPath]);
  CaptureConsoleOutput2('.\', cmdInstaller, procedure (const aMsg: string) begin fOnLog(aMsg); end);

  var szAfter := TFile.GetSize(fBuildResultInstaller);
  fOnLog(Format('Size of "%s" - %d bytes', [fBuildResultInstaller, szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting installer is too small?');
end;


procedure TKMBuilderKMR.Step11_CommitAndTag;
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
