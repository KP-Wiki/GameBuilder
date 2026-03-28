unit KM_BuilderKP;
interface
uses
  System.SysUtils,
  KM_BuilderCommon;


type
  TKMBuilderKP = class(TKMBuilder)
  private
    fGameName: string;
    fGameBuildFlags: string;

    fBuildName: string;
    fBuildMajor: Integer;
    fBuildMinor: Integer;
    fBuildRelease: Integer;
    fBuildRevision: Integer;

    fMapsRepoPath: string;

    fDelphiRSVarsPath: string;
    fFPCUPdeluxePath: string;
    fMadExceptPath: string;
    f7zipPath: string;
    fInnoSetupPath: string;

    fBuildFolder: string;
    fBuildResult7zip: string;
    fBuildResultInstaller: string;

    procedure Step00_UpdateRepositories(aConfig: TKMBuildConfiguration);
    procedure Step01_Initialize(aConfig: TKMBuildConfiguration);
    procedure Step02_CheckVersion(aConfig: TKMBuildConfiguration);
    procedure Step03_ScanForDebugFlags(aConfig: TKMBuildConfiguration);
    procedure Step04_WriteVersion(aConfig: TKMBuildConfiguration);
    procedure Step05_DeleteTempFiles(aConfig: TKMBuildConfiguration);
    //todo -cBuilder: Update scripting code and wiki
    procedure Step06_BuildGameExe(aConfig: TKMBuildConfiguration);
    procedure Step07_PatchGameExe(aConfig: TKMBuildConfiguration);
    procedure Step08_PackData(aConfig: TKMBuildConfiguration);
    procedure Step09_RunTests(aConfig: TKMBuildConfiguration);
    procedure Step10_ArrangeFolder(aConfig: TKMBuildConfiguration);
    procedure Step11_Pack7zip(aConfig: TKMBuildConfiguration);
    procedure Step12_PackInstaller(aConfig: TKMBuildConfiguration);
    procedure Step13_CreatePatch(aConfig: TKMBuildConfiguration);
    procedure Step14_RegisterOnKT(aConfig: TKMBuildConfiguration);
    procedure Step15_CommitAndTag(aConfig: TKMBuildConfiguration);
    //todo -cBuilder: git Push wiki
  public
    constructor Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc); override;

    procedure ExecuteWholeProjectGroup(aConfig: TKMBuildConfiguration); override;

    function GetInfo: string; override;
  end;


implementation
uses
  System.Classes, System.IOUtils, System.DateUtils, System.StrUtils,
  KromUtils,
  KM_BuilderUtility;


{ TKMBuilderKP }
constructor TKMBuilderKP.Create(aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited;

  // Builder constants
  fGameName := 'Knights Province';

  fBuildName := 'Alpha';
  fBuildMajor := 0;
  fBuildMinor := 13;
  fBuildRelease := 2;
  fBuildRevision := -1; // Calculated automatically

  // Component paths (will be moved into INI or XML settings)
  fMapsRepoPath := '..\knights_province.public.git\';

  // External apps
  fDelphiRSVarsPath := 'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat';
  fFPCUPdeluxePath := 'C:\fpcupdeluxe\lazarus\lazbuild.exe';
  fMadExceptPath := 'C:\Program Files (x86)\madCollection\madExcept\Tools\madExceptPatch.exe';
  f7zipPath := 'C:\Program Files\7-Zip\7z.exe';
  fInnoSetupPath := 'C:\Program Files (x86)\Inno Setup 6\iscc.exe';

  // Build information
  fBuildFolder := '<no folder>';
  fBuildResult7zip := '<no filename>';
  fBuildResultInstaller := '<no filename>';

  // Steps (order is important)
  fBuildSteps.Add(TKMBuildStep.New('Update repositories',   Step00_UpdateRepositories));
  fBuildSteps.Add(TKMBuildStep.New('Initialize',            Step01_Initialize));
  fBuildSteps.Add(TKMBuildStep.New('Check version',         Step02_CheckVersion));
  fBuildSteps.Add(TKMBuildStep.New('Scan for debug flags',  Step03_ScanForDebugFlags));
  fBuildSteps.Add(TKMBuildStep.New('Write version',         Step04_WriteVersion));
  fBuildSteps.Add(TKMBuildStep.New('Delete temp files',     Step05_DeleteTempFiles));
  fBuildSteps.Add(TKMBuildStep.New('Build executables',     Step06_BuildGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Patch game executable', Step07_PatchGameExe));
  fBuildSteps.Add(TKMBuildStep.New('Pack data',             Step08_PackData));
  fBuildSteps.Add(TKMBuildStep.New('Run tests',             Step09_RunTests));
  fBuildSteps.Add(TKMBuildStep.New('Arrange build folder',  Step10_ArrangeFolder));
  fBuildSteps.Add(TKMBuildStep.New('Pack 7-zip',            Step11_Pack7zip));
  fBuildSteps.Add(TKMBuildStep.New('Pack installer',        Step12_PackInstaller));
  fBuildSteps.Add(TKMBuildStep.New('Create patch',          Step13_CreatePatch));
  fBuildSteps.Add(TKMBuildStep.New('Register on KT',        Step14_RegisterOnKT));
  fBuildSteps.Add(TKMBuildStep.New('Commit and Tag',        Step15_CommitAndTag));

  // Scenarios
  // Nightly build (same as Release, without Installer)
  fBuildScenarios.Add(TKMBuildScenario.Create('Nightly build (7z)',          bcRelease, [0,1,2,3,4,5,6,7,8,9,10,11,   13,14,15]));

  // Public release version
  fBuildScenarios.Add(TKMBuildScenario.Create('Full build (7z + installer)', bcRelease, [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]));
end;


procedure TKMBuilderKP.ExecuteWholeProjectGroup(aConfig: TKMBuildConfiguration);
begin
  fWorker := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        fOnLog(Format('Starting builder thread %d', [TThread.CurrentThread.ThreadID]));

        BuildWinGroup(fDelphiRSVarsPath, 'KP_ProjectGroup.groupproj', aConfig);

        if CheckTerminated then Exit;

        // FPC projects dont have a group, but we still want to build them too
        BuildFpc(fFPCUPdeluxePath, 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86');
        BuildFpc(fFPCUPdeluxePath, 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64');

        fOnDone;
      except
        on E: Exception do
          fOnLog('ERROR: ' + E.Message);
      end;
    end);

  fWorker.FreeOnTerminate := False;
  fWorker.Start;
end;


function TKMBuilderKP.GetInfo: string;
begin
  var sb := TStringBuilder.Create;

  // Constants
  sb.AppendLine(Format('Game name:      %s %s', [fGameName, fBuildName]));
  sb.AppendLine(Format('Build:          %d.%d.%d.%d', [fBuildMajor, fBuildMinor, fBuildRelease, fBuildRevision]));
  sb.AppendLine(Format('Build flags:    %s', [fGameBuildFlags]));

  // Component paths
  sb.AppendLine('');
  sb.AppendLine(Format('Maps repo:      %s', [fMapsRepoPath]));

  // External apps
  sb.AppendLine('');
  sb.AppendLine(Format('Delphi rsvars:  %s', [fDelphiRSVarsPath]));
  sb.AppendLine(Format('FPCUPdeluxe:    %s', [fFPCUPdeluxePath]));
  sb.AppendLine(Format('madExcept:      %s', [fMadExceptPath]));
  sb.AppendLine(Format('7-zip:          %s', [f7zipPath]));
  sb.AppendLine(Format('Inno Setup:     %s', [fInnoSetupPath]));

  // Results
  sb.AppendLine('');
  sb.AppendLine(Format('Folder:         %s', [fBuildFolder]));
  sb.AppendLine(Format('7-zip package:  %s', [fBuildResult7zip]));
  sb.AppendLine(Format('Installer:      %s', [fBuildResultInstaller]));

  Result := Trim(sb.ToString);
  sb.Free;
end;


procedure TKMBuilderKP.Step00_UpdateRepositories(aConfig: TKMBuildConfiguration);
begin
  fOnLog('Update submodules ..');
  var cmdUpdateSubmodules := 'git submodule update --init --merge --recursive --remote --progress';
  var resUpdateSubmodules := CaptureConsoleOutput('.\', cmdUpdateSubmodules);
  fOnLog(resUpdateSubmodules);
  fOnLog('Update submodules done' + sLineBreak);

  if CheckTerminated then Exit;

  fOnLog('Pull maps ..');
  var cmdPullMaps := 'git pull';
  var resPullMaps := CaptureConsoleOutput(fMapsRepoPath, cmdPullMaps);
  fOnLog(resPullMaps);
  fOnLog('Pull maps done' + sLineBreak);
end;


procedure TKMBuilderKP.Step01_Initialize(aConfig: TKMBuildConfiguration);
begin
  CheckFileExists('Main project file', 'KnightsProvince.dproj');

  // Get revision number from git
  fOnLog('rev-list ..');
  var cmdRevList := Format('cmd.exe /C "@FOR /F "USEBACKQ tokens=*" %%F IN (`git rev-list --count HEAD`) DO @ECHO %%F"', []);
  var res := CaptureConsoleOutput('.\', cmdRevList);

  // KP history is slightly botched up, ~8500 commits got duplicated
  fBuildRevision := StrToInt(Trim(res)) - 8500;
  fOnLog(Format('Rev number - %d', [fBuildRevision]));

  // E.g.: "Knights Province Alpha 13.2.17800"
  fBuildFolder := Format('Knights Province %s %d.%d.%d\', [fBuildName, fBuildMinor, fBuildRelease, fBuildRevision]);
  fBuildResult7zip := ExcludeTrailingPathDelimiter(fBuildFolder) + '.7z';
  fBuildResultInstaller := ExcludeTrailingPathDelimiter(fBuildFolder) + ' Installer.exe';
end;


procedure TKMBuilderKP.Step02_CheckVersion(aConfig: TKMBuildConfiguration);
begin
  // E.g.: Alpha 13.2
  var buildName := Format('%s %d.%d', [fBuildName, fBuildMinor, fBuildRelease]);


  // There is a number of locations that have game version:
  //  - KM_Const
  begin
    var gameVersionConstFound := False;
    var sr := TStreamReader.Create('.\src\KM_Const.pas');
    repeat
      var pasLine := sr.ReadLine;
      if Pos('GAME_VERSION', pasLine) > 0 then
      begin
        gameVersionConstFound := True;
        if Pos(buildName, pasLine) = 0 then
          raise Exception.Create(Format('Game version in file - "%s"', [Trim(pasLine)]));
        Break;
      end;
    until sr.EndOfStream;
    sr.Free;

    if not gameVersionConstFound then
      raise Exception.Create(Format('Game version const not found in "%s"', ['.\src\KM_Const.pas']));

    fOnLog(Format('Game version in "%s" - ok', ['.\src\KM_Const.pas']));
  end;

  if CheckTerminated then Exit;

  //  - Readme
  begin
    var gameVersionFound := False;
    var sr := TStreamReader.Create('.\Win32\readme_eng.html', TEncoding.ANSI);
    repeat
      var htmlLine := sr.ReadLine;
      if Pos(buildName, htmlLine) > 0 then
      begin
        gameVersionFound := True;
        Break;
      end;
    until sr.EndOfStream;
    sr.Free;

    if not gameVersionFound then
      raise Exception.Create(Format('Game version const not found in "%s"', ['.\Win32\readme_eng.html']));

    fOnLog(Format('Game version in "%s" - ok', ['.\Win32\readme_eng.html']));
  end;

  if CheckTerminated then Exit;

  //  - Changelog
  begin
    // We have changelogs only after Minor versions. Release versions get included into them
    // Changelog Alpha 13.txt
    var changelogFilename := Format('.\Changelog %s %d.txt', [fBuildName, fBuildMinor]);
    CheckFileExists('Changelog', changelogFilename);

    var gameVersionFound := False;
    var sr := TStreamReader.Create(changelogFilename);
    repeat
      var txtLine := sr.ReadLine;
      if Pos(buildName, txtLine) > 0 then
      begin
        gameVersionFound := True;
        Break;
      end;
    until sr.EndOfStream;
    sr.Free;

    if not gameVersionFound then
      raise Exception.Create(Format('Game version const not found in "%s"', [changelogFilename]));

    fOnLog(Format('Game version in "%s" - ok', [changelogFilename]));
  end;
end;


procedure TKMBuilderKP.Step03_ScanForDebugFlags(aConfig: TKMBuildConfiguration);
begin
  // DEFINEs:
  // - DEBUG
  // - DBG_SKIP_SECURE_AUTH
  // - DBG_PERFLOG
  // - DBG_DBG_RNG_SPY
  // KM_Defaults:
  // - DBG_ flags // I want to make a rule that every debug flag must be names DBG_*** and be set to False vy default (like in KP). Then any True one is a redflag

  fGameBuildFlags := '';

  // Scan game code for debug flags (ignore Utils for now)
  var pasFilesScanned: Integer;
  ScanForDebugFlagsInPas('.\src\',
    procedure (aFlag: TKMDebugScan)
    begin
      fOnLog(Format('%s [%d]: %s', [aFlag.FilePath, aFlag.LineNumber, aFlag.LineText]));
      fGameBuildFlags := fGameBuildFlags + IfThen(fGameBuildFlags <> '', ', ') + aFlag.FlagName;
    end,
    pasFilesScanned);
  fOnLog(Format('Scanned %d pas files', [pasFilesScanned]));

  // Scan game code for debug flags (ignore Utils for now)
  var incFilesScanned: Integer;
  ScanForDebugFlagsInInc('.\src\',
    procedure (aFlag: TKMDebugScan)
    begin
      fOnLog(Format('%s [%d]: %s', [aFlag.FilePath, aFlag.LineNumber, aFlag.LineText]));
      fGameBuildFlags := fGameBuildFlags + IfThen(fGameBuildFlags <> '', ', ') + aFlag.FlagName;
    end,
    incFilesScanned);
  fOnLog(Format('Scanned %d inc files', [incFilesScanned]));
end;


procedure TKMBuilderKP.Step04_WriteVersion(aConfig: TKMBuildConfiguration);
begin
  // Write revision number for the game exe and Launcher/Updater
  TFile.WriteAllText('.\KM_Revision.inc', #39 + 'r' + IntToStr(fBuildRevision) + #39);
  TFile.WriteAllText('.\version', Format('%s %d.%d r%d', [fBuildName, fBuildMinor, fBuildRelease, fBuildRevision]));

  var countVerInfoMajor := 0;
  var countVerInfoMinor := 0;
  var countVerInfoRelease := 0;
  var countVerInfoBuild := 0;
  var countFileVersion := 0;
  var countProductVersion := 0;

  var sl := TStringList.Create;
  try
    sl.LoadFromFile('.\KnightsProvince.dproj');
    for var I := 0 to sl.Count - 1 do
    begin
      var tagVerInfoMajorOpen := Pos('<VerInfo_MajorVer>', sl[I]);
      if tagVerInfoMajorOpen > 0 then
      begin
        sl[I] := Copy(sl[I], 1, tagVerInfoMajorOpen - 1) + Format('<VerInfo_MajorVer>%d</VerInfo_MajorVer>', [fBuildMajor]);
        Inc(countVerInfoMajor);
      end;
      var tagVerInfoMinorOpen := Pos('<VerInfo_MinorVer>', sl[I]);
      if tagVerInfoMinorOpen > 0 then
      begin
        sl[I] := Copy(sl[I], 1, tagVerInfoMinorOpen - 1) + Format('<VerInfo_MinorVer>%d</VerInfo_MinorVer>', [fBuildMinor]);
        Inc(countVerInfoMinor);
      end;
      var tagVerInfoReleaseOpen := Pos('<VerInfo_Release>', sl[I]);
      if tagVerInfoReleaseOpen > 0 then
      begin
        sl[I] := Copy(sl[I], 1, tagVerInfoReleaseOpen - 1) + Format('<VerInfo_Release>%d</VerInfo_Release>', [fBuildRelease]);
        Inc(countVerInfoRelease);
      end;
      var tagVerInfoBuildOpen := Pos('<VerInfo_Build>', sl[I]);
      if tagVerInfoBuildOpen > 0 then
      begin
        sl[I] := Copy(sl[I], 1, tagVerInfoBuildOpen - 1) + Format('<VerInfo_Build>%d</VerInfo_Build>', [fBuildRevision]);
        Inc(countVerInfoBuild);
      end;

      var tagVerInfoKeysOpen := Pos('<VerInfo_Keys>', sl[I]);
      if tagVerInfoKeysOpen > 0 then
      begin
        var tagFileVersionOpen := Pos('FileVersion=', sl[I]);
        var tagFileVersionClose := Pos(';', sl[I], tagFileVersionOpen);

        sl[I] := Copy(sl[I], 1, tagFileVersionOpen - 1) + Format('FileVersion=%d.%d.%d.%d', [fBuildMajor, fBuildMinor, fBuildRelease, fBuildRevision]) + Copy(sl[I], tagFileVersionClose, Length(sl[I]));
        Inc(countFileVersion);

        var tagProductVersionOpen := Pos('ProductVersion=', sl[I]);
        var tagProductVersionClose := Pos(';', sl[I], tagProductVersionOpen);

        sl[I] := Copy(sl[I], 1, tagProductVersionOpen - 1) + Format('ProductVersion=%d.%d.%d', [fBuildMajor, fBuildMinor, fBuildRelease]) + Copy(sl[I], tagProductVersionClose, Length(sl[I]));
        Inc(countProductVersion);
      end;
    end;

    // We save updated DPROJ even if it is malformed - since it is under VCS it will be trivial to inspect and revert
    sl.SaveToFile('.\KnightsProvince.dproj');
  finally
    sl.Free;
  end;

  // We expect every tag to be present exactly once
  if countVerInfoMajor <> 1 then   raise Exception.Create('VerInfo_Major encountered not once');
  if countVerInfoMinor <> 1 then   raise Exception.Create('VerInfo_Minor encountered not once');
  if countVerInfoRelease <> 1 then raise Exception.Create('VerInfo_Release encountered not once');
  if countVerInfoBuild <> 1 then   raise Exception.Create('VerInfo_Build encountered not once');
  if countFileVersion <> 1 then    raise Exception.Create('FileVersion encountered not once');
  if countProductVersion <> 1 then raise Exception.Create('ProductVersion encountered not once');
end;


procedure TKMBuilderKP.Step05_DeleteTempFiles(aConfig: TKMBuildConfiguration);
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
end;


procedure TKMBuilderKP.Step06_BuildGameExe(aConfig: TKMBuildConfiguration);
begin
  BuildWin(fDelphiRSVarsPath, 'KnightsProvince.dproj', aConfig, 'KnightsProvince.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'utils\ScriptValidator\ScriptValidator.dproj', aConfig, 'ScriptValidator.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'utils\TranslationManager (from kp-wiki)\TranslationManager.dproj', aConfig, 'utils\TranslationManager (from kp-wiki)\TranslationManager.exe');

  if CheckTerminated then Exit;

  BuildWin(fDelphiRSVarsPath, 'utils\KP_DedicatedServer\KP_DedicatedServer.dproj', aConfig, 'utils\KP_DedicatedServer\KP_DedicatedServer.exe');

  if CheckTerminated then Exit;

  BuildFpc(fFPCUPdeluxePath, 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x86');

  if CheckTerminated then Exit;

  BuildFpc(fFPCUPdeluxePath, 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64.lpi', 'utils\KP_DedicatedServer\KP_DedicatedServer_Linux_x64');
end;


procedure TKMBuilderKP.Step07_PatchGameExe(aConfig: TKMBuildConfiguration);
begin
  var exeSizeBefore := TFile.GetSize('KnightsProvince.exe');
  fOnLog(Format('Size before patch - %d bytes', [exeSizeBefore]));

  fOnLog('Patching KnightsProvince.exe');
  begin
    CheckFileExists('madExcept', fMadExceptPath);

    var s := Format('cmd.exe /C ""%s" "%s""', [fMadExceptPath, 'KnightsProvince.exe']);
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


procedure TKMBuilderKP.Step08_PackData(aConfig: TKMBuildConfiguration);
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


//todo -cBuilder: It seems to make more sense to run the tests ASAP (fail fast), so think about moving this step to be executed earlier
procedure TKMBuilderKP.Step09_RunTests(aConfig: TKMBuildConfiguration);
begin
  // Unit tests
  BuildWin(fDelphiRSVarsPath, 'utils\TestingUnitTests\TestingUnitTests.dproj', bcRelease, 'TestingUnitTests.exe');

  var cmdUnitTests := '.\TestingUnitTests.exe -test';
  var resUnitTests := CaptureConsoleOutput('.\', cmdUnitTests);
  fOnLog(resUnitTests);

  if Pos('UNIT TESTS PASSED', resUnitTests) = 0 then
    raise Exception.Create('Unit tests did not succeed');

  // Game tests
  BuildWin(fDelphiRSVarsPath, 'utils\TestingGameTests\TestingGameTests.dproj', bcRelease, 'TestingGameTests.exe');

  var cmdGameTests := '.\TestingGameTests.exe -test';
  var resGameTests := CaptureConsoleOutput('.\', cmdGameTests);
  fOnLog(resGameTests);

  if Pos('GAME TESTS PASSED', resGameTests) = 0 then
    raise Exception.Create('Game tests did not succeed');
end;


procedure TKMBuilderKP.Step10_ArrangeFolder(aConfig: TKMBuildConfiguration);
begin
  if DirectoryExists('.\' + fBuildFolder) then
  begin
    fOnLog(Format('Deleting old build folder of "%s"', [fBuildFolder]));
    TDirectory.Delete('.\' + fBuildFolder, True);
  end;

  ForceDirectories('.\' + fBuildFolder);

  CheckFolderExists('Maps repository', fMapsRepoPath);
  CopyFolder(fMapsRepoPath + 'campaigns\', fBuildFolder + 'campaigns\');
  CopyFolder(fMapsRepoPath + 'maps\', fBuildFolder + 'maps\');

  if CheckTerminated then Exit;

  //CopyFolder('.\ExtAI', fBuildFolder + 'ExtAI\');
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


procedure TKMBuilderKP.Step11_Pack7zip(aConfig: TKMBuildConfiguration);
begin
  CheckFileExists('7-zip', f7zipPath);

  // Delete old archive if we had it for some reason
  DeleteFileIfExists(fBuildResult7zip);

  var cmd7zip := Format('"%s" a -t7z -m0=lzma2 -mx=9 -mfb=64 -md=128m -ms=on "%s" "%s"', [f7zipPath, fBuildResult7zip, fBuildFolder]);
  var res := CaptureConsoleOutput('.\', cmd7zip);
  fOnLog(res);

  CheckFileExists('Resulting 7z archive', fBuildResult7zip);

  var szAfter := TFile.GetSize(fBuildResult7zip);
  fOnLog(Format('Size of 7z - %d bytes', [szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting 7z archive is too small?');
end;


procedure TKMBuilderKP.Step12_PackInstaller(aConfig: TKMBuildConfiguration);
begin
  // old - Knights Province (Alpha 13.2 r17800)
  // new - Knights Province Alpha 13.2.17800
  var appName := Format('%s %s %d.%d.%d)', [fGameName, fBuildName, fBuildMinor, fBuildRelease, fBuildRevision]);

  // Delete old installer if we had it for some reason
  DeleteFileIfExists(fBuildResultInstaller);

  var swConstants := TStreamWriter.Create('.\installer\Constants.iss');
  swConstants.WriteLine('; REVISION (write into Registry)');
  swConstants.WriteLine(Format('#define Revision '#39'r%d'#39, [fBuildRevision]));
  swConstants.WriteLine('');
  swConstants.WriteLine('; Folder from where files get taken');
  swConstants.WriteLine(Format('#define SourceFolder '#39'..\%s'#39, [fBuildFolder]));
  swConstants.WriteLine('');
  swConstants.WriteLine('; How the installer executable will be named');
  swConstants.WriteLine(Format('#define OutputInstallerName '#39'%s'#39, [ChangeFileExt(fBuildResultInstaller, '')]));
  swConstants.WriteLine('');
  swConstants.WriteLine('; Application name used in many places');
  swConstants.WriteLine(Format('#define MyAppName '#39'%s'#39, [appName]));
  swConstants.WriteLine('');
  swConstants.WriteLine(Format('#define MyAppExeName '#39'%s'#39, ['KnightsProvince.exe']));
  swConstants.WriteLine(Format('#define Website '#39'%s'#39, ['http://www.knightsprovince.com/']));
  swConstants.Free;

  if CheckTerminated then Exit;

  CheckFileExists('InnoSetup', fInnoSetupPath);
  var cmdInstaller := Format('"%s" ".\installer\InstallerFull.iss"', [fInnoSetupPath]);
  CaptureConsoleOutput2('.\', cmdInstaller, procedure (const aMsg: string) begin fOnLog(aMsg); end);

  var szAfter := TFile.GetSize(fBuildResultInstaller);
  fOnLog(Format('Size of "%s" - %d bytes', [fBuildResultInstaller, szAfter]));

  if szAfter <= 0 then
    raise Exception.Create('Resulting installer is too small?');
end;


procedure TKMBuilderKP.Step13_CreatePatch(aConfig: TKMBuildConfiguration);
begin
  var launcherFilename := ExpandFileName('.\Launcher.exe');
  var result7zipFilename := ExpandFileName('.\' + fBuildResult7zip);
  CheckFileExists('Launcher', launcherFilename);
  CheckFileExists('7-zip package', result7zipFilename);

  // Example:
  // old - .\Launcher.exe ".\kp2025-10-29 (Alpha 13 wip r17455).7z"
  // new - .\Launcher.exe ".\Knights Province Alpha 13.2.17455.7z"
  var cmdPatch := Format('%s "%s"', [launcherFilename, result7zipFilename]);
  CreateProcessSimple(cmdPatch, False, True, False);
end;


procedure TKMBuilderKP.Step14_RegisterOnKT(aConfig: TKMBuildConfiguration);
begin
  var ktAdminFilename := '.\KT_Admin.exe';
  CheckFileExists('KT Admin', ktAdminFilename);

  // Example:
  // old - ".\KT_Admin.exe" register "kp2025-10-29 (Alpha 13 wip r17455)" 13 17492
  // new - ".\KT_Admin.exe" register "Knights Province Alpha 13.2.17800" 13 17800
  var cmdKtAdmin := Format('"%s" register "%s" %d %d', [ktAdminFilename, fBuildFolder, fBuildMinor, fBuildRevision]);
  CreateProcessSimple(cmdKtAdmin, True, True, False);
end;


procedure TKMBuilderKP.Step15_CommitAndTag(aConfig: TKMBuildConfiguration);
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
