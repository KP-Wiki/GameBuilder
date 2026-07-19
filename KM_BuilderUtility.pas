unit KM_BuilderUtility;
interface
uses
  System.SysUtils;


type
  TKMDebugScan = record
  public
    FilePath: string;
    LineNumber: Integer;
    LineText: string;
    FlagName: string;
    class function New(aFilePath: string; aLineNumber: Integer; aLineText: string; aFlagName: string): TKMDebugScan; static;
  end;

procedure ScanForCompilerDirectivesInPas(const aPath, aSkip, aIncText: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);
procedure ScanForDebugFlagsInPas(const aPath: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);
procedure ScanForDebugFlagsInInc(const aPath: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);


implementation
uses
  System.Classes, System.IOUtils, System.Math, System.StrUtils;


{ TKMDebugScan }
class function TKMDebugScan.New(aFilePath: string; aLineNumber: Integer; aLineText, aFlagName: string): TKMDebugScan;
begin
  Result := default(TKMDebugScan);
  Result.FilePath := aFilePath;
  Result.LineNumber := aLineNumber;
  Result.LineText := aLineText;
  Result.FlagName := aFlagName;
end;


procedure ScanForCompilerDirectivesInPas(const aPath, aSkip, aIncText: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);
begin
  var fullPath := ExpandFileName(aPath);
  var arrayFiles := TDirectory.GetFiles(fullPath, '*.pas', TSearchOption.soAllDirectories);

  aFilesScanned := Length(arrayFiles);

  var sl := TStringList.Create;
  for var I := 0 to High(arrayFiles) do
  begin
    var fname := arrayFiles[I];

    // Skip 3rdparty/ext files (we dont control them)
    if Pos(aSkip, fname) <> 0 then
      Continue;

    // Skip deprecated files (we dont care about them)
    if StartsText('_', ExtractFileName(fname)) then
      Continue;

    sl.LoadFromFile(fname);

    var incFound := False;
    for var K := 0 to 7 do
      if sl[K] = aIncText then
        incFound := True;

    if not incFound then
      aOnFlag(TKMDebugScan.New(ExtractRelativePath(fullPath, fname), 2, sl[1], 'Has no $I in first 8 lines'));

    Inc(aFilesScanned);
  end;
  sl.Free;
end;


procedure ScanForDebugFlagsInPas(const aPath: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);
begin
  var fullPath := ExpandFileName(aPath);
  var arrayFiles := TDirectory.GetFiles(fullPath, '*.pas', TSearchOption.soAllDirectories);

  aFilesScanned := Length(arrayFiles);

  var sl := TStringList.Create;
  for var I := 0 to High(arrayFiles) do
  begin
    var fname := arrayFiles[I];

    sl.LoadFromFile(fname);

    // Check for the code along the lines of DBG*** = True
    // Positive:
    //   Dbg ... True
    //   Dbg ... True .. //
    //   Dbg ... True .. }
    // Negative:
    //   Dbg ... // .. True
    //   // .. Dbg ... True
    //   Dbg ... then .. True
    //   function KaMRandom(aMax: Int64{$IFDEF DBG_RNG_SPY}; const aCaller: AnsiString; aLogRng: Boolean = True{$ENDIF}): Int64;
    for var K := 0 to sl.Count - 1 do
    begin
      var strLine := Trim(sl[K]);
      var posDebugFlag := Pos('dbg', LowerCase(strLine));
      var posTrue := PosEx('true', LowerCase(strLine), posDebugFlag);
      if (posDebugFlag > 0) and (posTrue > 0) then
      begin
        var posComment := Pos('//', LowerCase(strLine));
        var posBracket := Pos('}', LowerCase(strLine), posDebugFlag);
        var posThen := Pos(' then ', LowerCase(strLine), posDebugFlag);
        if ((posComment = 0) or (posComment > posTrue))
        and ((posBracket = 0) or (posBracket > posTrue))
        and ((posThen = 0) or (posThen > posTrue)) then
        begin
          // Try to find the var/const end
          var posSpace := Pos(' ', strLine, posDebugFlag);
          var posColon := Pos(':', strLine, posDebugFlag);
          var posEqual := Pos('=', strLine, posDebugFlag);

          var posDebugFlagEnd := Length(strLine);
          if posSpace > 0 then posDebugFlagEnd := Min(posDebugFlagEnd, posSpace);
          if posColon > 0 then posDebugFlagEnd := Min(posDebugFlagEnd, posColon);
          if posEqual > 0 then posDebugFlagEnd := Min(posDebugFlagEnd, posEqual);

          aOnFlag(TKMDebugScan.New(ExtractRelativePath(fullPath, fname), K+1, strLine, Copy(strLine, posDebugFlag, posDebugFlagEnd - posDebugFlag)));
          Inc(aFilesScanned);
        end;
      end;
    end;
  end;
  sl.Free;
end;


procedure ScanForDebugFlagsInInc(const aPath: string; aOnFlag: TProc<TKMDebugScan>; out aFilesScanned: Integer);
begin
  var fullPath := ExpandFileName(aPath);
  var arrayFiles := TDirectory.GetFiles(fullPath, '*.inc', TSearchOption.soAllDirectories);

  aFilesScanned := Length(arrayFiles);

  var sl := TStringList.Create;
  for var I := 0 to High(arrayFiles) do
  begin
    var fname := arrayFiles[I];

    sl.LoadFromFile(fname);

    // Check for {$DEFINE DBG_***
    for var K := 0 to sl.Count - 1 do
    begin
      var strLine := Trim(sl[K]);
      var posDebugFlag := Pos('{$define dbg', LowerCase(strLine));
      if posDebugFlag > 0 then
      begin
        posDebugFlag := Pos('dbg', LowerCase(strLine), posDebugFlag);
        var posDebugFlagEnd := Pos('}', strLine, posDebugFlag);
        aOnFlag(TKMDebugScan.New(ExtractRelativePath(fullPath, fname), K+1, strLine, Copy(strLine, posDebugFlag, posDebugFlagEnd - posDebugFlag)));
        Inc(aFilesScanned);
      end;
    end;
  end;
  sl.Free;
end;


end.
