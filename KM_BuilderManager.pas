unit KM_BuilderManager;
interface
uses
  System.Classes, System.SysUtils,
  KM_BuilderCommon;


type
  TKMBuilderGame = (
    bgUnknown,  // Default uninitialzed value
    bgKMR,      // KaM Remake
    bgKP        // Knights Province
  );


  TKMBuilderManager = class
  protected
    fBuilderClass: TKMBuilderClass;
    fBuilderSwatch: TKMBuilder; // Builder swatch to keep reference Configurations and Steps
    fBuilderActive: TKMBuilder;       // Actual builder

    fOnLog: TProc<string>;
    fOnStepBegin: TKMEventStepBegin;
    fOnStepDone: TKMEventStepDone;
    fOnDone: TProc;
    fWorker: TThread;
  public
    constructor Create(aGame: TKMBuilderGame; aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
    procedure ExecuteScenario(aScenario: Integer);
    procedure ExecuteStep(aStep: Integer);
    procedure ExecuteWholeProjectGroup;
    procedure Stop;

    // Utility getters
    function GetInfo: string;
    function GetScenarioCount: Integer;
    function GetScenarioName(aScenario: Integer): string;
    function GetScenarioContainsStep(aScenario, aStep: Integer): Boolean;
    function GetStepCount: Integer;
    function GetStepName(aStep: Integer): string;
  end;


implementation
uses
  KM_BuilderKMR, KM_BuilderKP;


{ TKMBuilderManager }
constructor TKMBuilderManager.Create(aGame: TKMBuilderGame; aOnLog: TProc<string>; aOnStepBegin: TKMEventStepBegin; aOnStepDone: TKMEventStepDone; aOnDone: TProc);
begin
  inherited Create;

  case aGame of
    bgKMR:      fBuilderClass := TKMBuilderKMR;
    bgKP:       fBuilderClass := TKMBuilderKP;
  else
    raise Exception.Create('Unknown game');
  end;

  fOnLog := aOnLog;
  fOnStepBegin := aOnStepBegin;
  fOnStepDone := aOnStepDone;
  fOnDone := aOnDone;

  fBuilderSwatch := fBuilderClass.Create(-1, nil, nil, nil, nil);
end;


function TKMBuilderManager.GetInfo: string;
begin
  if fBuilderActive <> nil then
    Result := fBuilderActive.GetInfo
  else
    Result := fBuilderSwatch.GetInfo;
end;


function TKMBuilderManager.GetScenarioCount: Integer;
begin
  Result := fBuilderSwatch.GetScenarioCount;
end;


function TKMBuilderManager.GetScenarioName(aScenario: Integer): string;
begin
  Result := fBuilderSwatch.GetScenarioName(aScenario);
end;


function TKMBuilderManager.GetScenarioContainsStep(aScenario, aStep: Integer): Boolean;
begin
  Result := fBuilderSwatch.GetScenarioContainsStep(aScenario, aStep);
end;


function TKMBuilderManager.GetStepCount: Integer;
begin
  Result := fBuilderSwatch.GetStepCount;
end;


function TKMBuilderManager.GetStepName(aStep: Integer): string;
begin
  Result := fBuilderSwatch.GetStepName(aStep);
end;


procedure TKMBuilderManager.ExecuteScenario(aScenario: Integer);
begin
  fBuilderActive.Free;
  fBuilderActive := fBuilderClass.Create(aScenario, fOnLog, fOnStepBegin, fOnStepDone, fOnDone);
  fBuilderActive.ExecuteConfig(aScenario);
end;


procedure TKMBuilderManager.ExecuteStep(aStep: Integer);
begin
  if fBuilderActive = nil then
  begin
    //fBuilderActive.Free;
    var buildConfig := 0; //todo: We dont have selector for Config yet
    fBuilderActive := fBuilderClass.Create(buildConfig, fOnLog, fOnStepBegin, fOnStepDone, fOnDone);
  end;
  fBuilderActive.ExecuteStep(aStep);
end;


procedure TKMBuilderManager.ExecuteWholeProjectGroup;
begin
  fBuilderActive.Free;
  var buildConfig := 0; //todo: We dont have selector for Config yet
  fBuilderActive := fBuilderClass.Create(buildConfig, fOnLog, fOnStepBegin, fOnStepDone, fOnDone);
  fBuilderActive.ExecuteWholeProjectGroup;
end;


procedure TKMBuilderManager.Stop;
begin
  FreeAndNil(fBuilderActive);
  // Builder worker will terminate when it can
end;


end.
