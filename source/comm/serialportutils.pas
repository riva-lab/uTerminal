
{ SerialPortUtils
  ------------------------------------------------------------------------------
  Unit provides simple class `TSerialEnumerator`
  for serial (COM) ports enumerating.
  Features:
   - get port COMxx identifier,
   - get port description,
   - get port availability state (check by connection attempt),
   - automatic list updating by timer,
   - event when updating is finished.
   -----------------------------------------------------------------------------
   (c) Riva, 2024
}
unit SerialPortUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, LazUTF8, synaser, controlwindevice;

type

  { TSerialEnumerator }

  TSerialEnumerator = class(TStringList)
  private
    FDesc:    TStringList;
    FBusy:    TStringList;
    FPrev:    TStringList;
    FTmr:     TTimer;
    FUpdate:  Boolean;
    FUpdated: Boolean;

    function GetBusy(Index: Integer): Boolean;
    function GetDescription(Index: Integer): String;

    procedure SetUpdate(AValue: Boolean);

    procedure OnTimer(Sender: TObject);
    procedure UpdateInternal;
    procedure Backup;
    procedure Compare;

    function GetDataString(Index: Integer): String;

  public
    CheckForBusy: Boolean; // if True then check availability by connection attempt
    Interval:     Integer; // update interval in milliseconds
    OnUpdate:     TNotifyEvent;

    constructor Create;
    destructor Destroy; override;

    procedure UpdateNow;

    property Update: Boolean read FUpdate write SetUpdate;
    property Updated: Boolean read FUpdated;
    property Description[Index: Integer]: String read GetDescription;
    property Busy[Index: Integer]: Boolean read GetBusy;
  end;


// check if port is connectable
function IsSerialPortFree(APort: String): Boolean;


var
  SerialEnumerator: TSerialEnumerator;

implementation


{ TSerialEnumerator }

procedure TSerialEnumerator.OnTimer(Sender: TObject);
  begin
    if Interval = 0 then FUpdate := False;
    UpdateNow;
  end;

// get list of available ports
procedure TSerialEnumerator.UpdateInternal;
  var
    devices: TStringList;
    i, j:    Integer;
  begin
    FDesc.Clear;
    FBusy.Clear;
    Clear;
    CommaText := GetSerialPortNames;
    Sort;

    if Count > 0 then
      for i := 0 to Count - 1 do
        FBusy.Add(BoolToStr(CheckForBusy and not IsSerialPortFree(Strings[i]), '1', '0'));

    {$IfDef WINDOWS}
      try
      // https://wiki.lazarus.freepascal.org/Windows_Programming_Tips#Enabling_and_disabling_devices
      devices := controlwindevice.LoadDevices(GUID_DEVCLASS_PORT);

      if Count * devices.Count > 0 then
        for i := 0 to Count - 1 do
          for j := 0 to devices.Count - 1 do
            if devices[j].EndsWith('(' + Strings[i] + ')') then
              begin
              FDesc.Add(WinCPToUTF8(devices[j]).Replace('(' + Strings[i] + ')', '').Trim);
              Break;
              end;
      finally
      devices.Free;
      end;
    {$EndIf}
  end;

procedure TSerialEnumerator.Backup;
  var
    i: Integer;
  begin
    FPrev.Clear;
    if Count > 0 then
      for i := 0 to Count - 1 do
        FPrev.Add(GetDataString(i));
  end;

procedure TSerialEnumerator.Compare;
  label
    _updatedExit;
  var
    i: Integer;
  begin
    if Count <> FPrev.Count then goto _updatedExit;

    if Count > 0 then
      for i := 0 to Count - 1 do
        if FPrev[i] <> GetDataString(i) then goto _updatedExit;

    FUpdated := False;
    Exit;

    _updatedExit:
      FUpdated := True;
  end;

function TSerialEnumerator.GetDataString(Index: Integer): String;
  begin
    if Index < 0 then Exit('');
    if Index >= Count then Exit('');
    Result := Format('%s,%s,%d', [
      Strings[Index], Description[Index], Busy[Index].ToInteger]);
  end;

function TSerialEnumerator.GetBusy(Index: Integer): Boolean;
  begin
    Result := False;
    if Index < 0 then Exit;
    if Index >= FBusy.Count then Exit;
    Result := FBusy[Index] = '1';
  end;

function TSerialEnumerator.GetDescription(Index: Integer): String;
  begin
    Result := '';
    if Index < 0 then Exit;
    if Index >= FDesc.Count then Exit;
    Result := FDesc[Index];
  end;

procedure TSerialEnumerator.SetUpdate(AValue: Boolean);
  begin
    if AValue = FUpdate then Exit;
    FUpdate       := AValue;
    FTmr.Enabled  := FUpdate;
    FTmr.Interval := Interval;
  end;

constructor TSerialEnumerator.Create;
  begin
    inherited Create;
    FDesc := TStringList.Create;
    FBusy := TStringList.Create;
    FPrev := TStringList.Create;
    FTmr  := TTimer.Create(nil);

    FTmr.Enabled := False;
    FTmr.OnTimer := @OnTimer;
    CheckForBusy := False;
    FUpdate      := False;
    FUpdated     := False;
    Interval     := 0;
    OnUpdate     := nil;
  end;

destructor TSerialEnumerator.Destroy;
  begin
    FTmr.Free;
    FDesc.Free;
    FBusy.Free;
    FPrev.Free;
    inherited Destroy;
  end;

procedure TSerialEnumerator.UpdateNow;
  begin
    Backup;
    UpdateInternal;
    if Assigned(OnUpdate) then OnUpdate(Self);
    Compare;
  end;



function IsSerialPortFree(APort: String): Boolean;
  begin
    Result := False;
    with TBlockSerial.Create do
      try
        try
        Connect(APort);
        Result := Handle <> System.THandle(-1);
        except
        Result := False;
        end;
      finally
      Flush;
      Purge;
      CloseSocket;
      Free;
      end;
  end;


initialization
  SerialEnumerator := TSerialEnumerator.Create;

end.
