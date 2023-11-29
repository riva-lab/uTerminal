unit u_serial;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, registry, synaser, synautil, u_utilities, LazUTF8;

resourcestring
  TXT_CS_OFFLINE  = 'Отключен';
  TXT_CS_ONLINE   = 'Готов';
  TXT_CS_RXING    = 'Прием...';
  TXT_CS_TXING    = 'Передача...';

  TXT_CE_NONE     = 'Успех';
  TXT_CE_RXERROR  = 'Ошибка приема';
  TXT_CE_BUSY     = 'Порт недоступен';
  TXT_CE_TXERROR  = 'Ошибка передачи';
  TXT_CE_TXCANCEL = 'Передача отменена';

  TXT_PORT_BUSY   = 'занят';

type
  TNewDataProc  = procedure of object;
  TCommError    = (ceNone, cePortBusy, ceRxError, ceTxError, ceTxCancel);
  TSerialSignal = (ssCTS, ssDSR, ssRing, ssRLSD, ssRTS, ssDTR, ssBreak);

  //ESerialReadTimeOut = class(Exception);


  {
    Класс TSerialPortThread
    Предназначен для связи через последовательный COM-порт.
    Работает в своем отдельном потоке для предотвращения
    зависания пользовательского интерфейса приложения.
  }

  { TSerialPortThread }

  TSerialPortThread = class(TThread)
  private
  const
    TIME_POLL_RX             = 30;          // [мс] период опроса входного буфера
    TIME_BREAK_DEFAULT       = 100;         // [мс] длительность сигнала Break
    TIME_RX_DEFAULT          = 1000;        // [мс] таймаут пакета при приеме
    TIME_TX_DEFAULT          = 1000;        // [мс] период автопередачи
    TIME_TX_DEADLOCK_DEFAULT = 1000;        // [мс] период автопередачи

  private
    FSerialTest:       TBlockSerial;
    FSerial:           TBlockSerial;
    FPortsList:        TStringList;
    FPortIndexInList:  Integer;
    FPort:             String;
    FPortName:         String;
    FPortPrevious:     String;
    FTimestampStrAfter: String;
    FTimestampStrBefore: String;
    FPortBaud:         Integer;
    FPortDataBits:     Integer;
    FPortBPS:          Integer;
    FTxPacketOffset:   Integer;
    FTxPackets:        Integer;
    FTxPacketSize:     Integer;
    FAutoSendInterval: LongWord;
    FPortParity:       Char;
    FPortStopBits:     Integer;
    FDeadlockTimeout:  Integer;
    FBreakDuration:    Integer;
    FRxPacketTime:     Integer;
    FStarted:          Boolean;
    FHardflow:         Boolean;
    FRxEnable:         Boolean;
    FRunning:          Boolean;
    FConnected:        Boolean;
    FTxStart:          Boolean;
    FIsRxing:          Boolean;
    FIsTxing:          Boolean;
    FCheckPort:        Boolean;
    FEnableRxTimestamp: Boolean;
    FAutoSend:         Boolean;
    FError:            TCommError;
    FOnRxEnd:          TNewDataProc;
    FOnRxStart:        TNewDataProc;
    FOnTxEnd:          TNewDataProc;
    FOnTxStart:        TNewDataProc;
    FDataRx:           Ansistring;
    FDataTx:           Ansistring;
    FDataTxAns:        Ansistring;
    FSignals:          array [TSerialSignal] of Boolean;

  private
    function GetConfigStr(AShort: Boolean): String;
    function GetConfigShort: String;
    function GetConfigString: String;
    function GetErrorString: String;
    function GetSignal(Index: TSerialSignal): Boolean;
    procedure SetBreakDuration(AValue: Integer);
    procedure SetSignal(Index: TSerialSignal; AValue: Boolean);

    function PortOpened: Boolean;
    procedure ClosePort;
    function OpenPort: Boolean;
    function IsPortReady: Boolean;
    function IsPortFree(APort: String): Boolean;

    procedure ReceiveData(ATickMs: LongWord);
    procedure TransmitData;
    function BreakControl(ATickMs: LongWord): Boolean;
    procedure OnStatus(Sender: TObject; Reason: THookSerialReason; const Value: String);

    procedure Execute; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure PortSettings(APort: String; ABaud, ADataBits: Integer;
      AParity: Char; AStopBits: Integer);

    procedure ResetError;

    procedure Transmit;
    procedure TransmitAnswer(AAnswer: String);

    function GetExistingPorts: String;
    function GetPortIndexInList: Integer;

  public
    property OnRxEnd: TNewDataProc read FOnRxEnd write FOnRxEnd;
    property OnRxStart: TNewDataProc read FOnRxStart write FOnRxStart;
    property OnTxEnd: TNewDataProc read FOnTxEnd write FOnTxEnd;
    property OnTxStart: TNewDataProc read FOnTxStart write FOnTxStart;

    property Started: Boolean read FStarted write FStarted;
    property RxEnable: Boolean read FRxEnable write FRxEnable;
    property AutoSend: Boolean read FAutoSend write FAutoSend;
    property AutoSendInterval: LongWord read FAutoSendInterval write FAutoSendInterval;
    property CheckPort: Boolean read FCheckPort write FCheckPort;
    property Hardflow: Boolean read FHardflow write FHardflow;
    property Signal[Index: TSerialSignal]: Boolean read GetSignal write SetSignal;
    property DeadlockTimeout: Integer read FDeadlockTimeout write FDeadlockTimeout;
    property BreakDuration: Integer read FBreakDuration write SetBreakDuration;
    property EnableRxTimestamp: Boolean read FEnableRxTimestamp write FEnableRxTimestamp;
    property RxPacketTime: Integer read FRxPacketTime write FRxPacketTime;
    property TimestampStrBefore: String read FTimestampStrBefore write FTimestampStrBefore;
    property TimestampStrAfter: String read FTimestampStrAfter write FTimestampStrAfter;

    property Port: String read FPort;
    property Connected: Boolean read FConnected;
    property IsRxing: Boolean read FIsRxing;
    property IsTxing: Boolean read FIsTxing;
    property Error: TCommError read FError;

    property DataRx: Ansistring read FDataRx;
    property DataTx: Ansistring write FDataTx;

    property ErrorString: String read GetErrorString;
    property ConfigString: String read GetConfigString;
    property ConfigShort: String read GetConfigShort;

    //property Serial: TBlockSerial read FSerial write FSerial; // debug
  end;


implementation

 {  ***  Класс  TSerialPortThread  ***  }

 { Методы доступа к свойствам класса get/set }

function TSerialPortThread.GetErrorString: String;
  begin
    case FError of
      ceNone: Result     := TXT_CE_NONE;
      cePortBusy: Result := TXT_CE_BUSY;
      ceRxError: Result  := TXT_CE_RXERROR;
      ceTxError: Result  := TXT_CE_TXERROR;
      ceTxCancel: Result := TXT_CE_TXCANCEL;
      end;
  end;

function TSerialPortThread.GetConfigStr(AShort: Boolean): String;
  var
    sb: array [0..2] of String = ('1', '1,5', '2');
  begin
    Result := AShort.Select(FPort, FPortName) + ', ';
    Result += FPortDataBits.ToString + '-';
    Result += FPortParity + '-';
    Result += sb[FPortStopBits] + ', ';
    Result += FPortBaud.ToString;
  end;

function TSerialPortThread.GetConfigShort: String;
  begin
    Result := GetConfigStr(True);
  end;

function TSerialPortThread.GetConfigString: String;
  begin
    Result := GetConfigStr(False);
  end;

function TSerialPortThread.GetSignal(Index: TSerialSignal): Boolean;
  begin
    case Index of
      ssCTS: Result  := FSerial.CTS;
      ssDSR: Result  := FSerial.DSR;
      ssRing: Result := FSerial.Ring;
      ssRLSD: Result := FSerial.Carrier
      else
        Result       := FSignals[Index];
      end;
  end;

procedure TSerialPortThread.SetSignal(Index: TSerialSignal; AValue: Boolean);
  begin
    if PortOpened then
        try
        if not FHardflow then
          case Index of
            ssRTS: FSerial.RTS := AValue;
            ssDTR: FSerial.DTR := AValue;
            end;

        if Index >= ssRTS then
          FSignals[Index] := AValue;
        finally
        end;
  end;

procedure TSerialPortThread.SetBreakDuration(AValue: Integer);
  begin
    if FBreakDuration = AValue then Exit;

    if AValue < 0 then
      FBreakDuration := TIME_BREAK_DEFAULT else
      FBreakDuration := AValue;
  end;


{ Управление подключением к COM-порту }

procedure TSerialPortThread.ClosePort;
  begin
    if PortOpened then
      with FSerial do
          try
          Flush;
          Purge;
          CloseSocket;
          finally
          //FPort := '-';
          end;
  end;

function TSerialPortThread.OpenPort: Boolean;
  begin
    with FSerial do
        try
        Connect(FPort);
        Config(FPortBaud, FPortDataBits, FPortParity, FPortStopBits, False, FHardflow);
        Result := PortOpened;
        except
        ClosePort;
        Result := False;
        end;

    FStarted     := FStarted and Result;
    if not Result then
      FError     := cePortBusy else
      begin
      FError     := ceNone;
      FTxStart   := False;
      FIsTxing   := False;
      FIsRxing   := False;
      FDataRx    := '';
      FDataTx    := '';
      FDataTxAns := '';

      SetSignal(ssRTS, FHardflow);
      SetSignal(ssDTR, FHardflow);
      end;
  end;

function TSerialPortThread.PortOpened: Boolean;
  begin
      try
      Exit(FSerial.Handle <> System.THandle(-1));
      except
      Exit(False);
      end;
  end;

procedure TSerialPortThread.PortSettings(APort: String; ABaud,
  ADataBits: Integer; AParity: Char; AStopBits: Integer);
  var
    lastOpened: Boolean;
    i:          Integer;

  function CheckRange(AValue, AMin, AMax: Variant): Integer;
    begin
      Result := AValue;
      if (AValue < AMin) then  Result := AMin;
      if (AValue > AMax) then  Result := AMax;
    end;

  begin
    FPortBaud     := CheckRange(ABaud, 50, 4000000);
    FPortDataBits := ADataBits;
    FPortStopBits := AStopBits;

    if AParity in ['N', 'E', 'O', 'M', 'S'] then
      FPortParity := AParity;

    if APort.StartsWith('COM') then
      begin
      FPort := 'COM';
      for i := 4 to APort.Length do
        if APort[i] in ['0'..'9'] then
          FPort += APort[i]
        else
          break;
      FPortName := APort;
      end
    else
      FPort     := '---';

    lastOpened := PortOpened;                 // считываем состояние порта до переконфигурации
    if FPortPrevious <> FPort then ClosePort; // закрываем порт, если изменяется именно порт
    if lastOpened then OpenPort;              // переоткрываем порт, если он был открыт ранее

    FPortPrevious := FPort;

    // расчет скорости передачи, байт/с
    FPortBPS := trunc(FPortBaud / (FPortDataBits + (FPortParity <> 'N').ToInteger +
      (FPortStopBits > 0).ToInteger + 3));
  end;

function TSerialPortThread.IsPortReady: Boolean;
  begin
    Result := False;

    if not FStarted then
      ClosePort
    else
    if PortOpened then
      Result := True
    else
      Result := OpenPort;
  end;

function TSerialPortThread.IsPortFree(APort: String): Boolean;
  begin
    Result := False;
    with FSerialTest do
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
        end;
  end;

procedure TSerialPortThread.ResetError;
  begin
    FError := ceNone;
  end;


{ Прием данных через COM-порт }

procedure TSerialPortThread.ReceiveData(ATickMs: LongWord);
  const
    timeLast: LongWord = 0;
  begin
      try
      FIsRxing := FSerial.WaitingData <> 0;
      except
      FIsRxing := False;
      end;

    if FIsRxing then
      begin
      FError := ceNone;
      Synchronize(FOnRxStart);

        try
        FDataRx := FSerial.RecvPacket(0);

        // добавление временной метки
        if FEnableRxTimestamp then
          begin
          if TickDelta(timeLast, ATickMs) > FRxPacketTime then
            FDataRx := LineEnding + FTimestampStrBefore + DateTimeToStr(Now)
              + FDataRx.Length.ToString + ' bytes'
              + FTimestampStrAfter + LineEnding + FDataRx;

          timeLast := ATickMs;
          end;

        except
        FError := ceRxError;
        end;

      if not FRxEnable then FDataRx := '';
      Synchronize(FOnRxEnd);
      end;
  end;


{ Передача данных через COM-порт }


procedure TSerialPortThread.Transmit;
  begin
    FTxStart := FConnected and not FIsTxing;
  end;

procedure TSerialPortThread.TransmitAnswer(AAnswer: String);
  begin
    FDataTxAns := AAnswer;
    Transmit;
  end;

procedure TSerialPortThread.TransmitData;
  var
    flagEnd: Boolean = False;
  begin
    if not FTxStart then
      begin
      FSerial.DeadlockTimeout := FAutoSend.Select(FAutoSendInterval, FDeadlockTimeout);
      Exit;
      end;

    if not FIsTxing then
      begin
      if FDataTxAns = '' then
        Synchronize(FOnTxStart) else
        FDataTx := FDataTxAns;

      if FDataTx.Length = 0 then
        begin
        FTxStart := False;
        Exit;
        end;

      FTxPacketSize   := FPortBPS * FSerial.DeadlockTimeout div 1000;
      FTxPacketOffset := 0;
      FError          := ceNone;
      FIsTxing        := True;

        try
        // запись отдельными пакетами во избежание потери данных из-за таймаута передачи
        while FTxPacketOffset < FDataTx.Length do
          begin
          FSerial.SendString(FDataTx.Substring(FTxPacketOffset, FTxPacketSize));
          FTxPacketOffset += FTxPacketSize;
          end;
        except
        FError   := ceTxError;
        FTxStart := False;
        FIsTxing := False;
        end;
      end
    else
        try
        flagEnd := FSerial.SendingData = 0;
        finally
        if flagEnd then
          begin
          FTxStart := False;
          FIsTxing := False;

          if FDataTxAns = '' then
            Synchronize(FOnTxEnd) else
            FDataTxAns := '';
          end;
        end;
  end;

function TSerialPortThread.BreakControl(ATickMs: LongWord): Boolean;
  const
    tmr: Integer = 0;     // static var
    brk: Boolean = False; // static var
  begin
    if not brk and FSignals[ssBreak] then
      begin
      tmr := ATickMs;
      brk := True;
      SetCommBreak(FSerial.Handle);
      end;

    if TickDelta(tmr, ATickMs) >= FBreakDuration then
      begin
      ClearCommBreak(FSerial.Handle);
      FSignals[ssBreak] := False;
      brk               := False;
      end;

    Result := brk;
  end;

procedure TSerialPortThread.OnStatus(Sender: TObject; Reason: THookSerialReason;
  const Value: String);
  begin
    // ошибка передачи (обычно по таймауту)
    if (Reason = HR_WriteCount) and (FDataTx.Length <> Value.ToInteger) then
      FError := ceTxError;
  end;


procedure TSerialPortThread.Execute;
  var
    tmr, tmrAS, tick: LongWord;
  begin
    FRunning := True;
    tmr      := GetTick;
    tmrAS    := GetTick;

    while not Terminated do
      begin
      tick       := GetTick;
      FConnected := IsPortReady;

      if FConnected then
        begin

        // прием
        if TickDelta(tmr, tick) >= TIME_POLL_RX then
          begin
          tmr := tick;
          ReceiveData(tick);
          end;

        // автопередача по таймеру
        if FAutoSend and not FIsTxing
          and (TickDelta(tmrAS, tick) >= FAutoSendInterval) then
          begin
          tmrAS := tick;
          Transmit;
          end;

        // передача
        if not BreakControl(tick) then
          TransmitData;

        Sleep(1);
        end
      else
        Sleep(TIME_POLL_RX);
      end;

    ClosePort;
    FRunning := False;
    FStarted := False;
  end;


{ Конструктор / деструктор }

constructor TSerialPortThread.Create;
  begin
    FreeOnTerminate := False;

    FPortsList          := TStringList.Create;
    FSerialTest         := TBlockSerial.Create;
    FSerial             := TBlockSerial.Create;
    FSerial.OnStatus    := @OnStatus;
    FRxEnable           := True;
    FCheckPort          := True;
    FStarted            := False;
    FHardflow           := False;
    FRunning            := False;
    FTxStart            := False;
    FConnected          := False;
    FEnableRxTimestamp  := False;
    FAutoSend           := False;
    FError              := ceNone;
    FPortIndexInList    := -1;
    FAutoSendInterval   := TIME_TX_DEFAULT;
    FDeadlockTimeout    := TIME_TX_DEADLOCK_DEFAULT;
    FBreakDuration      := TIME_BREAK_DEFAULT;
    FRxPacketTime       := TIME_RX_DEFAULT;
    FTimestampStrBefore := '[';
    FTimestampStrAfter  := '] >>>';
    FDataRx             := '';
    FDataTx             := '';
    FDataTxAns          := '';

    PortSettings('COM1', 9600, 8, 'N', SB1);

    // формат даты для временной метки
    FormatSettings.ShortDateFormat := 'yyyy/mm/dd';

    inherited Create(True);
  end;

destructor TSerialPortThread.Destroy;
  begin
    while FRunning do
      sleep(1);

    FreeAndNil(FPortsList);
    FreeAndNil(FSerial);
    FreeAndNil(FSerialTest);

    inherited Destroy;
  end;

{$IFDEF WINDOWS}
// получение списка доступных портов
//http://patotech.blogspot.com/2012/04/enumerate-com-ports-in-windows-with.html
function TSerialPortThread.GetExistingPorts: String;
  var
    reg:       TRegistry;
    l, v:      TStringList;
    n:         Integer;
    pn, fn, p: String;

  function findFriendlyName(key: String; port: String): String;
    var
      r:  TRegistry;
      k:  TStringList;
      i:  Integer;
      ck: String;
      rs: String;
    begin
      r := TRegistry.Create;
      k := TStringList.Create;

      r.RootKey := HKEY_LOCAL_MACHINE;
      r.OpenKeyReadOnly(key);
      r.GetKeyNames(k);
      r.CloseKey;

        try
        for i := 0 to k.Count - 1 do
          begin
          ck := key + k[i] + '\'; // current key
          // looking for "PortName" stringvalue in "Device Parameters" subkey
          if r.OpenKeyReadOnly(ck + 'Device Parameters') then
            begin
            if r.ReadString('PortName') = port then
              begin
              //Memo1.Lines.Add('--> ' + ck);
              r.CloseKey;
              r.OpenKeyReadOnly(ck);
              rs := r.ReadString('FriendlyName');
              Break;
              end; // if r.ReadString('PortName') = port ...
            end    // if r.OpenKeyReadOnly(ck + 'Device Parameters') ...
                   // keep looking on subkeys for "PortName"
          else // if not r.OpenKeyReadOnly(ck + 'Device Parameters') ...
            begin
            if r.OpenKeyReadOnly(ck) and r.HasSubKeys then
              begin
              rs := findFriendlyName(ck, port);
              if rs <> '' then Break;
              end; // if not (r.OpenKeyReadOnly(ck) and r.HasSubKeys) ...
            end;   // if not r.OpenKeyReadOnly(ck + 'Device Parameters') ...
          end; // for i := 0 to k.Count - 1 ...
        rs     := rs.Remove(rs.IndexOf('(') - 1);
        Result := rs;
        finally
        r.Free;
        k.Free;
        end; // try ...
    end;     // function findFriendlyName ...

  begin
    v      := TStringList.Create;
    l      := TStringList.Create;
    reg    := TRegistry.Create;
    p      := '';
    Result := '';

      try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      if reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
        begin
        reg.GetValueNames(l);

        for n := 0 to l.Count - 1 do
          begin
          pn := reg.ReadString(l[n]);
          fn := '   [' + SysToUTF8(findFriendlyName('\System\CurrentControlSet\Enum\', pn)) + ']';
          if FCheckPort and not IsPortFree(pn) then fn += ' <' + TXT_PORT_BUSY + '>';
          if pn = FPort then p := pn + fn;
          v.Add(pn + fn);
          end; // for n := 0 to l.Count - 1 ...

        v.Sort;
        FPortIndexInList := v.IndexOf(p);
        Result           := v.CommaText;
        end; // if reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') ...
      finally
      reg.Free;
      v.Free;
      l.Free;
      end; // try ...

    // только имена портов, без FriendlyName
    //FPortsList.CommaText := GetSerialPortNames;
    //FPortsList.Sort;
    //Result := FPortsList.CommaText;
  end;
{$ENDIF}

function TSerialPortThread.GetPortIndexInList: Integer;
  begin
    Result := FPortIndexInList;
  end;


end.
