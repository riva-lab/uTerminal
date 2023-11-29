unit u_plotter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Math, u_utilities, uregexpr, FPImage,
  u_plotter_types, u_plotter_points, u_plotter_charts;

type

  TPlotterCallback    = procedure(ACharts: TPlotterChartsList) of object;
  TPlotterCmdCallback = procedure(ACommand: String; AValue: Double; AHexLen: Integer) of object;

  { TPlotterParser }

  TPlotterParser = class
  private
    FOnParseDone:    TPlotterCallback;
    FOnCommand:      TPlotterCmdCallback;
    FProtocol:       TPlotterProtocol;
    FView:           TPlotterView;
    FRE:             TRegExpr;
    FTimer:          TTimer;
    FCharts:         TPlotterChartsList;
    FChartsBuf:      TPlotterChartsList;
    FFormatSettings: TFormatSettings;

    FInput:          String;
    FRawLabel:       String;
    FREString:       String;
    FRELabel:        String;
    FREValue:        String;
    FREBegin:        String;
    FLinesCount:     Integer;
    FDataSize:       Integer;
    FSample:         Integer;
    FSamplePrev:     Integer;
    FMaxLines:       Integer;
    FCurrentLine:    Integer;
    FInputIndex:     Integer;
    FRawLines:       Integer;
    FWindowPosBU:    Integer;
    FWindowSize:     Integer;
    FWindowPos:      Integer;
    FWindowClear:    Integer;
    FPause:          Boolean;
    FSettingsFailed: Boolean;
    FAfterReset:     Boolean;
    FAfterResetRE:   Boolean;
    FHandlerAllow:   Boolean;
    FHandlerActive:  Boolean;
    FWindowReset:    Boolean;
    FWindowResetBU:  Boolean;
    FRegExpCaseCare: Boolean;

  private
    procedure AddValue(ACaption: String; AValue: Double; ALine: Integer = -1);

    procedure Parse;

    function ParseRaw(ASize: Integer; ASinged: Boolean): Boolean;
    function ParseArduino: Boolean;
    function ParseRegExp: Boolean;

    procedure SetInput(AValue: String);
    procedure SetProtocol(AValue: TPlotterProtocol);
    procedure SetView(AValue: TPlotterView);

    procedure UpdateOnTimer(Sender: TObject);
    procedure SetPause(AValue: Boolean);

  public
    constructor Create(AMaxLines: Integer);
    destructor Destroy; override;

    procedure Reset;
    procedure WindowPositionRestore;
    procedure WindowPositionSave;
    procedure WindowPositionIncrement;

    // it decreases samples value only!
    procedure DeleteLastSamples(ACount: Integer);

    property OnParseDone: TPlotterCallback read FOnParseDone write FOnParseDone;
    property OnCommand: TPlotterCmdCallback read FOnCommand write FOnCommand;

    property Input: String read FInput write SetInput;
    property Protocol: TPlotterProtocol read FProtocol write SetProtocol;
    property View: TPlotterView read FView write SetView;
    property Pause: Boolean read FPause write SetPause;
    //property SettingsFailed: Boolean read FSettingsFailed;

    property Samples: Integer read FSample;
    property DataSize: Integer read FDataSize;
    property LinesCount: Integer read FLinesCount;
    property Charts: TPlotterChartsList read FCharts;

    property RegExpString: String read FREString write FREString;
    property RegExpValue: String read FREValue write FREValue;
    property RegExpLabel: String read FRELabel write FRELabel;
    property RegExpCaseCare: Boolean read FRegExpCaseCare write FRegExpCaseCare;

    property WindowSize: Integer read FWindowSize write FWindowSize;
    property WindowClear: Integer read FWindowClear write FWindowClear;
    property WindowPos: Integer read FWindowPos;
    property WindowReset: Boolean read FWindowReset;
  end;


implementation


{ TPlotterParser }

procedure TPlotterParser.AddValue(ACaption: String; AValue: Double; ALine: Integer);
  const
    _x: Double = 0;
  begin
    if ALine >= 0 then FCurrentLine := ALine;

    if FView <> pv2D then
      _x := FSample;

    if (FView = pv2D) and (FCurrentLine = 0) then
      begin
      FCharts.Add(ACaption, 0, 0, NaN);
      _x := AValue;
      end
    else
      FCharts.Add(ACaption, FCurrentLine, _x, AValue);
  end;

procedure TPlotterParser.Parse;
  var
    _Result: Boolean;
  begin
    while FHandlerActive do Sleep(2); // wait for the end of data output

    repeat
      _Result      := False;
      FCurrentLine := 0;

      case FProtocol of
        ppRawU8: _Result   := ParseRaw(1, False);
        ppRawU16: _Result  := ParseRaw(2, False);
        ppRawU24: _Result  := ParseRaw(3, False);
        ppRawU32: _Result  := ParseRaw(4, False);
        ppRawI8: _Result   := ParseRaw(1, True);
        ppRawI16: _Result  := ParseRaw(2, True);
        ppRawI24: _Result  := ParseRaw(3, True);
        ppRawI32: _Result  := ParseRaw(4, True);
        ppArduino: _Result := ParseArduino;
        ppRegExp: _Result  := ParseRegExp;
        end;

      if not _Result then break;

      FSample += 1; // sample is a simple serial counter

    until False;

    FLinesCount   := FCharts.ActiveLines;
    FHandlerAllow := True; // allow data output by timer

    // if current mode is pvSweep, then output immediatelly
    if FView = pvSweep then UpdateOnTimer(nil);
  end;

procedure TPlotterParser.UpdateOnTimer(Sender: TObject);
  begin
    // output data if it is available and output is allowed
    if (FSample > FSamplePrev) and FHandlerAllow then
      begin
      FSamplePrev    := FSample;
      FHandlerActive := True;
      FOnParseDone(FCharts);
      FCharts.ClearAll;
      FHandlerAllow  := False;
      FHandlerActive := False;
      end;
  end;


function TPlotterParser.ParseRaw(ASize: Integer; ASinged: Boolean): Boolean;

  function GetNumber: LongWord; inline;
    var
      i: Integer;
    begin
      Result   := 0;
      for i    := 1 to ASize do
        begin
        Result := Result shl 8;
        Result += Ord(FInput[i]);
        end;
    end;

  function Int24(AInput: LongWord): Int32; inline;
    begin
      if (AInput and $800000) <> 0 then
        AInput := AInput or $FF000000;
      Result   := Int32(AInput);
    end;

  function ConvertToDouble(AInput: LongWord): Double; inline;
    begin
      if not ASinged then
        Result             := AInput
      else
        case FProtocol of
          ppRawI8: Result  := Int8(AInput);
          ppRawI16: Result := Int16(AInput);
          ppRawI24: Result := Int24(AInput);
          ppRawI32: Result := Int32(AInput);
          else
            Result         := NaN;
          end;
    end;

  function GetCaption: String;
    begin
      if FView <> pv2D then
        Result := FRawLabel
      else
      if FCurrentLine = 0 then
        Result := FRawLabel + ' (X)'
      else
        Result := FRawLabel + ' (Y)';
    end;

  begin
    if FInput.Length < ASize * FRawLines then Exit(False);

    while FCurrentLine < FRawLines do
      begin
      AddValue(GetCaption, ConvertToDouble(GetNumber));
      Inc(FCurrentLine);
      FInput := FInput.Remove(0, ASize);
      end;

    Result := True;
  end;

function TPlotterParser.ParseArduino: Boolean;

  function FindNextEol: Integer;
    var
      _length, i: Integer;
      _found:     Boolean;
    begin
      _length := FInput.Length;
      _found  := False;
      Result  := -1;

      if 1 < _length then
        for i := 1 to _length do
          if ((FInput[i] = #10) or (FInput[i] = #13)) xor _found then
            begin
            if _found then Exit(i - 1);
            _found := True;
            Result := i;
            end;
    end;

  function FindArduinoStringBegin: Boolean;
    var
      _startPos: Integer;
    begin
      Result := True;

      if FAfterReset then
        begin
        _startPos   := FindNextEol;
        Result      := _startPos > 0;
        FAfterReset := not Result;

        if Result then
          FInput := FInput.Remove(0, _startPos);
        end;
    end;

  function GetArduinoString: Boolean;
    var
      _endPos: Integer;
    begin
      Result  := True;
      FInput  := FInput.TrimLeft([#10, #13]);
      _endPos := FindNextEol;

      if _endPos <= 0 then Exit(False);

      FRE.InputString := UnicodeString(FInput.Substring(0, _endPos - 1)) + ' ';
      FInput          := FInput.Remove(0, _endPos);
    end;

  procedure ApplyAdditionalParameters(AParams: String);
    var
      params: TStringArray;
      p:      String;
    begin
      params := AParams.Split('|'); // parameters are splitted by vertical bars
      for p in params do
        if Length(p) > 1 then
            try
            case p[1] of

              // color
              'c': FCharts[FCurrentLine].Color := RGBHexToColor(p.Remove(0, 1));

              // width
              'w': FCharts[FCurrentLine].Width := StrToInt(p.Remove(0, 1));

              // style
              's': FCharts[FCurrentLine].Style := TPlotterPenStyle(StrToInt(p.Remove(0, 1)));

              // points DataSize
              'p': FCharts[FCurrentLine].PointSize := StrToInt(p.Remove(0, 1));

              end;
            except
            end;
    end;

    // search for values in data string and add them to list
  function GetArduinoValues: Boolean;
    var
      Value: Double;
      isHex: Boolean = False;
    begin
      with FRE do
        if Exec then
          repeat

            // get value
              try
              case String(Match[4]) of

                // hexadecimal value
                '0x', '$', 'h', 'x':
                  begin
                  Value := StrToInt64('$' + String(Match[5]));
                  isHex := True;
                  end;

                // decimal value
                'd':
                  Value := StrToInt64(String(Match[5]));

                // octal value
                'o':
                  Value := StrToInt64('&' + String(Match[5]));

                // binary value
                'b':
                  Value := StrToInt64('%' + String(Match[5]));

                  // otherwise we suppose floating-point value
                else
                  Value := StrToFloat(String(Match[6]), FFormatSettings);
                end;
              except
              Value := NaN;
              end;

            if Match[1] = '' then                       // see what we received:
              begin                                     // - standard value
              AddValue(String(Match[2]), Value);

              // check for line additional parameters
              if String(Match[3]) <> '' then
                ApplyAdditionalParameters(String(Match[3]).ToLower);

              Inc(FCurrentLine);
              end
            else
            if FOnCommand <> nil then                   // - command
              FOnCommand(String(Match[2]).ToLower, Value,
                isHex.Select(Length(Match[5]), 0));

          until not (ExecNext and (FCurrentLine < FMaxLines));

      Result := FCurrentLine <> 0;
    end;

  begin
    Result := False;

    if FInput = '' then Exit;

    if not FindArduinoStringBegin then Exit;
    if not GetArduinoString then Exit;

    FCharts.AddingStart;
    Result := GetArduinoValues;

    if Result then
      // заполнение Y для отсутствующих линий значением NaN
      FCharts.AddingFinish((FView = pv2D).Select(
        FCharts.Item[0].Points[0].X, FSample));

    // пропуск невалидных строк (не содержащих ни одного значения)
    Result := Result or (FindNextEol > 0);
  end;

function TPlotterParser.ParseRegExp: Boolean;

  // move accumulated data from buffer to out
  procedure BufferOut;
    var
      i: Integer;
    begin
      FChartsBuf.AddingFinish((FView = pv2D).Select(
        FCharts.Item[0].Points[0].X, FSample));

      FCharts.AddingStart;
      for i := 0 to FChartsBuf.ActiveLines - 1 do
        AddValue(FChartsBuf[i].Caption, FChartsBuf[i].Points[0].Y, i);

      FChartsBuf.ClearAll;
    end;

    // get values from input string
  function GetRegexpValues: Boolean;
    var
      valStr, labStr:     String;
      Value:              Double;
      offset, offsetLast: LongInt;
    begin
      Result          := False;
      offsetLast      := 0;
      FRE.InputString := FInput;

      if FRE.Exec then
        repeat
          offset := FRE.MatchPos[0] + FRE.MatchLen[0] - 1;
          if offset = FInput.Length then break;
          offsetLast := offset;

          labStr := String(FRE.Substitute(FRELabel));
          valStr := String(FRE.Substitute(FREValue));

          // get value
            try
            if valStr[1] in ['$', '%', '&'] then
              Value := StrToInt64(valStr)
            else
              Value := StrToFloat(valStr, FFormatSettings);
            except
            Value := NaN;
            end;

          if FAfterResetRE then
            begin
            FREBegin      := labStr;
            FAfterResetRE := False;
            end
          else
          if FREBegin = labStr then
            begin
            Result := True;
            BufferOut;
            end;

          FChartsBuf.Add(labStr, 0, Value);

        until Result or not (FRE.ExecNext and (FChartsBuf.ActiveLines < FMaxLines));

      if offsetLast > 0 then
        begin
        FInput          := FInput.Remove(0, offsetLast);
        FRE.InputString := FInput;
        end;
    end;

  begin
    Result := False;

    if FInput <> '' then
      Result := GetRegexpValues;
  end;


procedure TPlotterParser.SetInput(AValue: String);
  begin
    if FPause or FSettingsFailed then Exit;
    if (FView = pvSweep) and (FWindowSize <= 0) then Exit;
    if FOnParseDone = nil then Exit;

    FInput    := FInput + AValue;
    FDataSize += AValue.Length;
    Parse;
  end;

procedure TPlotterParser.SetProtocol(AValue: TPlotterProtocol);
  begin
    if FProtocol = AValue then Exit;
    if not (FProtocol in [Low(TPlotterProtocol)..High(TPlotterProtocol)]) then Exit;
    FProtocol := AValue;
    Reset;
  end;

procedure TPlotterParser.SetView(AValue: TPlotterView);
  begin
    if FView = AValue then Exit;
    FView := AValue;
    Reset;
  end;

procedure TPlotterParser.SetPause(AValue: Boolean);
  begin
    if FPause = AValue then Exit;
    FPause := AValue;

    if FPause then
      begin
      FAfterReset := True;

      // Clear input buffer to prevent detecting unexpected invalid lines.
      // Example. If FInput = ' li' before pause and new data is 'ine=123 abc'
      // then we will detect new unexpected line with label 'liine'.
      // Although 'li' and 'ine' are parts of 'line'. So we must clear FInput.
      FInput := '';
      end;
  end;


procedure TPlotterParser.WindowPositionRestore;
  begin
    FWindowPos   := FWindowPosBU;
    FWindowReset := FWindowResetBU;
  end;

procedure TPlotterParser.WindowPositionSave;
  begin
    FWindowPosBU   := FWindowPos;
    FWindowResetBU := FWindowReset;
  end;

procedure TPlotterParser.WindowPositionIncrement;
  begin
    FWindowPos += 1;

    if FWindowPos = FWindowSize then
      begin
      FWindowPos   := 0;
      FWindowReset := False;
      end;
  end;

procedure TPlotterParser.DeleteLastSamples(ACount: Integer);
  begin
    FSample     := (FSample > ACount).Select(FSample - ACount, 0);
    FSamplePrev := FSample;
  end;


constructor TPlotterParser.Create(AMaxLines: Integer);
  begin
    FOnParseDone    := nil;
    FOnCommand      := nil;
    FProtocol       := ppArduino;
    FView           := pvStandard;
    FRE             := TRegExpr.Create;
    FTimer          := TTimer.Create(nil);
    FTimer.OnTimer  := @UpdateOnTimer;
    FTimer.Interval := 16; // 60 FPS max
    FTimer.Enabled  := False;
    FWindowSize     := 400;
    FWindowClear    := 20;
    FMaxLines       := AMaxLines;
    FRegExpCaseCare := False;
    FREString       := '(?<=\s)(\w+)=(\d+)(?=\s)'; // some default
    FRELabel        := '$1';
    FREValue        := '$2';

    FFormatSettings := DefaultFormatSettings;
    FFormatSettings.DecimalSeparator := '.';

    Reset;
  end;

destructor TPlotterParser.Destroy;
  begin
    FCharts.Free;
    FRE.Free;
    FTimer.Free;
    FChartsBuf.Free;

    inherited Destroy;
  end;


procedure TPlotterParser.Reset;
  begin
      try
      FInput          := '';
      FRawLabel       := '';
      FREBegin        := '';
      FDataSize       := 0;
      FSample         := 0;
      FSamplePrev     := 0;
      FLinesCount     := 0;
      FInputIndex     := 1;
      FAfterReset     := True;
      FAfterResetRE   := True;
      FWindowReset    := True;
      FWindowPos      := 0;
      FHandlerAllow   := False;
      FHandlerActive  := False;
      FPause          := False;
      FSettingsFailed := False;
      FTimer.Enabled  := FView <> pvSweep;
      WindowPositionSave;

      FCharts.Free;
      FCharts := TPlotterChartsList.Create(FMaxLines);

      FChartsBuf.Free;
      FChartsBuf := TPlotterChartsList.Create(FMaxLines);

      FRE.ModifierM := True; // regex: treat input string as multiple lines

      case FProtocol of

        ppRawU8, ppRawU16, ppRawU24, ppRawU32,
        ppRawI8, ppRawI16, ppRawI24, ppRawI32:
          begin
          FRawLines := (FView = pv2D).Select(2, 1);
          FRawLabel := TXT_PLOTTER_PROTOCOL[FProtocol].
            Replace('uint', 'u').Replace('int', 'i').Replace('Big-Endian', '').Trim;
          //FRawLabel := Format('Raw %s%d', [
          //  (FProtocol >= ppRawI8).Select('i', 'u'),
          //  ((Ord(FProtocol) - Ord(ppRawU8)) mod 4 + 1) * 8]);
          end;

        ppArduino:
          begin
          FRE.ModifierI  := True; // regex: case-insensitive
          FRE.Expression :=       // regex: expression for Arduino format
            '(?:(\|)?([^\s,:|]+)(\|[^\s,: ]*)?[\t ]*:[\t ]*|^|[\t, ])' +
            '(?:(?:(0x|\$|h|x|b|d|o)([\da-f]+))|nan|n|' +
            '([\-+]?(?:\d+\.\d*|\.\d+|\d+)(?:e[\-+]?\d+)?))(?=[,\s$])';
          end;

        ppRegExp:
          begin
          FSettingsFailed := FREValue = '';
          FRE.ModifierI   := not FRegExpCaseCare; // set case sensitivity
          FRE.Expression  := UnicodeString(FREString.Replace(#10, '').Replace(#13, ''));
          FRE.Compile;
          end;
        end;

      except
      FSettingsFailed := True;
      end;
  end;

end.
