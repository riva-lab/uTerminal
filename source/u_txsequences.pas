unit u_txsequences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, base64, IniPropStorage, LazUTF8, u_encodings, u_utilities;

type

  { TSequencesList }

  TSequencesList = class
  PRIVATE
    FData, FCaption: TStringList;
    FCount:          Integer;
    FNumSys:         Integer;
    FEncoding:       String;

    function IsIndexCorrect(Index: Integer): Boolean;

    function GetCaption(Index: Integer): String;
    function GetData(Index: Integer): String;
    function GetStrings(Index: Integer): String;

    procedure SetCaption(Index: Integer; AValue: String);
    procedure SetData(Index: Integer; AValue: String);
    procedure SetStrings(Index: Integer; AValue: String);

  PUBLIC
    constructor Create;
    destructor Destroy; OVERRIDE;

    procedure LoadFromIni(ARes: TIniPropStorage);
    procedure SaveToIni(ARes: TIniPropStorage);

    function Add(const AStr: String): Integer;
    function AddData(const AData: String): Integer;
    procedure Delete(AIndex: Integer);
    function HasCaption(AIndex: Integer): Boolean;
    function GetCell(AIndex: Integer): String;
    function GetHint(AIndex: Integer): String;
    function Move(AIndex: Integer; ADirUp: Boolean): Boolean;

    property Strings[Index: Integer]: String read GetStrings write SetStrings;
    property Data[Index: Integer]: String read GetData write SetData;
    property Caption[Index: Integer]: String read GetCaption write SetCaption;
    property Count: Integer read FCount;
    property Encoding: String read FEncoding write FEncoding;
    property NumSys: Integer read FNumSys write FNumSys;
  end;


implementation

{ TSequencesList }


function TSequencesList.IsIndexCorrect(Index: Integer): Boolean;
  begin
    Result := (Index >= 0) and (Index < FCount);
  end;

function TSequencesList.GetStrings(Index: Integer): String;
  begin
    if not IsIndexCorrect(Index) then Exit('');
    Result := EncodingToUTF8(FData.Strings[Index], FEncoding);
  end;

procedure TSequencesList.SetStrings(Index: Integer; AValue: String);
  begin
    if not IsIndexCorrect(Index) then Exit;
    FData.Strings[Index] := UTF8ToEncoding(AValue, FEncoding);
  end;

function TSequencesList.GetCaption(Index: Integer): String;
  begin
    if not IsIndexCorrect(Index) then Exit('');
    Result := FCaption.Strings[Index];
  end;

procedure TSequencesList.SetCaption(Index: Integer; AValue: String);
  begin
    if not IsIndexCorrect(Index) then Exit;
    FCaption.Strings[Index] := UTF8LeftStr(AValue, 16);
  end;

function TSequencesList.GetData(Index: Integer): String;
  begin
    if not IsIndexCorrect(Index) then Exit('');
    Result := FData.Strings[Index];
  end;

procedure TSequencesList.SetData(Index: Integer; AValue: String);
  begin
    if not IsIndexCorrect(Index) then Exit;
    FData.Strings[Index] := AValue;
  end;

constructor TSequencesList.Create;
  begin
    FData    := TStringList.Create;
    FCaption := TStringList.Create;
    FCount   := FData.Count;
  end;

destructor TSequencesList.Destroy;
  begin
    FreeAndNil(FData);
    FreeAndNil(FCaption);
    inherited Destroy;
  end;

procedure TSequencesList.LoadFromIni(ARes: TIniPropStorage);
  begin
    if ARes = nil then Exit;

    with ARes do
      begin
      IniSection := 'TX Sequences List';

      if ReadInteger('TXSCount', 0) = 0 then Exit;

        try
        FData.CommaText    := DecodeStringBase64(ReadString('TXSData', ''));
        FCaption.CommaText := DecodeStringBase64(ReadString('TXSName', ''));
        except
        FData.Clear;
        FCaption.Clear;
        end;

      IniSection := ''; // выход из текущей секции
      end;

    FCount := FData.Count;
  end;

procedure TSequencesList.SaveToIni(ARes: TIniPropStorage);
  begin
    if ARes = nil then Exit;
    if FCount = 0 then Exit;

    with ARes do
      begin
      IniSection := 'TX Sequences List';

      WriteInteger('TXSCount', FCount);
      WriteString('TXSData', EncodeStringBase64(FData.CommaText));
      WriteString('TXSName', EncodeStringBase64(FCaption.CommaText));

      IniSection := ''; // выход из текущей секции
      end;
  end;

procedure TSequencesList.Delete(AIndex: Integer);
  begin
    if not IsIndexCorrect(AIndex) then Exit;
    FData.Delete(AIndex);
    FCaption.Delete(AIndex);
    FCount := FData.Count;
  end;

function TSequencesList.Add(const AStr: String): Integer;
  begin
    if AStr = '' then Exit(-1);
    Result := AddData(UTF8ToEncoding(AStr, FEncoding));
  end;

function TSequencesList.AddData(const AData: String): Integer;
  begin
    if AData = '' then Exit(-1);
    FData.Add(AData);
    Result := FCaption.Add('');
    FCount := FData.Count;
  end;

function TSequencesList.HasCaption(AIndex: Integer): Boolean;
  begin
    if not IsIndexCorrect(AIndex) then Exit(False);
    Result := FCaption.Strings[AIndex].Length > 0;
  end;

function TSequencesList.GetCell(AIndex: Integer): String;
  begin
    if not IsIndexCorrect(AIndex) then Exit('');

    if FCaption.Strings[AIndex].Length > 0 then
      Result := FCaption.Strings[AIndex] else
      if FNumSys = 0 then
        Result := EncodingToUTF8(FData.Strings[AIndex], FEncoding) else
        Result := EncodingToUTF8(StrToCodes(FData[AIndex], FNumSys, 80), FEncoding);
  end;

function TSequencesList.GetHint(AIndex: Integer): String;

  function HorzLine(ASize: Integer): String;
    var
      min: Integer;
    begin
      if ASize < 80 then min := ASize else min := 80;
      Result := LineEnding + StringOfChar('_', min) + LineEnding;
    end;

  var
    hline: String;

  begin
    if not IsIndexCorrect(AIndex) then Exit('');

    if FNumSys = 0 then
      Result := EncodingToUTF8(FData.Strings[AIndex], FEncoding) else
      Result := EncodingToUTF8(StrToCodes(FData[AIndex], FNumSys, 80), FEncoding);

    hline := HorzLine(Result.Length);
    if FCaption.Strings[AIndex].Length > 0 then
      Result := FCaption.Strings[AIndex] + ':' + hline + LineEnding + Result;

    Result += hline + SizeStr(FData[AIndex]);
  end;

function TSequencesList.Move(AIndex: Integer; ADirUp: Boolean): Boolean;
  begin
    Result := True;
    if not IsIndexCorrect(AIndex) then Exit(False);
    if ADirUp and (AIndex < 1) then Exit(False);
    if not ADirUp and (AIndex >= FCount - 1) then Exit(False);

    FData.Move(AIndex, AIndex + CheckBoolean(ADirUp, -1, 1));
    FCaption.Move(AIndex, AIndex + CheckBoolean(ADirUp, -1, 1));
  end;

end.

