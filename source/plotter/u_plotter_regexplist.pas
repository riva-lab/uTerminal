unit u_plotter_regexplist;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage, LazFileUtils, uregexpr, base64,
  u_utilities;

type

  { TPlotterRegExpItem }

  TPlotterRegExpItem = class
  private
    FName:     String;
    FRegExp:   String;
    FRELabel:  String;
    FREValue:  String;
    FErrorMsg: String;
    FError:    Boolean;
    FRE:       TRegExpr;

  private
    procedure SetRegExp(AValue: String);

  public
    constructor Create(AName: String = '');
    destructor Destroy; override;

    function Save: String;
    function Load(AText: String): Boolean;

    property Name: String read FName write FName;
    property RegExp: String read FRegExp write SetRegExp;
    property RELabel: String read FRELabel write FRELabel;
    property REValue: String read FREValue write FREValue;
    property Error: Boolean read FError write FError;
    property ErrorMsg: String read FErrorMsg write FErrorMsg;
  end;


  { TPlotterRegExpList }

  TPlotterRegExpList = class(TFPList)
  private
    FIniStorage:  TIniPropStorage;
    FIniFileName: String;
    FID:          String;
    FErrorMsg:    String;

  private
    function GetItems(Index: Integer): TPlotterRegExpItem;

  public
    constructor Create(AIniFileName: String = ''; ID: String = '1');
    destructor Destroy; override;

    procedure LoadFromIni;
    procedure SaveToIni;

    function Add(AName, AREStr, ARELabel, AREValue: String): Integer;
    function Delete(AName: String): Integer;
    function IndexOf(AName: String): Integer;
    function CommaText: String;

    property IniFileName: String read FIniFileName write FIniFileName;
    property Items[Index: Integer]: TPlotterRegExpItem read GetItems;
    property ErrorMsg: String read FErrorMsg;
  end;


implementation


{ TPlotterRegExpItem }

procedure TPlotterRegExpItem.SetRegExp(AValue: String);
  begin
    if FRegExp = AValue then Exit;
    FRegExp := AValue;
    FError  := False;

    // verify regexp
    with FRE do
        try
        Expression := FRegExp.Replace(#10, '').Replace(#13, '');
        Compile;
        except
        FError     := True;
        FErrorMsg  := Format('%d: %s', [
          Integer((CompilerErrorPos > 1000).Select(0, CompilerErrorPos)), ErrorMsg(LastError)]);
        end;
  end;

constructor TPlotterRegExpItem.Create(AName: String);
  begin
    FRE       := TRegExpr.Create;
    FName     := AName;
    FRegExp   := '';
    FRELabel  := '';
    FREValue  := '';
    FErrorMsg := '';
    FError    := False;
  end;

destructor TPlotterRegExpItem.Destroy;
  begin
    FRE.Free;
    inherited Destroy;
  end;

function TPlotterRegExpItem.Save: String;
  begin
    Result := '';
    with TStringList.Create do
      begin
      LineBreak := ' ';
      Add(EncodeStringBase64(FName));
      Add(EncodeStringBase64(FRegExp));
      Add(EncodeStringBase64(FRELabel));
      Add(EncodeStringBase64(FREValue));
      Result := Text;
      Free;
      end;
  end;

function TPlotterRegExpItem.Load(AText: String): Boolean;
  begin
    Result := False;
    with TStringList.Create do
      begin
      LineBreak := ' ';
      Text      := AText;
      Result    := Count <> 0;
      if Result then
          try
          FName    := DecodeStringBase64(Strings[0]);
          FRegExp  := DecodeStringBase64(Strings[1]);
          FRELabel := DecodeStringBase64(Strings[2]);
          FREValue := DecodeStringBase64(Strings[3]);
          except
          end;
      Free;
      end;
  end;


{ TPlotterRegExpList }

function TPlotterRegExpList.GetItems(Index: Integer): TPlotterRegExpItem;
  begin
    if Index < Count then
      Result := TPlotterRegExpItem(inherited Items[Index])
    else
      Result := nil;
  end;

constructor TPlotterRegExpList.Create(AIniFileName: String; ID: String);
  begin
    FIniStorage  := TIniPropStorage.Create(nil);
    FIniFileName := AIniFileName;
    FID          := ID;
    FErrorMsg    := '';
  end;

destructor TPlotterRegExpList.Destroy;
  var
    i: Integer;
  begin
    i := 0;
    while i < Count do Items[i.PostInc].Free;
    FIniStorage.Free;
    inherited Destroy;
  end;

procedure TPlotterRegExpList.LoadFromIni;
  var
    cnt, i: Integer;
  begin
    if FileExistsUTF8(FIniFileName) then
      with FIniStorage do
        begin
        IniFileName := FIniFileName;
        Active      := True;
        IniSection  := Self.ClassName + '_' + FID;

        Clear;
        cnt := ReadInteger('Count', 0);
        if cnt > 0 then
          for i := 0 to cnt - 1 do
            with TPlotterRegExpItem.Create do
              begin
              if Load(ReadString('item-' + i.ToString, '')) then
                Add(Name, RegExp, RELabel, REValue);
              Free;
              end;
        end;
  end;

procedure TPlotterRegExpList.SaveToIni;
  var
    i: Integer;
  begin
    if FileExistsUTF8(FIniFileName) then
      with FIniStorage do
        begin
        IniFileName := FIniFileName;
        Active      := True;
        IniSection  := Self.ClassName + '_' + FID;
        EraseSections;

        WriteInteger('Count', Count);
        if Count > 0 then
          for i := 0 to Count - 1 do
            WriteString('item-' + i.ToString, Items[i].Save);

        Active := False;
        end;
  end;

function TPlotterRegExpList.Add(AName, AREStr, ARELabel, AREValue: String): Integer;
  var
    item: TPlotterRegExpItem;
  begin
    Result := -1;
    if AName = '' then Exit;

    item         := TPlotterRegExpItem.Create(AName);
    item.RegExp  := AREStr;
    item.RELabel := ARELabel;
    item.REValue := AREValue;

    if item.Error then
      begin
      FErrorMsg := item.ErrorMsg;
      item.Free;
      Exit;
      end;

    Delete(AName);
    Result := inherited Add(item);
  end;

function TPlotterRegExpList.Delete(AName: String): Integer;
  begin
    Result := IndexOf(AName);
    if Result < 0 then Exit;
    Items[Result].Free;
    Remove(Items[Result]);
  end;

function TPlotterRegExpList.IndexOf(AName: String): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if Count > 0 then
      for i := 0 to Count - 1 do
        if Items[i].Name = AName then
          Exit(i);
  end;

function TPlotterRegExpList.CommaText: String;
  var
    i: Integer;
  begin
    Result := '';
    with TStringList.Create do
      begin
      if Self.Count > 0 then
        for i := 0 to Self.Count - 1 do
          Add(Items[i].Name);

      Result := CommaText;
      Free;
      end;
  end;


end.
