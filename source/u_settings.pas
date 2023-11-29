unit u_settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Spin, StdCtrls, Forms, IniPropStorage, Dialogs, Graphics,
  u_utilities;

type

  {
    TSettings
    ---------
    Класс для обмена значениями между
    свойством компонента и переменной по указателю.
    Используется для упрощения работы с массивом настроек.
    Также сохраняет настройки в заданный ini-файл.
    Обновляет строки в TComboBox (для динамического перевода).
    Обновляет ширину строк в TComboBox.
  }

  TSettings = class
  private

    FCount:      Integer;
    FForm:       TForm;
    FAComp:      array of TComponent;
    FAField:     array of Pointer;
    FIIBackup:   array of Integer;
    FAMult:      array of Integer;
    FCBList:     array of TStringArray;
    FIniStorage: TIniPropStorage;
    FFS:         TFormatSettings;

    function Find(AComponent: TComponent): Integer;
    function Find(AField: Pointer): Integer;

  public

    constructor Create;
    constructor Create(AForm: TForm; AIniFileName: String);
    destructor Destroy; override;

    procedure Add(AComponent: TComponent; AField: Pointer = nil);
    procedure Add(AComponent: TComponent; AField: Pointer; AMultiplier: Integer);
    procedure Add(AComponent: TComponent; AField: Pointer; AComboList: TStringArray);
    procedure Add(AComponent: TComponent; AField: Pointer; AMultiplier: Integer; AComboList: TStringArray);

    procedure SaveToIniStorage;
    procedure LoadFromIniStorage;
    procedure LoadFields;
    procedure LoadCompValues;
    procedure ItemIndexBackup;
    procedure ItemIndexRestore;
    procedure AdjustComboItemWidth;
    procedure UpdateComboboxList;
  end;

implementation

{ TSettings }

constructor TSettings.Create;
  var
    i: Integer;
  begin
    for i := 0 to High(FAComp) do
      FreeAndNil(FCBList[i]);

    FreeAndNil(FAComp);
    FreeAndNil(FAField);
    FreeAndNil(FIIBackup);
    FreeAndNil(FCBList);
    FreeAndNil(FAMult);

    FCount := 0;
    FForm  := nil;
    FIniStorage.Free;

    FFS := DefaultFormatSettings;
    FFS.DecimalSeparator := '.';
  end;

constructor TSettings.Create(AForm: TForm; AIniFileName: String);
  begin
    Create;
    FForm := AForm;

    FIniStorage             := TIniPropStorage.Create(nil);
    FIniStorage.IniFileName := AIniFileName;
  end;

destructor TSettings.Destroy;
  begin
    Create;

    inherited Destroy;
  end;


function TSettings.Find(AComponent: TComponent): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i  := 0 to FCount - 1 do
      if FAComp[i].Name = AComponent.Name then Exit(i);
  end;

function TSettings.Find(AField: Pointer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    if AField = nil then Exit;
    for i := 0 to FCount - 1 do
      if FAField[i] = AField then Exit(i);
  end;


procedure TSettings.Add(AComponent: TComponent; AField: Pointer);
  begin
    Add(AComponent, AField, 1, []);
  end;

procedure TSettings.Add(AComponent: TComponent; AField: Pointer;
  AMultiplier: Integer);
  begin
    Add(AComponent, AField, AMultiplier, []);
  end;

procedure TSettings.Add(AComponent: TComponent; AField: Pointer;
  AComboList: TStringArray);
  begin
    Add(AComponent, AField, 1, AComboList);
  end;

procedure TSettings.Add(AComponent: TComponent; AField: Pointer;
  AMultiplier: Integer; AComboList: TStringArray);
  var
    lastIndex, i: Integer;
  begin
    if Find(AComponent) >= 0 then Exit;
    if Assigned(AField) and (Find(AField) >= 0) then Exit;

    lastIndex := FCount;
    FCount    += 1;
    SetLength(FAComp, FCount);
    SetLength(FAField, FCount);
    SetLength(FIIBackup, FCount);
    SetLength(FCBList, FCount);
    SetLength(FAMult, FCount);

    FAComp[lastIndex]  := AComponent;
    FAField[lastIndex] := AField;
    FAMult[lastIndex]  := AMultiplier;

    if Length(AComboList) > 0 then
      begin
      SetLength(FCBList[lastIndex], Length(AComboList));
      for i := Low(AComboList) to High(AComboList) do
        FCBList[lastIndex][i - Low(AComboList)] := AComboList[i];
      end;
  end;


procedure TSettings.SaveToIniStorage;
  var
    i:  Integer;
    _c: TComponent;
    _n: TComponentName;
  begin
    if FIniStorage = nil then Exit;

    with FIniStorage do
      begin
      Active     := True;
      IniSection := 'TSettings_' + FForm.Name;
      EraseSections;

      WriteInteger('TSettings_Count', FCount);

      for i := 0 to FCount - 1 do
        begin
        _c := FAComp[i];
        _n := _c.Name;
        case _c.ClassName of
          'TSpinEdit': WriteInteger(_n, TSpinEdit(_c).Value);
          'TFloatSpinEdit': WriteString(_n, TFloatSpinEdit(_c).Value.ToString(FFS));
          'TComboBox': WriteInteger(_n, TComboBox(_c).ItemIndex);
          'TCheckBox': WriteBoolean(_n, TCheckBox(_c).Checked);
          'TEdit': WriteString(_n, TEdit(_c).Text);
          'TColorButton': WriteInteger(_n, Integer(TColorButton(_c).ButtonColor));
          'TRadioButton': WriteBoolean(_n, TRadioButton(_c).Checked);
          end;
        end;

      IniSection := '';
      end;
  end;

procedure TSettings.LoadFromIniStorage;
  var
    i:  Integer;
    _c: TComponent;
    _n: TComponentName;
  begin
    if FIniStorage = nil then Exit;
    if not FileExists(FIniStorage.IniFileName) then Exit;

    with FIniStorage do
      begin
      Active     := True;
      IniSection := 'TSettings_' + FForm.Name;

      if ReadInteger('TSettings_Count', 0) <= 0 then Exit;

      for i := 0 to FCount - 1 do
        begin
        _c := FAComp[i];
        _n := _c.Name;
        case _c.ClassName of
          'TSpinEdit': TSpinEdit(_c).Value := ReadInteger(_n, 0);
          'TFloatSpinEdit': TFloatSpinEdit(_c).Value := StrToFloat(ReadString(_n, '0'), FFS);
          'TComboBox': TComboBox(_c).ItemIndex := ReadInteger(_n, 0);
          'TCheckBox': TCheckBox(_c).Checked := ReadBoolean(_n, False);
          'TEdit': TEdit(_c).Text := ReadString(_n, '');
          'TColorButton': TColorButton(_c).ButtonColor := TColor(ReadInteger(_n, 0));
          'TRadioButton': TRadioButton(_c).Checked := ReadBoolean(_n, False);
          end;
        end;

      IniSection := '';
      end;
  end;


procedure TSettings.LoadFields;
  var
    i:  Integer;
    _c: TComponent;
    _f: Pointer;
  begin
    for i := 0 to FCount - 1 do
      if FAField[i] <> nil then
        begin
        _c := FAComp[i];
        _f := FAField[i];
        case _c.ClassName of
          'TSpinEdit': (PInteger(_f))^ := TSpinEdit(_c).Value * FAMult[i];
          'TFloatSpinEdit': (PDouble(_f))^ := TFloatSpinEdit(_c).Value * FAMult[i];
          'TComboBox': (PInteger(_f))^     := TComboBox(_c).ItemIndex;
          'TCheckBox': (PBoolean(_f))^     := TCheckBox(_c).Checked;
          'TEdit': (PString(_f))^          := TEdit(_c).Text;
          'TColorButton': (PColor(_f))^    := TColorButton(_c).ButtonColor;
          'TRadioButton': (PBoolean(_f))^  := TRadioButton(_c).Checked;
          end;
        end;
  end;

procedure TSettings.LoadCompValues;
  var
    i:  Integer;
    _c: TComponent;
    _f: Pointer;
  begin
    for i := 0 to FCount - 1 do
      if FAField[i] <> nil then
        begin
        _c := FAComp[i];
        _f := FAField[i];
        case _c.ClassName of
          'TSpinEdit': TSpinEdit(_c).Value := (PInteger(_f))^ div FAMult[i];
          'TFloatSpinEdit': TFloatSpinEdit(_c).Value := (PDouble(_f))^ / FAMult[i];
          'TComboBox': TComboBox(_c).ItemIndex := (PInteger(_f))^;
          'TCheckBox': TCheckBox(_c).Checked := (PBoolean(_f))^;
          'TEdit': TEdit(_c).Text := (PString(_f))^;
          'TColorButton': TColorButton(_c).ButtonColor := (PColor(_f))^;
          'TRadioButton': TRadioButton(_c).Checked := (PBoolean(_f))^;
          end;
        end;
  end;


procedure TSettings.ItemIndexBackup;
  var
    i: Integer;
  begin
    for i := 0 to FCount - 1 do
      if FAComp[i].ClassName = 'TComboBox' then
        FIIBackup[i] := TComboBox(FAComp[i]).ItemIndex;
  end;

procedure TSettings.ItemIndexRestore;
  var
    i: Integer;
  begin
    for i := 0 to FCount - 1 do
      if FAComp[i].ClassName = 'TComboBox' then
        TComboBox(FAComp[i]).ItemIndex := FIIBackup[i];
  end;


procedure TSettings.AdjustComboItemWidth;
  var
    i:  Integer;
    cb: TComboBox;
  begin
    if FForm = nil then Exit;
    for i := 0 to FCount - 1 do
      if FAComp[i].ClassName = 'TComboBox' then
        begin
        cb           := TComboBox(FAComp[i]);
        cb.ItemWidth := GetListStringsMaxWidth(FForm, cb.Items);
        end;
  end;

procedure TSettings.UpdateComboboxList;
  var
    i: Integer;
  begin
    if FForm = nil then Exit;
    for i := 0 to FCount - 1 do
      if FAComp[i].ClassName = 'TComboBox' then
        if Length(FCBList[i]) > 0 then
          ComboBoxUpdateList(TComboBox(FAComp[i]), FCBList[i]);
  end;

end.
