unit fm_about;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls,
  StdCtrls, LCLType, LCLIntf, Buttons, app_ver, Classes;

resourcestring
  ABOUT_RIGHTS  = 'Все права защищены';
  ABOUT_VERSION = 'версия';
  ABOUT_BIT     = '-битная';
  ABOUT_BUILD   = 'сборка';
  ABOUT_SITE    = 'сайт';

const

  // адрес сайта, пример https://example.site/home, пустая строка адреса отключает видимость ссылки
  APP_SITE_ADDRESS = 'https://gitlab.com/riva-lab/uTerminal';
  APP_SITE         = 'gitlab.com'; // отображаемое имя сайта

  FILE_LICENSE = 'license.md';
  FILE_README  = 'readme.md';

type

  { TfmAbout }

  TfmAbout = class(TForm)
    Image1:       TImage;
    lbInfo:       TLabel;
    lbSite:       TLabel;
    lbCopyRights: TLabel;
    mLicense:     TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure lbSiteClick(Sender: TObject);
    procedure lbCopyRightsClick(Sender: TObject);
  PRIVATE
    { private declarations }
  PUBLIC
    { public declarations }
    procedure ShowSplash(IsShow: Boolean = True);
  end;

var
  fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.FormCreate(Sender: TObject);
  const
  {$IfDef WIN64}
    sys_arc = '64';
  {$Else}
    sys_arc = '32';
  {$EndIf}
  var
    info:          String;
    BuildDateTime: TDateTime;
  begin
    TryStrToDate({$INCLUDE %DATE%}, BuildDateTime, 'YYYY/MM/DD', '/');

    ReadAppInfo;
    info := app_info.InternalName;
    info += #13#10 + app_info.FileDescription;
    info += #13#10 + ABOUT_VERSION + ' ' + app_info.FileVersion + ', ' + sys_arc + ABOUT_BIT;
    info += #13#10 + ABOUT_BUILD + ' ' + FormatDateTime('yyyy.mm.dd', BuildDateTime);
    info += #13#10;
    info += #13#10 + ABOUT_RIGHTS;
    info += #13#10 + app_info.CompanyName;
    info += #13#10 + '© ' + app_info.LegalCopyright;

    lbInfo.Caption := info;
    lbSite.Caption := {ABOUT_SITE + ': ' +} APP_SITE;
    lbSite.Hint    := APP_SITE_ADDRESS;
  end;

procedure TfmAbout.FormDeactivate(Sender: TObject);
  begin
    Close;
  end;

procedure TfmAbout.FormShow(Sender: TObject);
  begin
    mLicense.Visible := False;
  end;

procedure TfmAbout.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of
      chr(13), chr(27), chr(32):
        Close;
      end;
  end;


procedure TfmAbout.lbSiteClick(Sender: TObject);
  begin
    OpenURL(APP_SITE_ADDRESS);
  end;

procedure TfmAbout.lbCopyRightsClick(Sender: TObject);
  var
    txt: String = '';
  begin
    txt += 'Free Pascal RAD IDE Lazarus' + #13#10;
    txt += 'http://www.lazarus-ide.org' + #13#10;
    txt += #13#10;
    txt += 'Free Pascal Compiler ' +{$INCLUDE %FPCVERSION%} +#13#10;
    txt += 'https://freepascal.org' + #13#10;
    txt += #13#10;
    txt += 'ComPort Library ver. 3.0 for Free Pascal 2.0.4' + #13#10;
    txt += 'http://comport.sf.net/' + #13#10;
    txt += #13#10;
    txt += 'TAChart - a charting LGPL component for Lazarus' + #13#10;
    txt += 'http://wiki.lazarus.freepascal.org/TAChart' + #13#10;
    //txt += '' + #13#10;

    with mLicense do
      begin
      Anchors := [akTop, akLeft, akRight, akBottom];
      Clear;
      Lines.Add(txt);
      Visible := not Visible;
      end;
  end;

procedure TfmAbout.ShowSplash(IsShow: Boolean = True);
  begin
    {$IfNDef Debug}
    Position  := poScreenCenter;
    FormStyle := fsSplash;

    if IsShow then
      begin
      Show;
      Update;  // выводит форму на экран сразу же
      Sleep(2000);
      Close;
      end;
    {$EndIf}

    FormStyle   := fsNormal;
    BorderStyle := bsNone;
    Position    := poMainFormCenter;
  end;

end.
