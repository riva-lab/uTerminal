unit fm_about;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, ExtCtrls, StdCtrls, LCLType, LCLIntf,
  app_ver, u_strings, fr_about, Classes;

resourcestring
  ABOUT_RIGHTS  = 'Все права защищены\nСвободное ПО';
  ABOUT_VERSION = 'версия';
  ABOUT_BIT     = '-битная';
  ABOUT_BUILD   = 'сборка';
  ABOUT_SITE    = 'сайт';
  ABOUT_LICENSE = 'лицензия';
  ABOUT_INFO    = 'информация';
  ABOUT_DESCR   = 'микроТерминал для последовательного порта\n';

const

  // адрес сайта, пример https://example.site/home, пустая строка адреса отключает видимость ссылки
  APP_SITE_ADDRESS = 'https://gitlab.com/riva-lab/uTerminal';
  APP_SITE         = 'gitlab.com'; // отображаемое имя сайта

  FILE_LICENSE      = 'license.md';
  FILE_LICENSE_HTML = 'license.html';
  FILE_README       = 'readme.md';
  FILE_README_HTML  = 'readme.html';

type

  { TfmAbout }

  TfmAbout = class(TForm)
    frALinks: TfrAboutLinks;
    imLogo:   TImage;
    lbInfo:   TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure linkClick(Sender: TObject);

  PRIVATE
    FAppArc:           String;
    FAppAuthor:        String;
    FAppBrief:         String;
    FAppBuild:         String;
    FAppCopyright:     String;
    FAppDescription:   String;
    FAppIntName:       String;
    FAppSite:          String;
    FAppSiteAvailable: Boolean;
    FAppVersion:       String;
    FAppComments:      String;

  PUBLIC
    procedure ShowSplash(IsShow: Boolean = True);
    procedure VisitSite;
    procedure UpdateInfo;

    property AppIntName: String read FAppIntName;
    property AppVersion: String read FAppVersion;
    property AppBuild: String read FAppBuild;
    property AppArc: String read FAppArc;
    property AppAuthor: String read FAppAuthor;
    property AppCopyright: String read FAppCopyright;
    property AppDescription: String read FAppDescription;
    property AppBrief: String read FAppBrief;
    property AppSite: String read FAppSite;
    property AppSiteAvailable: Boolean read FAppSiteAvailable;
  end;

var
  fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.FormCreate(Sender: TObject);
  begin
    UpdateInfo;
    Constraints.MaxWidth  := imLogo.Width;
    Constraints.MaxHeight := imLogo.Height;
  end;

procedure TfmAbout.FormDeactivate(Sender: TObject);
  begin
    Close;
  end;

procedure TfmAbout.FormShow(Sender: TObject);
  begin
    UpdateInfo;
  end;

procedure TfmAbout.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of
      chr(13), chr(27), chr(32):
        Close;
      end;
  end;


procedure TfmAbout.linkClick(Sender: TObject);
  begin
    case TComponent(Sender).Name of
      'lbCopyRights':
        if not OpenDocument(FILE_README) then
          if not OpenDocument('..' + DirectorySeparator + FILE_README_HTML) then
            OpenDocument('..' + DirectorySeparator + FILE_README);

      'lbLicense':
        if not OpenDocument(FILE_LICENSE) then
          if not OpenDocument('..' + DirectorySeparator + FILE_LICENSE_HTML) then
            OpenDocument('..' + DirectorySeparator + FILE_LICENSE);

      'lbSite':
        VisitSite;
      end;
  end;

procedure TfmAbout.ShowSplash(IsShow: Boolean = True);
  var
    i: Integer;
  begin
    {$IfNDef Debug}
    Position       := poScreenCenter;
    FormStyle      := fsStayOnTop;
    OnDeactivate   := nil;
    OnUTF8KeyPress := nil;

    if IsShow then
      begin
      Show;

      for i := 0 to 120 do
        begin
        Sleep(10);
        Application.ProcessMessages;
        end;

      Close;
      end;
    {$EndIf}

    FormStyle      := fsNormal;
    BorderStyle    := bsNone;
    Position       := poMainFormCenter;
    OnDeactivate   := @FormDeactivate;
    OnUTF8KeyPress := @FormUTF8KeyPress;
  end;


procedure TfmAbout.VisitSite;
  begin
    OpenURL(APP_SITE_ADDRESS);
  end;

procedure TfmAbout.UpdateInfo;
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
    FAppIntName       := app_info.InternalName;
    FAppArc           := sys_arc + ABOUT_BIT;
    FAppAuthor        := app_info.CompanyName;
    FAppBuild         := ABOUT_BUILD + ' ' + FormatDateTime('yyyy.mm.dd', BuildDateTime);
    FAppDescription   := app_info.FileDescription;
    FAppVersion       := ABOUT_VERSION + ' ' + app_info.FileVersion;
    FAppCopyright     := '© ' + app_info.LegalCopyright;
    //FAppComments      := app_info.Comments;
    FAppComments      := ABOUT_DESCR;
    FAppBrief         := FAppIntName + ' ' + FAppVersion + ', ' + FAppArc + ', ' + FAppBuild;
    FAppSite          := APP_SITE;
    FAppSiteAvailable := Length(APP_SITE_ADDRESS) > 0;

    info := app_info.InternalName + LineEnding;
    info += FAppVersion + ', ' + FAppArc + LineEnding;
    info += FAppBuild + LineEnding + LineEnding;
    info += MultiString(FAppComments) + LineEnding + LineEnding;
    info += FAppCopyright + LineEnding;
    info += MultiString(ABOUT_RIGHTS) + LineEnding;
    info += FAppAuthor;

    lbInfo.Caption                := info;
    frALinks.lbSite.Caption       := {ABOUT_SITE + ': ' +} APP_SITE;
    frALinks.lbSite.Hint          := APP_SITE_ADDRESS;
    frALinks.lbSite.Visible       := FAppSiteAvailable;
    frALinks.lbLicense.Hint       := FILE_LICENSE;
    frALinks.lbCopyRights.Hint    := FILE_README;
    frALinks.lbLicense.Caption    := ABOUT_LICENSE;
    frALinks.lbCopyRights.Caption := ABOUT_INFO;
  end;

end.
