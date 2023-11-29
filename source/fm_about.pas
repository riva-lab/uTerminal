unit fm_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LCLType, LCLIntf, BCSVGViewer,
  fr_about, u_utilities, OnlineUpdater, ouVersion;

resourcestring
  ABOUT_VERSION     = 'версия %d.%d.%d';
  ABOUT_BIT         = '%s-битная';
  ABOUT_BUILD       = 'сборка #%d, %s';
  ABOUT_SITE        = 'сайт';
  ABOUT_LICENSE     = 'лицензия';
  ABOUT_INFO        = 'информация';

const
  FILE_LICENSE      = 'license.md';
  FILE_LICENSE_HTML = 'license.html';
  FILE_README       = 'readme.md';
  FILE_README_HTML  = 'readme.html';
  ABOUT_RIGHTS      = 'Modified FreeBSD License';
  ABOUT_OPENSRC     = 'Open Source Freeware';

  // адрес сайта, пример https://example.site/home,
  // пустая строка адреса отключает видимость ссылки
  APP_SITE_ADDRESS  = 'https://riva-lab.gitlab.io/html/apps/uterminal.html';

  // отображаемое имя сайта
  APP_SITE          = 'riva-lab.gitlab.io';

  APP_REPO_ADDRESS  = 'https://gitlab.com/riva-lab/uTerminal';

type

  { TfmAbout }

  TfmAbout = class(TForm)
    frALinks:  TfrAboutLinks;
    lbInfo:    TLabel;
    BCSVGLogo: TBCSVGViewer;

    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure linkClick(Sender: TObject);

  private
    FAppArc:       String;
    FAppAuthor:    String;
    FAppBuild:     String;
    FAppCopyright: String;
    FAppDescr:     String;
    FAppIntName:   String;
    FAppVersion:   String;

  public
    procedure ShowSplash(IsShow: Boolean = True);
    procedure VisitSite;
    procedure UpdateInfo;

    property AppIntName: String read FAppIntName;
    property AppVersion: String read FAppVersion;
    property AppBuild: String read FAppBuild;
    property AppArc: String read FAppArc;
    property AppAuthor: String read FAppAuthor;
    property AppCopyright: String read FAppCopyright;
    property AppDescription: String read FAppDescr;
  end;

var
  fmAbout: TfmAbout;

implementation

{$R *.lfm}

{ TfmAbout }

procedure TfmAbout.FormCreate(Sender: TObject);
  begin
    UpdateInfo;
    BCSVGLogo.Height := Scale96ToScreen(240);
    BCSVGLogo.Width  := Scale96ToScreen(360);
  end;

procedure TfmAbout.FormDeactivate(Sender: TObject);
  begin
    Close;
  end;

procedure TfmAbout.FormShow(Sender: TObject);
  begin
    UpdateInfo;
    BCSVGLogo.SVGString := GetResourceAsString('LOGO');
  end;

procedure TfmAbout.FormUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of
      chr(13), chr(27), chr(32):
        Close;
      end;
  end;


procedure TfmAbout.linkClick(Sender: TObject);

  function TryOpenFile(AFilename: String): Boolean;
    begin
      Result := False;
      if OpenDocument(AFilename) then Exit(True);
      if OpenDocument('..' + DirectorySeparator + AFilename) then Exit(True);
    end;

  procedure GetFromResources(ARes: String; AFilename: String);
    begin
      with TResourceStream.Create(HINSTANCE, ARes, RT_RCDATA) do
          try
          SaveToFile(AFilename);
          finally
          Free;
          OpenDocument(AFilename); // open file
          Sleep(2000);             // wait for 2s while file is opening
          DeleteFile(AFilename);   // delete file
          end;
    end;

  begin
    case TComponent(Sender).Name of
      'lbCopyRights':
        if not TryOpenFile(FILE_README_HTML) then
          if not TryOpenFile(FILE_README) then
            GetFromResources('README', FILE_README);

      'lbLicense':
        if not TryOpenFile(FILE_LICENSE_HTML) then
          if not TryOpenFile(FILE_LICENSE) then
            GetFromResources('LICENSE', FILE_LICENSE);

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
  var
    BuildDateTime: TDateTime;
    _build:        Integer = 0;
  begin
    FAppArc := '';

    {$IfDef WINDOWS}
      {$IfDef WIN64}
      FAppArc := Format(ABOUT_BIT, ['64']);
      {$EndIf}
      {$IfDef WIN32}
      FAppArc := Format(ABOUT_BIT, ['32']);
      {$EndIf}
    {$EndIf}

    with TFileVersionInfoSimple.Create do
        try
        if Assigned(ReadVersionInfo) then
          begin
          FAppIntName   := InternalName;
          FAppAuthor    := CompanyName;
          FAppDescr     := FileDescription;
          FAppCopyright := '© ' + LegalCopyright;
          with  ParseVersion(FileVersion) do
            begin
            FAppVersion := Format(ABOUT_VERSION, [Major, Minor, Revision]);
            _build      := Build;
            end;

          end;
        finally
        Free;
        end;

    TryStrToDate({$INCLUDE %DATE%}, BuildDateTime, 'YYYY/MM/DD', '/');
    FAppBuild := Format(ABOUT_BUILD, [_build, FormatDateTime('yyyy.mm.dd', BuildDateTime)]);

    with TStringList.Create do
        try
        Add(FAppIntName);
        Add(FAppVersion + (FAppArc <> '').Select(', ' + FAppArc, ''));
        Add(FAppBuild);
        Add('');
        Add(FAppDescr);
        Add('');
        Add(FAppCopyright);
        Add(FAppAuthor);
        Add(ABOUT_RIGHTS);
        Add(ABOUT_OPENSRC);
        lbInfo.Caption := Text;
        finally
        Free;
        end;

    frALinks.lbSite.Caption       := APP_SITE;
    frALinks.lbSite.Hint          := APP_SITE_ADDRESS;
    frALinks.lbSite.Visible       := APP_SITE_ADDRESS <> '';
    frALinks.lbLicense.Hint       := FILE_LICENSE;
    frALinks.lbCopyRights.Hint    := FILE_README_HTML;
    frALinks.lbLicense.Caption    := ABOUT_LICENSE;
    frALinks.lbCopyRights.Caption := ABOUT_INFO;
  end;

end.
