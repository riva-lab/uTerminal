unit fr_about;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, StdCtrls, LCLType, Graphics;

type

  { TfrAboutLinks }

  TfrAboutLinks = class(TFrame)
    lbLicense:    TLabel;
    lbCopyRights: TLabel;
    lbSite:       TLabel;

    procedure CreateParams(var Params: TCreateParams); OVERRIDE;
  PRIVATE
    { private declarations }
  PUBLIC
    { public declarations }
    constructor Create(AOwner: TComponent); OVERRIDE;
  end;

implementation

{$R *.lfm}

 { TfrAboutLinks }
 // https://andrey.mikhalchuk.com/2008/10/04/how-to-make-delphi-tframe-background-transparent.html

constructor TfrAboutLinks.Create(AOwner: TComponent);
  begin
    inherited;
    Brush.Style := bsClear;
  end;

procedure TfrAboutLinks.CreateParams(var Params: TCreateParams);
  begin
    inherited;
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
  end;

end.

