program uTerminal;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols, fm_main, fm_commands, fm_settings,
  fm_insertchar, fm_about, app_ver, fm_chart, u_strings, fm_confirm,
  u_utilities, fr_about, u_txsequences
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmCommands, fmCommands);
  Application.CreateForm(TfmASCIIChar, fmASCIIChar);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmChart, fmChart);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmConfirm, fmConfirm);
  Application.Run;
end.

