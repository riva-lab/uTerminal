program uTerminal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, CPortLib10, tachartlazaruspkg, fm_main, com_detect, fm_commands,
  fm_insertchar, fm_about, app_ver, fm_graph
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmCommands, fmCommands);
  Application.CreateForm(TfmASCIIChar, fmASCIIChar);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmGraph, fmGraph);
  Application.Run;
end.

