program _uTerminal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
   {$IFDEF UseCThreads}
    cthreads,
   {$ENDIF}
  {$ENDIF}

  // this includes the LCL widgetset
  Interfaces,

  Forms, SysUtils, LazUTF8, OnlineUpdater, AppTuner,

  // all application forms
  fm_main, fm_commands, fm_settings, fm_insertchar, fm_about, fm_confirm, 
  fm_update, u_settings_record;


{$R *.res}

begin
  { CRITICAL! Load INI file as soon as possible to support dark theme.
    INI file should be loaded before Application.Initialize method! }
  appTunerEx.IniFile := ExtractFilePath(ParamStrUTF8(0)) + SETTINGS_FILE;

  if fm_update.IsUpdaterReplaceActivated then Exit;

  RequireDerivedFormResource := True; 
  Application.Scaled := True;

  // create forms and run app gui
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmCommands, fmCommands);
  Application.CreateForm(TfmASCIIChar, fmASCIIChar);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmConfirm, fmConfirm);
  Application.CreateForm(TfmUpdate, fmUpdate);
  Application.Run;
end.
