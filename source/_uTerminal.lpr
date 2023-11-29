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

  // for themes handling                    
  {$IFDEF ALLOW_DARK_THEME}
  uDarkStyleParams, uMetaDarkStyle, uDarkStyleSchemes,
  {$ENDIF}

  Forms, SysUtils, IniPropStorage, LazUTF8, OnlineUpdater,

  // all application forms
  fm_main, fm_commands, fm_settings, fm_insertchar, fm_about, fm_confirm, 
  fm_update, u_settings_record;


  procedure DarkThemeSupport;
    begin
    {$IFDEF ALLOW_DARK_THEME}

      // dark theme works only on windows 1809 (build 17763) and higher
      {$IFDEF WINDOWS}
      if Win32BuildNumber > 17763 then
      {$ENDIF}

        // read theme value from app .ini settings file
        with TIniPropStorage.Create(nil) do
          begin
          IniFileName   := fm_settings.GetAppIniFileName;
          Active        := True;
          IniSection    := 'TSettings_fmSettings'; // section needs to be correct!

          // theme mode
          case ReadInteger('cbAppTheme', 0) of
            1: PreferredAppMode := pamForceLight;
            2: PreferredAppMode := pamForceDark;
            else
              PreferredAppMode := pamAllowDark;
            end;

          // apply theme
          uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);

          // allow showing theme selector in settings window
          fm_settings.appThemeAvailable := True;

          Free; // TIniPropStorage object
          end;

    {$ENDIF}
    end;


{$R *.res}

begin           
  if fm_update.IsUpdaterReplaceActivated then Exit;

  RequireDerivedFormResource := True; 
  Application.Scaled := True;

  DarkThemeSupport;

  // create forms and run app gui
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmCommands, fmCommands);
  Application.CreateForm(TfmASCIIChar, fmASCIIChar);
  Application.CreateForm(TfmAbout, fmAbout);
  Application.CreateForm(TfmSettings, fmSettings);
  Application.CreateForm(TfmConfirm, fmConfirm);
  Application.CreateForm(TfmUpdate, fmUpdate);
  Application.Run;
end.
