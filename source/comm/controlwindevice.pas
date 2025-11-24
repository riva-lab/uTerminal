unit controlwindevice;

{ Enable Disable windows devices

  Copyright (c) 2010-2012 Ludo Brands

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}


{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, dynlibs, Windows;

const
  GUID_DEVCLASS_NET: TGUID  = '{4D36E972-E325-11CE-BFC1-08002BE10318}';
  GUID_DEVCLASS_PORT: TGUID = '{4D36E978-E325-11CE-BFC1-08002BE10318}';

type
  TDeviceControlResult = (
    DCROK, DCRErrEnumDeviceInfo, DCRErrSetClassInstallParams, DCRErrDIF_PROPERTYCHANGE);

function LoadDevices(GUID_DevClass: TGUID): TStringList;
function EnableDevice(SelectedItem: DWord): TDeviceControlResult;
function DisableDevice(SelectedItem: DWord): TDeviceControlResult;

implementation

// Setup api, based on SetupApi.pas JEDI library
const
  DIF_PROPERTYCHANGE = $00000012;
  DICS_ENABLE        = $00000001;
  DICS_DISABLE       = $00000002;
  DICS_FLAG_GLOBAL   = $00000001; // make change in all hardware profiles
  DIGCF_PRESENT      = $00000002;
  SPDRP_DEVICEDESC   = $00000000; // DeviceDesc (R/W)
  SPDRP_CLASS        = $00000007; // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID    = $00000008; // ClassGUID (R/W)
  SPDRP_FRIENDLYNAME = $0000000C; // FriendlyName (R/W)

type
  HDEVINFO    = Pointer;
  DI_FUNCTION = LongWord;    // Function type for device installer

  PSPClassInstallHeader  = ^TSPClassInstallHeader;
  SP_CLASSINSTALL_HEADER = packed record
    cbSize:          DWORD;
    InstallFunction: DI_FUNCTION;
  end;
  TSPClassInstallHeader = SP_CLASSINSTALL_HEADER;

  PSPPropChangeParams  = ^TSPPropChangeParams;
  SP_PROPCHANGE_PARAMS = packed record
    ClassInstallHeader: TSPClassInstallHeader;
    StateChange:        DWORD;
    Scope:              DWORD;
    HwProfile:          DWORD;
  end;
  TSPPropChangeParams = SP_PROPCHANGE_PARAMS;

  PSPDevInfoData  = ^TSPDevInfoData;
  SP_DEVINFO_DATA = packed record
    cbSize:    DWORD;
    ClassGuid: TGUID;
    DevInst:   DWORD; // DEVINST handle
    Reserved:  ULONG_PTR;
  end;
  TSPDevInfoData = SP_DEVINFO_DATA;

  TSetupDiEnumDeviceInfo             = function(DeviceInfoSet: HDEVINFO; MemberIndex: DWORD; var DeviceInfoData: TSPDevInfoData): LongBool; stdcall;
  TSetupDiSetClassInstallParamsA     = function(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader; ClassInstallParamsSize: DWORD): LongBool; stdcall;
  TSetupDiSetClassInstallParamsW     = function(DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData; ClassInstallParams: PSPClassInstallHeader; ClassInstallParamsSize: DWORD): LongBool; stdcall;
  TSetupDiSetClassInstallParams      = TSetupDiSetClassInstallParamsA;
  TSetupDiCallClassInstaller         = function(InstallFunction: DI_FUNCTION; DeviceInfoSet: HDEVINFO; DeviceInfoData: PSPDevInfoData): LongBool; stdcall;
  TSetupDiGetClassDevs               = function(ClassGuid: PGUID; const Enumerator: PAnsiChar; hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;
  TSetupDiGetDeviceRegistryPropertyA = function(DeviceInfoSet: HDEVINFO; const DeviceInfoData: TSPDevInfoData; Property_: DWORD; var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetDeviceRegistryPropertyW = function(DeviceInfoSet: HDEVINFO; const DeviceInfoData: TSPDevInfoData; Property_: DWORD; var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD; var RequiredSize: DWORD): BOOL; stdcall;
  TSetupDiGetDeviceRegistryProperty  = TSetupDiGetDeviceRegistryPropertyA;

var
  DevInfo:                          hDevInfo;
  SetupDiEnumDeviceInfo:            TSetupDiEnumDeviceInfo;
  SetupDiSetClassInstallParams:     TSetupDiSetClassInstallParams;
  SetupDiCallClassInstaller:        TSetupDiCallClassInstaller;
  SetupDiGetClassDevs:              TSetupDiGetClassDevs;
  SetupDiGetDeviceRegistryProperty: TSetupDiGetDeviceRegistryProperty;
  SetupApiLoadCount:                Integer = 0;

function LoadSetupApi: Boolean;
  var
    SetupApiLib: TLibHandle;
  begin
    Result := True;
    Inc(SetupApiLoadCount);
    if SetupApiLoadCount > 1 then
      Exit;
    SetupApiLib := LoadLibrary('SetupApi.dll');
    Result      := SetupApiLib <> 0;
    if Result then
      begin
      SetupDiEnumDeviceInfo            := GetProcedureAddress(SetupApiLib, 'SetupDiEnumDeviceInfo');
      SetupDiSetClassInstallParams     := GetProcedureAddress(SetupApiLib, 'SetupDiSetClassInstallParamsA');
      SetupDiCallClassInstaller        := GetProcedureAddress(SetupApiLib, 'SetupDiCallClassInstaller');
      SetupDiGetClassDevs              := GetProcedureAddress(SetupApiLib, 'SetupDiGetClassDevsA');
      SetupDiGetDeviceRegistryProperty := GetProcedureAddress(SetupApiLib, 'SetupDiGetDeviceRegistryPropertyA');
      end;
  end;

// implementation

function StateChange(NewState, SelectedItem: DWord; hDevInfo: hDevInfo): TDeviceControlResult;
  var
    PropChangeParams: TSPPropChangeParams;
    DeviceInfoData:   TSPDevInfoData;
  begin
    PropChangeParams.ClassInstallHeader.cbSize := SizeOf(TSPClassInstallHeader);
    DeviceInfoData.cbSize := SizeOf(TSPDevInfoData);
    // Get a handle to the Selected Item.
    if (not SetupDiEnumDeviceInfo(hDevInfo, SelectedItem, DeviceInfoData)) then
      begin
      Result := DCRErrEnumDeviceInfo;
      Exit;
      end;
    // Set the PropChangeParams structure.
    PropChangeParams.ClassInstallHeader.InstallFunction := DIF_PROPERTYCHANGE;
    PropChangeParams.Scope       := DICS_FLAG_GLOBAL;
    PropChangeParams.StateChange := NewState;
    if (not SetupDiSetClassInstallParams(hDevInfo, @DeviceInfoData,
      PSPClassInstallHeader(@PropChangeParams), SizeOf(PropChangeParams))) then
      begin
      Result := DCRErrSetClassInstallParams;
      Exit;
      end;
    // Call the ClassInstaller and perform the change.
    if (not SetupDiCallClassInstaller(DIF_PROPERTYCHANGE, hDevInfo, @DeviceInfoData)) then
      begin
      Result := DCRErrDIF_PROPERTYCHANGE;
      Exit;
      end;
    Result := DCROK;
  end;

function GetRegistryProperty(PnPHandle: HDEVINFO; DevData: TSPDevInfoData; Prop: DWORD; Buffer: PChar; dwLength: DWord): Boolean;
  var
    aBuffer: array[0..256] of Char;
  begin
    dwLength   := 0;
    aBuffer[0] := #0;
    SetupDiGetDeviceRegistryProperty(PnPHandle, DevData, Prop, Prop, PBYTE(@aBuffer[0]), SizeOf(aBuffer), dwLength);
    StrCopy(Buffer, aBuffer);
    Result     := Buffer^ <> #0;
  end;

function ConstructDeviceName(DeviceInfoSet: hDevInfo; DeviceInfoData: TSPDevInfoData; Buffer: PChar; dwLength: DWord): Boolean;
  const
    UnknownDevice = '<Unknown Device>';
  begin
    if (not GetRegistryProperty(DeviceInfoSet, DeviceInfoData, SPDRP_FRIENDLYNAME, Buffer, dwLength)) then
      begin
      if (not GetRegistryProperty(DeviceInfoSet, DeviceInfoData, SPDRP_DEVICEDESC, Buffer, dwLength)) then
        begin
        if (not GetRegistryProperty(DeviceInfoSet, DeviceInfoData, SPDRP_CLASS, Buffer, dwLength)) then
          begin
          if (not GetRegistryProperty(DeviceInfoSet, DeviceInfoData, SPDRP_CLASSGUID, Buffer, dwLength)) then
            begin
            dwLength := DWord(SizeOf(UnknownDevice));
            Buffer   := Pointer(LocalAlloc(LPTR, Cardinal(dwLength)));
            StrCopy(Buffer, UnknownDevice);
            end;
          end;
        end;
      end;
    Result := True;
  end;


function LoadDevices(GUID_DevClass: TGUID): TStringList;
  var
    DeviceInfoData: TSPDevInfoData;
    i:              DWord;
    pszText:        PChar;
  begin
    if (not LoadSetupAPI) then
      begin
      Result := nil;
      Exit;
      end;
    DevInfo := nil;
    // Get a handle to all devices in all classes present on system
    DevInfo := SetupDiGetClassDevs(@GUID_DevClass, nil, 0, DIGCF_PRESENT);
    if (DevInfo = Pointer(INVALID_HANDLE_VALUE)) then
      begin
      Result := nil;
      Exit;
      end;
    Result := TStringList.Create;
    DeviceInfoData.cbSize := SizeOf(TSPDevInfoData);
    i := 0;
    // Enumerate though all the devices.
    while SetupDiEnumDeviceInfo(DevInfo, i, DeviceInfoData) do
      begin
      GetMem(pszText, 256);
        try
        // Get a friendly name for the device.
        ConstructDeviceName(DevInfo, DeviceInfoData, pszText, DWord(nil));
        Result.AddObject(pszText, TObject(i));
        finally
        FreeMem(pszText);
        Inc(i);
        end;
      end;
  end;

function EnableDevice(SelectedItem: DWord): TDeviceControlResult;
  begin
    Result := StateChange(DICS_ENABLE, SelectedItem, DevInfo);
  end;

function DisableDevice(SelectedItem: DWord): TDeviceControlResult;
  begin
    Result := StateChange(DICS_DISABLE, SelectedItem, DevInfo);
  end;

end.
