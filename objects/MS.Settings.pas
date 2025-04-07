unit MS.Settings;

interface

uses
  System.Generics.Collections
  ;

const
  SETTING_GROUP_GENERAL: string = 'General';
  SETTING_KEY_LANGUAGE: string = 'Language';
  SETTING_KEY_STYLE: string = 'Style';
  SETTING_GROUP_GAME: string = 'Game';
  SETTING_KEY_BOARD_SIZE: string = 'BoardSize';
  SETTING_KEY_DIFFICULTY: string = 'Difficulty';


type
  TSetting = class
  private
    FGroup: string;
    FKey: string;
    FValue: string;
  protected
  public
    property Group: string read FGroup write FGroup;
    property Key: string read FKey write FKey;
    property Value: string read FValue write FValue;

    constructor Create(
      const AGroup: string;
      const AKey: string;
      const AValue: string
    );
  end;

  TSettingList = class(TObjectList<TSetting>)
  private
    function GetSetting(
      const AGroup: string;
      const AKey: string;
      const ADefaultValue: string = ''
    ): TSetting;
  protected
  public
    property Setting[const AGroup: string; const AKey: string; const ADefaultValue: string = '']: TSetting read GetSetting;

    class function Load: TSettingList;
    class procedure Save(
      const ASettingList: TSettingList
    );
  end;

implementation

uses
  System.Classes,
  System.IniFiles,
  System.IOUtils,
  System.SysUtils
  ;

{ TSettingList }

function TSettingList.GetSetting(
  const AGroup: string;
  const AKey: string;
  const ADefaultValue: string = ''
): TSetting;
var
  rSetting: TSetting;
begin
  Result := nil;
  for rSetting in Self do
    if (rSetting.Group.Equals(AGroup) and rSetting.Key.Equals(AKey)) then
    begin
      Result := rSetting;
      Break;
    end;

  if (not Assigned(Result)) then
  begin
    Result := TSetting.Create(AGroup, AKey, ADefaultValue);
    Add(Result);
  end;
end;

class function TSettingList.Load: TSettingList;
var
  oIniFile: TIniFile;
  oSections: TStringList;
  sSection: string;
  oKeys: TStringList;
  sKey: string;
begin
  Result    := TSettingList.Create;
  oIniFile  := TIniFile.Create(TPath.ChangeExtension(ParamStr(0), 'ini'));
  try
    oSections := TStringList.Create;
    try
      oIniFile.ReadSections(oSections);
      for sSection in oSections do
      begin
        oKeys := TStringList.Create;
        oIniFile.ReadSection(sSection, oKeys);
        try
          for sKey in oKeys do
            Result.Add(TSetting.Create(sSection, sKey, oIniFile.ReadString(sSection, sKey, '')));
        finally
          FreeAndNil(oKeys);
        end;
      end;
    finally
      FreeAndNil(oSections)
    end;
  finally
    FreeAndNil(oIniFile);
  end;
end;

class procedure TSettingList.Save(
  const ASettingList: TSettingList
);
var
  oIniFile: TIniFile;
  rSetting: TSetting;
begin
  oIniFile  := TIniFile.Create(TPath.ChangeExtension(ParamStr(0), 'ini'));
  try
    for rSetting in ASettingList do
      oIniFile.WriteString(rSetting.Group, rSetting.Key, rSetting.Value);
  finally
    FreeAndNil(oIniFile);
  end;
end;

{ TSetting }

constructor TSetting.Create(
  const AGroup: string;
  const AKey: string;
  const AValue: string
);
begin
  FGroup  := AGroup;
  FKey    := AKey;
  FValue  := AValue;
end;

end.
