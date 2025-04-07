unit Form.Application;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FrameStand,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  MS.Settings
  ;

type
  TFormApplication = class(TForm)
    LApplication: TLang;
    SBApplication: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FFrameStand: TFrameStand;
    FSettings: TSettingList;
  public
    { Déclarations publiques }
    property FrameStand: TFrameStand read FFrameStand;
    property Settings: TSettingList read FSettings;

    procedure MainMenu;
  end;

var
  FormApplication: TFormApplication;

implementation

{$R *.fmx}

uses
  FMX.Styles,
  Frame.Main
  ;

procedure TFormApplication.FormCreate(Sender: TObject);
  procedure LoadSettings;
  begin
    FSettings := TSettingList.Load;

    LApplication.Lang := FSettings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value;
    TStyleManager.TrySetStyleFromResource(FSettings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_STYLE].Value);
  end;

  procedure LoadFrameStand;
  begin
    FFrameStand := TFrameStand.Create(Self);
    MainMenu;
  end;
begin
  LoadFrameStand;
end;

procedure TFormApplication.FormDestroy(Sender: TObject);
begin
  TSettingList.Save(FSettings);
  FreeAndNil(FSettings);
  FreeAndNil(FFrameStand);
end;

procedure TFormApplication.MainMenu;
begin
  FFrameStand.CloseAll;
  FFrameStand.GetFrameInfo<TFrameMain>.Show;
end;

end.
