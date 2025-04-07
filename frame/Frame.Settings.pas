unit Frame.Settings;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Base, FMX.Layouts, FMX.Objects, FMX.Controls.Presentation,
  SubjectStand;

type
  TFrameSettings = class(TFrameBase)
    GLSettings: TGridLayout;
    GLLanguage: TGridLayout;
    RBLanguageEN: TRadioButton;
    RBLanguageFR: TRadioButton;
    BCancel: TButton;
    BSave: TButton;
    GLStyle: TGridLayout;
    RBStyleClassic: TRadioButton;
    RBStyleModern: TRadioButton;
    procedure BCancelClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    [BeforeShow]
    procedure BeforeShow;
  end;

implementation

{$R *.fmx}

uses
  FMX.Styles,
  Form.Application,
  MS.Settings
  ;

{ TFrameSettings }

procedure TFrameSettings.BCancelClick(Sender: TObject);
begin
  inherited;
  FormApplication.MainMenu;
end;

procedure TFrameSettings.BeforeShow;
begin
  RBLanguageEN.IsChecked := FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value.Equals('en');
  RBLanguageFR.IsChecked := FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value.Equals('fr');

  RBStyleClassic.IsChecked  := FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_STYLE].Value.Equals('classic');
  RBStyleModern.IsChecked   := FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_STYLE].Value.Equals('modern');
end;

procedure TFrameSettings.BSaveClick(Sender: TObject);
begin
  inherited;
  if RBLanguageEN.IsChecked then
    FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value := 'en'
  else if RBLanguageFR.IsChecked then
    FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value := 'fr';
  FormApplication.LApplication.Lang := FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value;

  if RBStyleClassic.IsChecked then
    FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_STYLE].Value := 'classic'
  else if RBStyleModern.IsChecked then
    FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_STYLE].Value := 'modern';
  TStyleManager.TrySetStyleFromResource(FormApplication.Settings.Setting[SETTING_GROUP_GENERAL, SETTING_KEY_LANGUAGE].Value);

  FormApplication.MainMenu;
end;

end.
