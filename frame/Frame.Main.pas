unit Frame.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Base, FMX.Layouts, FMX.Controls.Presentation, FMX.Objects;

type
  TFrameMain = class(TFrameBase)
    GLMain: TGridLayout;
    BMenuNewGame: TButton;
    BMenuSettings: TButton;
    BMenuCredits: TButton;
    BMenuExit: TButton;
    procedure BMenuNewGameClick(Sender: TObject);
    procedure BMenuSettingsClick(Sender: TObject);
    procedure BMenuExitClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.fmx}

uses
  Form.Application,
  Frame.Game.New,
  Frame.Settings
  ;

procedure TFrameMain.BMenuExitClick(Sender: TObject);
begin
  inherited;
  FormApplication.Close;
end;

procedure TFrameMain.BMenuNewGameClick(Sender: TObject);
begin
  inherited;
  FrameStand.GetFrameInfo<TFrameGameNew>.Show;
end;

procedure TFrameMain.BMenuSettingsClick(Sender: TObject);
begin
  inherited;
  FrameStand.GetFrameInfo<TFrameSettings>.Show;
end;

end.
