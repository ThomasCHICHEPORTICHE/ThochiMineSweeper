program ThochiMineSweeper;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Application in '..\form\Form.Application.pas' {FormApplication},
  Frame.Base in '..\frame\Frame.Base.pas' {FrameBase: TFrame},
  Frame.Main in '..\frame\Frame.Main.pas' {FrameMain: TFrame},
  MS.Types in '..\objects\MS.Types.pas',
  Frame.Game.New in '..\frame\Frame.Game.New.pas' {FrameGameNew: TFrame},
  Frame.Game in '..\frame\Frame.Game.pas' {FrameGame: TFrame},
  DM.Application in '..\datamodule\DM.Application.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormApplication, FormApplication);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
