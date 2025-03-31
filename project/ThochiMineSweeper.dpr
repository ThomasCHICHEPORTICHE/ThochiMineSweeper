program ThochiMineSweeper;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Application in '..\form\Form.Application.pas' {FormApplication},
  Frame.Base in '..\frame\Frame.Base.pas' {FrameBase: TFrame},
  Frame.Main in '..\frame\Frame.Main.pas' {FrameMain: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormApplication, FormApplication);
  Application.Run;
end.
