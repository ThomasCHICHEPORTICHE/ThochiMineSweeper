program ThochiMineSweeper;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Application in '..\form\Form.Application.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
