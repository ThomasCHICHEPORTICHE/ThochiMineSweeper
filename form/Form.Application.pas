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
  FrameStand, FMX.Controls.Presentation, FMX.StdCtrls
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
  public
    { Déclarations publiques }
    property FrameStand: TFrameStand read FFrameStand;

    procedure MainMenu;
  end;

var
  FormApplication: TFormApplication;

implementation

{$R *.fmx}

uses
  Frame.Main
  ;

procedure TFormApplication.FormCreate(Sender: TObject);
begin
  FFrameStand := TFrameStand.Create(Self);
  MainMenu;
end;

procedure TFormApplication.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFrameStand);
end;

procedure TFormApplication.MainMenu;
begin
  FFrameStand.CloseAll;
  FFrameStand.GetFrameInfo<TFrameMain>.Show;
end;

end.
