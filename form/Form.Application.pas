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
  FrameStand
  ;

type
  TFormApplication = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { D�clarations priv�es }
    FFrameStand: TFrameStand;
  public
    { D�clarations publiques }
    property FrameStand: TFrameStand read FFrameStand;
  end;

var
  FormApplication: TFormApplication;

implementation

{$R *.fmx}

procedure TFormApplication.FormCreate(Sender: TObject);
begin
  FFrameStand := TFrameStand.Create(Self);
end;

procedure TFormApplication.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFrameStand);
end;

end.
