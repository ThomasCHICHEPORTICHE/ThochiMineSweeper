unit Frame.Base;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FrameStand, FMX.Objects, FMX.Controls.Presentation
  ;

type
  TFrameBase = class(TFrame)
    LContent: TLayout;
    LMain: TLayout;
    LHeader: TLayout;
    LFooter: TLayout;
    LIOs: TLayout;
    RBackground: TRectangle;
    GLOption: TGridLayout;
    procedure FrameResized(Sender: TObject);
  private
    { D�clarations priv�es }
    [FrameStandAttribute]
    FFrameStand: TFrameStand;
  protected
    property FrameStand: TFrameStand read FFrameStand;
  public
    { D�clarations publiques }

  end;

implementation

{$R *.fmx}

procedure TFrameBase.FrameResized(Sender: TObject);
begin
  GLOption.ItemHeight := GLOption.Size.Height;
  GLOption.ItemWidth  := (GLOption.Size.Width / GLOption.ControlsCount);
end;

end.
