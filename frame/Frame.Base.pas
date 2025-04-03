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
  private
    { Déclarations privées }
    [FrameStandAttribute]
    FFrameStand: TFrameStand;
  protected
    property FrameStand: TFrameStand read FFrameStand;
  public
    { Déclarations publiques }

  end;

implementation

{$R *.fmx}

end.
