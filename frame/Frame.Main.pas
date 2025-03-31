unit Frame.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Frame.Base, FMX.Layouts, FMX.Controls.Presentation;

type
  TFrameMain = class(TFrameBase)
    GLMenu: TGridLayout;
    BMenuNewGame: TButton;
    BMenuSettings: TButton;
    BMenuCredits: TButton;
    BMenuExit: TButton;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FrameMain: TFrameMain;

implementation

{$R *.fmx}

end.
