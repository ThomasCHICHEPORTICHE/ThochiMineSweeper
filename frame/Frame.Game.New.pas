unit Frame.Game.New;

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
  FrameStand,
  Frame.Base,
  MS.Types,
  FMX.Controls.Presentation, FMX.Objects
  ;

type
  TFrameGameNew = class(TFrameBase)
    GLGameNew: TGridLayout;
    GLGameNewGameBoardSize: TGridLayout;
    GLGameNewGameDifficulty: TGridLayout;
    LGameNewOption: TLayout;
    SBGameBoardSize9x9: TSpeedButton;
    SBGameBoardSize16x16: TSpeedButton;
    SBGameBoardSizeCustom: TSpeedButton;
    SBGameDifficultyEasy: TSpeedButton;
    SBGameDifficultyNormal: TSpeedButton;
    SBGameDifficultyHard: TSpeedButton;
    BGameNewStart: TButton;
    procedure SBGameBoardSizeClick(Sender: TObject);
    procedure SBGameDifficultyClick(Sender: TObject);
    procedure BGameNewStartClick(Sender: TObject);
  private
    { Déclarations privées }
    FGameBoardSize: TGameBoardSize;
    FGameDifficulty: TGameDifficulty;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Frame.Game
  ;

{$R *.fmx}

{ TFrameBase1 }

procedure TFrameGameNew.BGameNewStartClick(Sender: TObject);
begin
  inherited;
  FrameStand.GetFrameInfo<TFrameGame>.Frame.Load(FGameBoardSize, FGameDifficulty);
  FrameStand.GetFrameInfo<TFrameGame>.Show;
end;

constructor TFrameGameNew.Create(AOwner: TComponent);
begin
  inherited;
  FGameBoardSize  := TGameBoardSize.gbs9x9;
  FGameDifficulty := TGameDifficulty.gdNormal;
end;

procedure TFrameGameNew.SBGameBoardSizeClick(Sender: TObject);
begin
  inherited;
  FGameBoardSize := TGameBoardSize(TComponent(Sender).Tag);
end;

procedure TFrameGameNew.SBGameDifficultyClick(Sender: TObject);
begin
  inherited;
  FGameDifficulty := TGameDifficulty(TComponent(Sender).Tag);
end;

end.

