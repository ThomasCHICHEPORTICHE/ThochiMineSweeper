unit MS.Types;

interface

type
  TGameBoardSize = (
    gbs9x9,
    gbs16x16,
    gbsCustom
  );

  TGameDifficulty = (
    gdEasy,
    gdNormal,
    gdHard
  );

  TCell = class
  private
    FX: Integer;
    FY: Integer;
    FIsBomb: Boolean;
  protected
  public
    property X: Integer read FX;
    property Y: Integer read FY;
    property IsBomb: Boolean read FIsBomb;

    constructor Create(
      const AX: Integer;
      const AY: Integer;
      const ADifficulty: TGameDifficulty
    );
  end;

  TGameBoard = class
  private
    FMatrix: TArray<TArray<TCell>>;
    FSize: TGameBoardSize;
    FDifficulty: TGameDifficulty;
    procedure SetSize(const Value: TGameBoardSize);
    procedure SetDifficulty(const Value: TGameDifficulty);
  protected
  public
    property Matrix: TArray<TArray<TCell>> read FMatrix;
    property Size: TGameBoardSize read FSize write SetSize;
    property Difficulty: TGameDifficulty read FDifficulty write SetDifficulty;

    procedure LoadMatrix;
  end;

const
  //Columns and Rows count
  GAME_BOARD_SIZE_VALUE : array[TGameBoardSize] of Integer = (
    9,
    16,
    -1
  );

  //Random range generation multiplier
  //ex: Random(2 * GAME_DIFFICULTY_VALUE[TGameDifficulty.gdEasy])
  GAME_DIFFICULTY_VALUE : array[TGameDifficulty] of Integer = (
    3,
    2,
    1
  );

implementation

{ TGameBoard }

procedure TGameBoard.LoadMatrix;
var
  iX: Integer;
  iY: Integer;
begin
  for iX := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
    for iY := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
      FMatrix[iX, iY] := TCell.Create(iX, iY, FDifficulty);
end;

procedure TGameBoard.SetDifficulty(const Value: TGameDifficulty);
begin
  FDifficulty := Value;
end;

procedure TGameBoard.SetSize(const Value: TGameBoardSize);
begin
  FSize := Value;
end;

{ TCell }

constructor TCell.Create(
  const AX: Integer;
  const AY: Integer;
  const ADifficulty: TGameDifficulty
);
begin
  FX := AX;
  FY := AY;

  Randomize;
  FIsBomb := (Random(2 * GAME_DIFFICULTY_VALUE[ADifficulty]) = 1);
end;

end.
