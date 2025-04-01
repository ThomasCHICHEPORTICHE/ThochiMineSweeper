unit MS.Types;

interface

uses
  System.Generics.Collections;

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

  TCellProperty = (
    cpBomb,
    cpRevealed,
    cpFlagged
  );
  TCellProperties = set of TCellProperty;

  TCell = class
  private
    FRow: Integer;
    FColumn: Integer;
    FProperties: TCellProperties;
    function GetIsBom: Boolean;
    function GetIsRevealed: Boolean;
    function GetIsFlagged: Boolean;
    procedure SetIsFlagged(const Value: Boolean);
    procedure SetIsRevealed(const Value: Boolean);
  protected
  public
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property Properties: TCellProperties read FProperties write FProperties;
    property IsBomb: Boolean read GetIsBom;
    property IsRevealed: Boolean read GetIsRevealed write SetIsRevealed;
    property IsFlagged: Boolean read GetIsFlagged write SetIsFlagged;


    constructor Create(
      const ARow: Integer;
      const AColumn: Integer;
      const ADifficulty: TGameDifficulty
    );
  end;

  TCellList = class(TObjectList<TCell>)
  private
    function GetBombCount: Integer;
  protected
  public
    property BombCount: Integer read GetBombCount;
  end;

  TGameBoard = class
  private
    FMatrix: TArray<TArray<TCell>>;
    FSize: TGameBoardSize;
    FDifficulty: TGameDifficulty;
    procedure SetSize(const Value: TGameBoardSize);
    procedure SetDifficulty(const Value: TGameDifficulty);
    function GetCell(
      const ARow: Integer;
      const AColumn: Integer
    ): TCell;
    procedure SetCell(
      const ARow: Integer;
      const AColumn: Integer;
      const ACell: TCell
    );
    function GetAdjacentCellList(
      const ACell: TCell
    ): TCellList;
  protected
    property AdjacentCellList[const ACell: TCell]: TCellList read GetAdjacentCellList;
  public
    property Matrix: TArray<TArray<TCell>> read FMatrix;
    property Size: TGameBoardSize read FSize write SetSize;
    property Difficulty: TGameDifficulty read FDifficulty write SetDifficulty;
    property Cell[const ARow: Integer; const AColumn: Integer]: TCell read GetCell write SetCell;

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

function TGameBoard.GetAdjacentCellList(
  const ACell: TCell
): TCellList;
begin
  Result := TCellList.Create(False);
  //TopLeft
  Result.Add(Cell[ACell.Row - 1, ACell.Column - 1]);
  //Top
  Result.Add(Cell[ACell.Row - 1, ACell.Column]);
  //TopRight
  Result.Add(Cell[ACell.Row - 1, ACell.Column + 1]);
  //Left
  Result.Add(Cell[ACell.Row, ACell.Column - 1]);
  //Right
  Result.Add(Cell[ACell.Row, ACell.Column + 1]);
  //BottomLeft
  Result.Add(Cell[ACell.Row + 1, ACell.Column - 1]);
  //Bottom
  Result.Add(Cell[ACell.Row + 1, ACell.Column]);
  //BottomRight
  Result.Add(Cell[ACell.Row + 1, ACell.Column + 1]);
end;

function TGameBoard.GetCell(
  const ARow: Integer;
  const AColumn: Integer
): TCell;
begin
  Result := nil;
  if (
    (ARow <= 0) and (AColumn <= 0) or
    (ARow >= GAME_BOARD_SIZE_VALUE[FSize]) and (AColumn >= GAME_BOARD_SIZE_VALUE[FSize])
  ) then
    Exit;
  Result := FMatrix[ARow, AColumn];
end;

procedure TGameBoard.LoadMatrix;
var
  iX: Integer;
  iY: Integer;
begin
  for iX := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
    for iY := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
      FMatrix[iX, iY] := TCell.Create(iX, iY, FDifficulty);
end;

procedure TGameBoard.SetCell(
  const ARow: Integer;
  const AColumn: Integer;
  const ACell: TCell
);
begin
  if (ARow >= GAME_BOARD_SIZE_VALUE[FSize]) and (AColumn >= GAME_BOARD_SIZE_VALUE[FSize]) then
    System.Error(reRangeError);

  FMatrix[ARow, AColumn] := ACell
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
  const ARow: Integer;
  const AColumn: Integer;
  const ADifficulty: TGameDifficulty
);
begin
  FRow := ARow;
  FColumn := AColumn;

  Randomize;
  if (Random(2 * GAME_DIFFICULTY_VALUE[ADifficulty]) = 1) then
    FProperties := FProperties + [TCellProperty.cpBomb];
end;

function TCell.GetIsBom: Boolean;
begin
  Result := (TCellProperty.cpBomb in FProperties);
end;

function TCell.GetIsFlagged: Boolean;
begin
  Result := (TCellProperty.cpFlagged in Properties);
end;

function TCell.GetIsRevealed: Boolean;
begin
  Result := (TCellProperty.cpRevealed in Properties);
end;

procedure TCell.SetIsFlagged(const Value: Boolean);
begin
  case Value of
    True:
      FProperties := FProperties + [TCellProperty.cpFlagged];
    False:
      FProperties := FProperties - [TCellProperty.cpFlagged];
  end;
end;

procedure TCell.SetIsRevealed(const Value: Boolean);
begin
  case Value of
    True:
      FProperties := FProperties + [TCellProperty.cpRevealed];
    False:
      FProperties := FProperties - [TCellProperty.cpRevealed];
  end;
end;

{ TCellList }

function TCellList.GetBombCount: Integer;
var
  rCell: TCell;
begin
  Result := 0;
  for rCell in Self do
    if rCell.IsBomb then
      Inc(Result);
end;

end.
