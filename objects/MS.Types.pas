unit MS.Types;

interface

uses
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  Fmx.StdCtrls
  ;

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

  TCellButton = class;
  TCell = class
  private
    FRow: Integer;
    FColumn: Integer;
    FProperties: TCellProperties;
    FCellButton: TCellButton;
    function GetIsBom: Boolean;
    function GetIsRevealed: Boolean;
    function GetIsFlagged: Boolean;
    procedure SetIsFlagged(const Value: Boolean);
    procedure SetIsRevealed(const Value: Boolean);
    function GetCellButton: TCellButton;
  protected
  public
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
    property Properties: TCellProperties read FProperties write FProperties;
    property IsBomb: Boolean read GetIsBom;
    property IsRevealed: Boolean read GetIsRevealed write SetIsRevealed;
    property IsFlagged: Boolean read GetIsFlagged write SetIsFlagged;
    property CellButton: TCellButton read GetCellButton;


    constructor Create(
      const ARow: Integer;
      const AColumn: Integer;
      const ADifficulty: TGameDifficulty
    );
  end;

  TCellList = class(TObjectList<TCell>)
  private
    function GetBombCount: Integer;
    function GetBombLeftCount: Integer;
    function GetFlaggedCount: Integer;
  protected
  public
    property BombCount: Integer read GetBombCount;
    property BombLeftCount: Integer read GetBombLeftCount;
    property FlaggedCount: Integer read GetFlaggedCount;

    procedure AddCell(
      const ACell: TCell
    );
  end;

  TCellButton = class(TButton)
  private
    FCell: TCell;
  protected
  public
    property Cell: TCell read FCell;

    constructor Create(
      const AOwner: TComponent;
      const ACell: TCell
    );
  end;

  TGameBoard = class
  private
    FMatrix: TArray<TArray<TCell>>;
    FCellList: TCellList;
    FSize: TGameBoardSize;
    FDifficulty: TGameDifficulty;
    FStopWatch: TStopwatch;
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
    function GetCellList: TCellList;
    function GetAdjacentEmptyCellList(
      const ACell: TCell
    ): TCellList;
    function GetIsRunning: Boolean;
  protected
  public
    property Matrix: TArray<TArray<TCell>> read FMatrix;
    property Size: TGameBoardSize read FSize write SetSize;
    property Difficulty: TGameDifficulty read FDifficulty write SetDifficulty;
    property Cell[const ARow: Integer; const AColumn: Integer]: TCell read GetCell write SetCell;
    property AdjacentCellList[const ACell: TCell]: TCellList read GetAdjacentCellList;
    property CellList: TCellList read GetCellList;
    property StopWatch: TStopwatch read FStopWatch;
    property AdjacentEmptyCellList[const ACell: TCell]: TCellList read GetAdjacentEmptyCellList;
    property IsRunning: Boolean read GetIsRunning;

    constructor Create(
      const ASize: TGameBoardSize;
      const ADifficulty: TGameDifficulty
    );
    destructor Destroy; override;

    procedure LoadMatrix;
    procedure Start;
    procedure Stop;
  end;

  EBombException = class(Exception);

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

uses
  FMX.Types
  ;

{ TGameBoard }

constructor TGameBoard.Create(
  const ASize: TGameBoardSize;
  const ADifficulty: TGameDifficulty
);
begin
  FSize       := ASize;
  FDifficulty := ADifficulty;
end;

destructor TGameBoard.Destroy;
begin
  Stop;

  SetLength(FMatrix, 0, 0);
  FreeAndNil(FCellList);

  inherited;
end;

function TGameBoard.GetAdjacentCellList(
  const ACell: TCell
): TCellList;
begin
  Result := TCellList.Create(False);
  //TopLeft
  Result.AddCell(Cell[ACell.Row - 1, ACell.Column - 1]);
  //Top
  Result.AddCell(Cell[ACell.Row - 1, ACell.Column]);
  //TopRight
  Result.AddCell(Cell[ACell.Row - 1, ACell.Column + 1]);
  //Left
  Result.AddCell(Cell[ACell.Row, ACell.Column - 1]);
  //Right
  Result.AddCell(Cell[ACell.Row, ACell.Column + 1]);
  //BottomLeft
  Result.AddCell(Cell[ACell.Row + 1, ACell.Column - 1]);
  //Bottom
  Result.AddCell(Cell[ACell.Row + 1, ACell.Column]);
  //BottomRight
  Result.AddCell(Cell[ACell.Row + 1, ACell.Column + 1]);
end;

function TGameBoard.GetAdjacentEmptyCellList(const ACell: TCell): TCellList;
begin
  Result := TCellList.Create(False);
  //Top
  Result.AddCell(Cell[ACell.Row - 1, ACell.Column]);
  //Left
  Result.AddCell(Cell[ACell.Row, ACell.Column - 1]);
  //Right
  Result.AddCell(Cell[ACell.Row, ACell.Column + 1]);
  //Bottom
  Result.AddCell(Cell[ACell.Row + 1, ACell.Column]);
end;

function TGameBoard.GetCell(
  const ARow: Integer;
  const AColumn: Integer
): TCell;
begin
  Result := nil;
  if (
    (ARow < 0) or (AColumn < 0) or
    (ARow >= GAME_BOARD_SIZE_VALUE[FSize]) or (AColumn >= GAME_BOARD_SIZE_VALUE[FSize])
  ) then
    Exit;
  Result := FMatrix[ARow, AColumn];
end;

function TGameBoard.GetCellList: TCellList;
var
  iRow: Integer;
  iColumn: Integer;
begin
  if (not Assigned(FCellList)) then
  begin
    FCellList := TCellList.Create(False);
    for iRow := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
      for iColumn := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
        FCellList.Add(FMatrix[iRow, iColumn]);
  end;
  Result := FCellList;
end;

function TGameBoard.GetIsRunning: Boolean;
begin
  Result := FStopWatch.IsRunning;
end;

procedure TGameBoard.LoadMatrix;
var
  iRow: Integer;
  iColumn: Integer;
begin
  SetLength(FMatrix, GAME_BOARD_SIZE_VALUE[FSize], GAME_BOARD_SIZE_VALUE[FSize]);
  for iRow := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
    for iColumn := 0 to GAME_BOARD_SIZE_VALUE[FSize] - 1 do
      FMatrix[iRow, iColumn] := TCell.Create(iRow, iColumn, FDifficulty);
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

procedure TGameBoard.Start;
begin
  if (not FStopWatch.IsRunning) then
    FStopWatch := TStopwatch.StartNew;
end;

procedure TGameBoard.Stop;
begin
  if FStopWatch.IsRunning then
    FStopWatch.Stop
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

function TCell.GetCellButton: TCellButton;
begin
  if (not Assigned(FCellButton)) then
  begin
    FCellButton                 := TCellButton.Create(nil, Self);
    FCellButton.Align           := TAlignLayout.Client;
    FCellButton.Margins.Top     := 1;
    FCellButton.Margins.Bottom  := 1;
    FCellButton.Margins.Left    := 1;
    FCellButton.Margins.Right   := 1;
    FCellButton.StaysPressed    := True;
    FCellButton.StyleLookup     := 'ButtonStyle';
    FCellButton.ApplyStyleLookup;
  end;
  Result := FCellButton;
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

procedure TCellList.AddCell(
  const ACell: TCell
);
begin
  if Assigned(ACell) then
    Add(ACell);
end;

function TCellList.GetBombCount: Integer;
var
  rCell: TCell;
begin
  Result := 0;
  for rCell in Self do
    if rCell.IsBomb then
      Inc(Result);
end;

function TCellList.GetBombLeftCount: Integer;
var
  rCell: TCell;
begin
  Result := 0;
  for rCell in Self do
    if (rCell.IsBomb and (not rCell.IsRevealed)) then
      Inc(Result);
end;

function TCellList.GetFlaggedCount: Integer;
var
  rCell: TCell;
begin
  Result := 0;
  for rCell in Self do
    if rCell.IsFlagged then
      Inc(Result);
end;

{ TCellButton }

constructor TCellButton.Create(
  const AOwner: TComponent;
  const ACell: TCell
);
begin
  inherited Create(AOwner);
  FCell := ACell;
end;

end.
