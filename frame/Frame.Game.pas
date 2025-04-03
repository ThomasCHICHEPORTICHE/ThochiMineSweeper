unit Frame.Game;

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
  MS.Types, FMX.Objects, FMX.Controls.Presentation
  ;

type
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
  TFrameGame = class(TFrameBase)
    GPLBoardGame: TGridPanelLayout;
    IBombLeftCount: TImage;
    LBombLeftCount: TLabel;
    LTimer: TLabel;
    ITimer: TImage;
    TGame: TTimer;
    procedure TGameTimer(Sender: TObject);
  private
    { Déclarations privées }
    FGameBoard: TGameBoard;

    procedure LoadUI;
    procedure CellButtonLeftClick(
      const ACellButton: TCellButton
    );
    procedure CellButtonRightClick(
      const ACellButton: TCellButton
    );
    procedure CellButtonMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X: Single;
      Y: Single
    );
    procedure CellButtonRefresh(
      const ACellButton: TCellButton
    );
    procedure RefreshUI;
  public
    { Déclarations publiques }
    procedure Load(
      const AGameBoardSize: TGameBoardSize;
      const AGameDifficulty: TGameDifficulty
    );
  end;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  System.StrUtils
  ;

{ TFrameGame }

procedure TFrameGame.CellButtonMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X: Single;
  Y: Single
);
begin
  case Button of
    TMouseButton.mbLeft:
      CellButtonLeftClick(TCellButton(Sender));

    TMouseButton.mbRight:
      CellButtonRightClick(TCellButton(Sender));
  else
    Exit;
  end;
end;

procedure TFrameGame.CellButtonRefresh(
  const ACellButton: TCellButton
);
var
  rCellList: TCellList;
begin
  try
    ACellButton.Text := '';

    if ((not ACellButton.Cell.IsRevealed) and ACellButton.Cell.IsFlagged) then
      ACellButton.Text := 'F';

    if (not ACellButton.Cell.IsRevealed) then
      Exit;

    if ACellButton.Cell.IsBomb then
      ACellButton.Text := 'B'
    else
    begin
      rCellList := FGameBoard.AdjacentCellList[ACellButton.Cell];
      try
        if (rCellList.BombCount <> 0) then
          ACellButton.Text := rCellList.BombCount.ToString
        else
        begin
        end;
      finally
        FreeAndNil(rCellList);
      end;
    end;

  finally
    inherited;
  end;
end;

procedure TFrameGame.CellButtonRightClick(
  const ACellButton: TCellButton
);
begin
  try
    ACellButton.Cell.IsFlagged := (not ACellButton.Cell.IsFlagged);
  finally
    CellButtonRefresh(ACellButton);
    RefreshUI;
  end;
end;

procedure TFrameGame.CellButtonLeftClick(
  const ACellButton: TCellButton
);
begin
  try
    if ACellButton.Cell.IsFlagged then
      Exit;
    ACellButton.Cell.IsRevealed := True;
    if ACellButton.Cell.IsBomb then
      raise Exception.Create('Big bada boum !');
  finally
    CellButtonRefresh(ACellButton);
    RefreshUI;
  end;
end;

procedure TFrameGame.Load(
  const AGameBoardSize: TGameBoardSize;
  const AGameDifficulty: TGameDifficulty
);
begin
  FreeAndNil(FGameBoard);
  FGameBoard := TGameBoard.Create(AGameBoardSize, AGameDifficulty);
  FGameBoard.LoadMatrix;
  LoadUI;
  FGameBoard.Start;
end;

procedure TFrameGame.LoadUI;
  procedure AddCellItem(
    const ACellCollection : TGridPanelLayout.TCellCollection
  );
  var
    rCellItem: TGridPanelLayout.TCellItem;
  begin
    rCellItem           := TGridPanelLayout.TCellItem(ACellCollection.Add);
    rCellItem.SizeStyle := TGridPanelLayout.TSizeStyle.Absolute;
  end;

  procedure ResizeCellItem(
    const ACellCollection : TGridPanelLayout.TCellCollection
  );
  var
    rCellItem: TCollectionItem;
  begin
    for rCellItem in ACellCollection do
      TGridPanelLayout.TCellItem(rCellItem).Value := (GPLBoardGame.Size.Width / ACellCollection.Count);
  end;

  procedure AddCell(
    const ACell: TCell
  );
  var
    rCellButton: TCellButton;
  begin
    rCellButton             := TCellButton.Create(GPLBoardGame, ACell);
    rCellButton.Align       := TAlignLayout.Client;
    rCellButton.Parent      := GPLBoardGame;
    rCellButton.OnMouseDown := CellButtonMouseDown;
    GPLBoardGame.ControlCollection.AddControl(rCellButton, ACell.Column, ACell.Row);
  end;
var
  rRow: TArray<TCell>;
  rCell: TCell;
begin
  GPLBoardGame.BeginUpdate;
  try
    GPLBoardGame.ControlCollection.Clear;
    GPLBoardGame.RowCollection.Clear;
    GPLBoardGame.ColumnCollection.Clear;

    while (GPLBoardGame.RowCollection.Count < GAME_BOARD_SIZE_VALUE[FGameBoard.Size]) do
      AddCellItem(GPLBoardGame.RowCollection);
    ResizeCellItem(GPLBoardGame.RowCollection);

    while (GPLBoardGame.ColumnCollection.Count < GAME_BOARD_SIZE_VALUE[FGameBoard.Size]) do
      AddCellItem(GPLBoardGame.ColumnCollection);
    ResizeCellItem(GPLBoardGame.ColumnCollection);

    for rRow in FGameBoard.Matrix do
      for rCell in rRow do
        AddCell(rCell);
  finally
    GPLBoardGame.EndUpdate;
    RefreshUI;
  end;
end;

procedure TFrameGame.RefreshUI;
begin
  LBombLeftCount.Text := (FGameBoard.CellList.BombCount - FGameBoard.CellList.FlaggedCount).ToString;
end;

procedure TFrameGame.TGameTimer(Sender: TObject);
begin
  inherited;
  if (not Assigned(FGameBoard)) then
    Exit;
  if (not FGameBoard.StopWatch.IsRunning) then
    Exit;
  LTimer.Text := (FGameBoard.StopWatch.ElapsedMilliseconds div 1000).ToString;
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

