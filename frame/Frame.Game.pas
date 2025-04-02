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
  MS.Types, FMX.Objects
  ;

type
  TFrameGame = class(TFrameBase)
    GPLBoardGame: TGridPanelLayout;
  private
    { Déclarations privées }
    FGameBoard: TGameBoard;

    procedure LoadUI;
    procedure CellButtonLeftClick(
      const ACell: TCell;
      const AButton: TButton
    );
    procedure CellButtonRightClick(
      const ACell: TCell;
      const AButton: TButton
    );
    procedure CellButtonMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X: Single;
      Y: Single
    );
    procedure CellButtonRefresh(
      const AButton: TButton
    );
  public
    { Déclarations publiques }
    procedure Load(
      const AGameBoardSize: TGameBoardSize;
      const AGameDifficulty: TGameDifficulty
    );
  end;

var
  FrameGame: TFrameGame;

implementation

{$R *.fmx}

{ TFrameGame }

procedure TFrameGame.CellButtonMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X: Single;
  Y: Single
);
var
  rCell: TCell;
  rButton: TButton;
begin
  rButton := TButton(Sender);
  rCell   := TCell(rButton.TagObject);
  case Button of
    TMouseButton.mbLeft:
      CellButtonLeftClick(rCell, rButton);

    TMouseButton.mbRight:
      CellButtonRightClick(rCell, rButton);
  else
    Exit;
  end;
end;

procedure TFrameGame.CellButtonRefresh(
  const AButton: TButton
);
var
  rCell: TCell;
  rCellList: TCellList;
begin
  rCell   := TCell(AButton.TagObject);

  try
    AButton.Text := '';

    if ((not rCell.IsRevealed) and rCell.IsFlagged) then
      AButton.Text := 'F';

    if (not rCell.IsRevealed) then
      Exit;

    if rCell.IsBomb then
      AButton.Text := 'B'
    else
    begin
      rCellList := FGameBoard.AdjacentCellList[rCell];
      try
        AButton.Text := rCellList.BombCount.ToString;
      finally
        FreeAndNil(rCellList);
      end;
    end;

  finally
    inherited;
  end;
end;

procedure TFrameGame.CellButtonRightClick(
  const ACell: TCell;
  const AButton: TButton
);
begin
  try
    ACell.IsFlagged := (not ACell.IsFlagged);
  finally
    CellButtonRefresh(AButton);
  end;
end;

procedure TFrameGame.CellButtonLeftClick(
  const ACell: TCell;
  const AButton: TButton
);
begin
  try
    ACell.IsRevealed := True;
    if ACell.IsBomb then
      raise Exception.Create('Big bada boum !');
  finally
    CellButtonRefresh(AButton);
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
    rButton: TButton;
  begin
    rButton             := TButton.Create(GPLBoardGame);
    rButton.Align       := TAlignLayout.Client;
    rButton.Parent      := GPLBoardGame;
    rButton.TagObject   := ACell;
    rButton.OnMouseDown := CellButtonMouseDown;
    GPLBoardGame.ControlCollection.AddControl(rButton, ACell.Column, ACell.Row);
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
  end;
end;

end.

