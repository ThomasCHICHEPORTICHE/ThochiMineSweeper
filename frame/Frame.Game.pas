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
    procedure CellButtonClick(Sender: TObject);
    procedure ButtonRefresh(
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

procedure TFrameGame.ButtonRefresh(
  const AButton: TButton
);
var
  rCell: TCell;
  rCellList: TCellList;
begin
  rCell   := TCell(AButton.TagObject);

  try
    if rCell.IsRevealed then
      Exit;

    AButton.Text := '';
    if rCell.IsFlagged then
      AButton.Text := 'F'
    else if rCell.IsBomb then
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
    rCell.IsRevealed := True;
    inherited;
  end;
end;

procedure TFrameGame.CellButtonClick(Sender: TObject);
var
  rButton: TButton;
  rCell: TCell;
begin
  rButton := TButton(Sender);
  rCell   := TCell(rButton.TagObject);

  try
    if rCell.IsBomb then
      raise Exception.Create('Big bada boum !');
  finally
    ButtonRefresh(rButton);
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
    rButton           := TButton.Create(GPLBoardGame);
    rButton.Align     := TAlignLayout.Client;
    rButton.Parent    := GPLBoardGame;
    rButton.TagObject := ACell;
    GPLBoardGame.ControlCollection.AddControl(rButton, ACell.Column, ACell.Row);

    rButton.OnClick   := CellButtonClick;
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

