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
  MS.Types,
  FMX.Objects,
  FMX.Controls.Presentation,
  System.Generics.Collections, System.ImageList, FMX.ImgList
  ;

type
  TFrameGame = class(TFrameBase)
    GPLBoardGame: TGridPanelLayout;
    IBombLeftCount: TImage;
    LBombLeftCount: TLabel;
    LTimer: TLabel;
    ITimer: TImage;
    TGame: TTimer;
    ILGame: TImageList;
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
    procedure RevealAllAdjacentEmptyCells(
      const ACell: TCell
    );
    procedure RevealAllBombs;

    procedure GameOver;
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
  Form.Application,
  System.DateUtils,
  System.StrUtils,
  FMX.DialogService
  ;

{ TFrameGame }

procedure TFrameGame.CellButtonMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X: Single;
  Y: Single
);
var
  rCellButton: TCellButton;
begin
  if (not FGameBoard.IsRunning) then
    Exit;

  rCellButton := TCellButton(Sender);
  try
    case Button of
      TMouseButton.mbLeft:
        CellButtonLeftClick(rCellButton);

      TMouseButton.mbRight:
        CellButtonRightClick(rCellButton);
    else
      Exit;
    end;
  except
    on E: EBombException do
      GameOver;
    on E: Exception do
      Raise E;
  end;
  CellButtonRefresh(rCellButton);
  RefreshUI;
end;

procedure TFrameGame.CellButtonRefresh(
  const ACellButton: TCellButton
);
var
  rCellList: TCellList;
begin
  try
    ACellButton.ImageIndex := -1;

    if ((not ACellButton.Cell.IsRevealed) and ACellButton.Cell.IsFlagged) then
      ACellButton.ImageIndex := 9;

    if (not ACellButton.Cell.IsRevealed) then
      Exit;

    if ACellButton.Cell.IsBomb then
      ACellButton.ImageIndex := 10
    else
    begin
      rCellList := FGameBoard.AdjacentCellList[ACellButton.Cell];
      try
        ACellButton.ImageIndex := ILGame.Source.IndexOf(Format('number-%d', [rCellList.BombCount]));
        if (rCellList.BombCount = 0) then
          RevealAllAdjacentEmptyCells(ACellButton.Cell);
      finally
        FreeAndNil(rCellList);
      end;
      ACellButton.IsPressed := True;
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

procedure TFrameGame.GameOver;
begin
  FGameBoard.Stop;
  RevealAllBombs;
  TDialogService.MessageDialog(
    'Game over !',
    TMsgDlgType.mtError,
    [TMsgDlgBtn.mbOK, TMsgDlgBtn.mbRetry],
    TMsgDlgBtn.mbOK,
    0,
    procedure(
      const AModalResult: TModalResult
    )
    begin
      if (AModalResult = mrRetry) then
        Load(FGameBoard.Size, FGameBoard.Difficulty)
      else
        FormApplication.MainMenu;
    end
  );
end;

procedure TFrameGame.CellButtonLeftClick(
  const ACellButton: TCellButton
);
begin
  if ACellButton.Cell.IsFlagged then
    Exit;
  ACellButton.Cell.IsRevealed := True;
  if ACellButton.Cell.IsBomb then
    raise EBombException.Create('Big bada boum !');
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
    rCellButton                 := ACell.CellButton;
    rCellButton.Parent          := GPLBoardGame;
    rCellButton.Images          := ILGame;
    rCellButton.OnMouseDown     := CellButtonMouseDown;
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

procedure TFrameGame.RevealAllAdjacentEmptyCells(
  const ACell: TCell
);
var
  oCellList: TCellList;
  rCell: TCell;
begin
  ACell.IsRevealed := True;
  oCellList := FGameBoard.AdjacentCellList[ACell];
  try
    for rCell in oCellList do
      RevealAllAdjacentEmptyCells(rCell);
  finally
    FreeAndNil(oCellList);
  end;
end;

procedure TFrameGame.RevealAllBombs;
var
  rCell: TCell;
begin
  for rCell in FGameBoard.CellList do
    if rCell.IsBomb then
    begin
      rCell.IsRevealed := True;
      CellButtonRefresh(rCell.CellButton);
    end;
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

end.

