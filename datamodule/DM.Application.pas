unit DM.Application;

interface

uses
  System.SysUtils, System.Classes, FMX.Types;

type
  TDataModule1 = class(TDataModule)
    LApplication: TLang;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

end.
