unit u_About;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TfrmAcerca }

  TfrmAcerca = class(TForm)
    btnCerrar: TButton;
    Image1: TImage;
    lblTitulo: TLabel;
    lblVersion: TLabel;
    lblVersion1: TLabel;
    procedure btnCerrarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);


  private

  public

    Const
      VERSION         = '0.1a';

  end;

var
  frmAcerca: TfrmAcerca;

implementation

{$R *.lfm}

{ TfrmAcerca }





procedure TfrmAcerca.btnCerrarClick(Sender: TObject);
begin
  frmAcerca.Close;
end;

procedure TfrmAcerca.FormCreate(Sender: TObject);
begin
  lblVersion.Caption:= '2020 ' + VERSION;
end;






end.

