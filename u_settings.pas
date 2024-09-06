unit u_Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin;

type

  { TfrmConfig }

  TfrmConfig = class(TForm)
    btnAceptar: TButton;
    btnCancelar: TButton;
    cboTileSize: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    lblPixelSize: TLabel;
    speTilesetSize: TSpinEdit;
    procedure btnAceptarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure cboTileSizeChange(Sender: TObject);

  private

  public

  end;

var
  frmConfig: TfrmConfig;

implementation

{$R *.lfm}

{ TfrmConfig }

procedure TfrmConfig.btnCancelarClick(Sender: TObject);
begin

  ModalResult := 2;
  Close;
end;

procedure TfrmConfig.cboTileSizeChange(Sender: TObject);
begin
  if speTilesetSize.Value < (StrToInt(cboTileSize.Caption)*2) then
      speTilesetSize.Value:= StrToInt(cboTileSize.Caption)*2;
  speTilesetSize.Increment:= StrToInt(cboTileSize.Caption);
end;


procedure TfrmConfig.btnAceptarClick(Sender: TObject);
var
  res: integer;
  errorflag: boolean;
begin
  res := 0;
  errorflag := False;
  if StrToInt(cboTileSize.Caption) < 4 then
      begin
        res := Application.MessageBox('El tamaño del tile no puede ser inferior a 4.',
                                    'Valor incorrecto', $10);
        errorflag := True;
        cboTileSize.Caption:= '4';
        cboTileSize.SetFocus;
      end
  else if StrToInt(cboTileSize.Caption) > 64 then
      begin
        res := Application.MessageBox('El tamaño del tile no puede ser superior a 64.',
                                    'Valor incorrecto', $10);
        errorflag := True;
        cboTileSize.Caption:= '64';
        cboTileSize.SetFocus;
      end;

  if speTilesetSize.Value < StrToInt(cboTileSize.Caption)*2 then
      begin
        res := Application.MessageBox('El ancho del tileset no puede ser inferior al doble del tamaño del tile.',
                                    'Valor incorrecto', $10);
        errorflag := True;
        speTilesetSize.Value:= StrToInt(cboTileSize.Caption)*2;
        speTilesetSize.SetFocus;
      end
  else if speTilesetSize.Value > 1024 then
      begin
        res := Application.MessageBox('El ancho del tile no puede ser superior a 1024.',
                                    'Valor incorrecto', $10);
        errorflag := True;
        speTilesetSize.SetFocus;
      end;

  if not errorflag then
      ModalResult := 1;

end;





end.

