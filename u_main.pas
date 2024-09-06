unit u_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, ComCtrls, Menus, LazFileUtils, u_About, u_Settings;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    Image1: TImage;
    Image2: TImage;
    imgList: TImageList;
    MainMenu1: TMainMenu;
    mnuArchivo: TMenuItem;
    mnuExportarTiled: TMenuItem;
    mnuExportarTilemapCSV: TMenuItem;
    mnuSalir: TMenuItem;
    mnuVer: TMenuItem;
    mnuVerRejilla: TMenuItem;
    mnuAcerca: TMenuItem;
    mnuAbrir: TMenuItem;
    mnuGuardarComo: TMenuItem;
    mnuGuardarTilesetTilemap: TMenuItem;
    mnuGuardarTileset: TMenuItem;
    mnuGuardarTilemap: TMenuItem;
    MenuItem8: TMenuItem;
    mnuSeparador1: TMenuItem;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    spbAbrir: TSpeedButton;
    spbGenerar: TSpeedButton;
    spbConfig: TSpeedButton;
    spbGuardar: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    stInfo: TStaticText;
    tView: TTreeView;


    procedure FormCreate(Sender: TObject);
    procedure mnuExportarTiledClick(Sender: TObject);
    procedure mnuExportarTilemapCSVClick(Sender: TObject);
    procedure mnuSalirClick(Sender: TObject);
    procedure mnuVerRejillaClick(Sender: TObject);
    procedure mnuAcercaClick(Sender: TObject);
    procedure mnuAbrirClick(Sender: TObject);
    procedure mnuGuardarTilesetTilemapClick(Sender: TObject);
    procedure mnuGuardarTilesetClick(Sender: TObject);
    procedure mnuGuardarTilemapClick(Sender: TObject);
    procedure spbAbrirClick(Sender: TObject);
    procedure spbConfigClick(Sender: TObject);
    procedure spbGenerarClick(Sender: TObject);
    procedure spbGuardarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

  private

    // Tipo de dato para almacenar un tile
    type
      tTile = Record
      pixel : array [1..4096] of integer;
      activo : boolean;
    end;

    const
      IMG_INFORMACION = 0;
      IMG_ADVERTENCIA = 1;
      IMG_ERROR       = 2;
      IMG_ABRIR       = 3;
      IMG_GUARDAR     = 4;
      IMG_GENERAR     = 5;
      IMG_AJUSTES     = 6;
      IMG_OK          = 7;
      IMG_PREGUNTA    = 8;
      IMG_PROCESO     = 12;

    var
      TileSet: array of tTile;         // El tileset en RAM!
      TileMap: array of integer;       // El mapa de tiles!
      TotalBloques: Integer;           // División en bloques que componen la imagen fuente
      TilesUnicos: Integer;            // Los tiles únicos detectados que conforman el tileset final
      VerGrid: Boolean;                // Flag para dibujar o no la rejilla
      Nombre: String;                  // Nombre del fichero abierto

    Function ComparaTiles(var tile1: tTile; var tile2: array of tTile) : boolean;
    Function BuscaTile(var tileTemp: tTile) : integer;
    procedure CargaTileTemp(var tile1: tTile; x: integer; y: integer);
    procedure CopiaTile(var tile1: tTile; var tileset1: array of tTile; TilePos: integer);
    procedure Mensaje(texto: string; tipo: byte);
    procedure DibujaGrid;
    procedure HabilitarGuardar(Enable: Boolean);
    procedure HabilitarProcesar(Enable: Boolean);
    procedure MostrarTileset;
    procedure MostrarInfoTileset;
    procedure ComprobarDiv;
    procedure InicializarTileset;
    procedure GuardarTileset(Archivo: string);
    procedure GuardarTilemapCSV(Archivo: string);
    procedure ExportarTiled(Archivo: string);
    function TilemapToTMX() : String;
    procedure GuardarTilemapBIN(Archivo: string);


  public

    Const
      VERSION         = '0.1a';

    Var
      TileSize:       integer;       // Tamaño del tile
      TilesetSize:    integer;       // Tamaño horizontal del Tilset a generar

  end;


  // Tipo para almacenar la array de tiles del tileset
  //type
  //  tTileArray = array of tTile;

var
  frmMain: TfrmMain;
  ImgSrc: TPicture;                // Imagen completa de origen
  ImgTileset: TPicture;            // Imagen resultante del tileset generado



implementation

{$R *.lfm}

{ TfrmMain }


// ComparaTiles
function TfrmMain.ComparaTiles(var tile1: tTile; var tile2: array of tTile): boolean;
var
  pixel, tilen : integer;
  flag : Boolean;
  TilesDiferentes : integer;
begin
  TilesDiferentes := 0;

  for tilen := 0 to TilesUnicos-1 do begin
    flag := False;
    for pixel := 1 to TileSize*TileSize do begin
      if tile1.pixel[pixel] <> tile2[tilen].pixel[pixel] then begin
        flag := True;
        Break;
      end;
    end;
    if flag then
      TilesDiferentes := TilesDiferentes + 1;
  end;


  if TilesDiferentes = TilesUnicos then
    ComparaTiles := False
  Else
    ComparaTiles := True;
end;

// BuscaTile
// Se le pasa un tTile, lo busca en el tileset
// Si encuentra equivalencia devuelve el índice del tile en el tileset
// Si no lo encuentra devuelve -1
function TfrmMain.BuscaTile(var tileTemp: tTile): integer;
var
  pixel, tilen : integer;
  flag : Boolean;
  //TileId: integer;
begin
  BuscaTile := -1;
  for tilen := 0 to TilesUnicos-1 do begin
    flag := False;

    for pixel := 1 to TileSize*TileSize do begin
      if tileTemp.pixel[pixel] <> TileSet[tilen].pixel[pixel] then begin
        flag := True;
        Break;
      end;
    end;

    if not flag then begin
      BuscaTile := tilen;
      Break;
    end;

  end;
end;


// CargaTileTemp
procedure TfrmMain.CargaTileTemp(var tile1: tTile; x: integer; y: integer);
var
  tx, ty, Pixel : integer;

begin
  Pixel := 1;
  for ty := y to y + TileSize-1 do begin
    for tx := x to x + TileSize-1 do begin
      tile1.pixel[Pixel] := ImgSrc.Pixmap.Canvas.Pixels[tx, ty];
      Pixel := Pixel + 1;   // En principio no tenemos por qué comprobar desbordamiento
    end;
  end;
end;


// CopiaTile
procedure TfrmMain.CopiaTile(var tile1: tTile; var tileset1: array of tTile; TilePos: integer);
var
  pixel: integer;

begin
  for pixel := 1 to TileSize*TileSize do begin
    tileset1[TilePos].pixel[pixel] := tile1.pixel[pixel];
  end;
  tileset1[TilePos].activo := True;

end;

// Mensaje
procedure TfrmMain.Mensaje(texto: string; tipo: byte);
var
  nodo: TTreeNode;
begin
  nodo := TTreeNode.Create(nil);
  nodo := tView.Items.Add(nodo, texto);
  nodo.SelectedIndex := tipo;
  nodo.ImageIndex := tipo;
  // Truco para desplazar la lista al final
  nodo.Selected := True;
  nodo.Selected := False;
  tView.Update;
end;

// DibujaGrid
procedure TfrmMain.DibujaGrid;
var
  x, y: integer;
begin
  x := 0;
  y := 0;
  if VerGrid then begin
    Image1.Canvas.Pen.Width:= 1;
    Image1.Canvas.Pen.Color := clGray;
    Image1.Canvas.Pen.Style := psSolid;

    // Image1.Picture.Bitmap := ImgSrc.Bitmap;   // Con esto el grid no se dibuja en el color seleccionado
    Image1.Picture.Bitmap.Canvas.Draw(0, 0, ImgSrc.Bitmap);

    while x < Image1.Width do begin
      Image1.Canvas.Line(x, 0, x, Image1.Height);
      x := x + TileSize;
    end;
    while y < Image1.Height do begin
      Image1.Canvas.Line(0, y, Image1.Width, y);
      y := y + TileSize;
    end;
    Image1.Invalidate;
  end
  else begin
    Image1.Picture.Bitmap.Canvas.Draw(0, 0, ImgSrc.Bitmap);
  end;
end;

// HabilitarGuardar
procedure TfrmMain.HabilitarGuardar(Enable: Boolean);
begin
  mnuGuardarComo.Enabled:= Enable;
  MenuItem8.Enabled:= Enable;
  spbGuardar.Enabled:= Enable;
end;

procedure TfrmMain.HabilitarProcesar(Enable: Boolean);
begin
  spbGenerar.Enabled:= Enable;
end;

// MostrarTileset
procedure TfrmMain.MostrarTileset;
var
  x, y, tx, ty, tilen, pixel: integer;
  fraccion: real;
  tilesAlto: integer;

begin
  Mensaje('Organizando tiles...', IMG_PROCESO);
  x := 0;
  y := 0;
  // Inicializamos el control de imagen 2 que mostrará el tileset
  Image2.Width := TileSetSize;
  tilesAlto := TilesUnicos Div ((TilesetSize-TileSize) Div TileSize);
  fraccion := Frac(TilesUnicos / ((TilesetSize-TileSize) Div TileSize));

  if (fraccion > 0.09) and (fraccion < 0.9) then begin
    tilesAlto := tilesAlto + 1;
  end;
  // BUG ..................................................
  // No se calcula bien el alto de la Image2.
  // De momento optamos por añadir siempre una fila más!!!
  // ......................................................
  tilesAlto := tilesAlto + 1;
  Image2.Height := tilesAlto * TileSize;



  for tilen := 0 to TilesUnicos do begin
    if TileSet[tilen].activo then begin
      pixel := 1;

      for ty := 0 to TileSize-1 do begin
        for tx :=0 to TileSize-1 do begin
          Image2.Canvas.Pixels[x+tx, y+ty] := TileSet[tilen].pixel[pixel];
          //Image2.Canvas.Pixels[x+tx, y+ty] := clRed;
          pixel := pixel+1;
        end;
      end;

      x := x + TileSize;
      if x > TileSetSize - TileSize then begin
        x := 0;
        y := y + TileSize;
      end;

    end;
  end;
  Image2.Invalidate;
  MostrarInfoTileset;
end;

// MostrarInfoTileset
procedure TfrmMain.MostrarInfoTileset;
begin
  stInfo.Caption:= 'Tile size: ' + IntToSTR(TileSize) + LineEnding +
                   'Tileset size: ' + IntToStr(TilesetSize);
  if Image2.Height > 0 then
    stInfo.Caption:= stInfo.Caption + ' x ' + IntToStr(Image2.Height);
end;

// ComprobarDiv
procedure TfrmMain.ComprobarDiv;
var
  res: integer;
begin
  HabilitarProcesar(True);  // Habilitamos las funciones de procesado
  res := Image1.Width mod TileSize;
  if res > 0 then begin
    Mensaje('El ancho de la imagen no es divisible entre el tamaño del tile ('
                + IntToStr(TileSize) + ').', IMG_ADVERTENCIA);
    HabilitarProcesar(False);  // Deshabilitamos las funciones de procesado
  end;
  res := Image1.Height mod TileSize;
  if res > 0 then begin
    Mensaje('El alto de la imagen no es divisible entre el tamaño del tile ('
                + IntToStr(TileSize) + ').', IMG_ADVERTENCIA);
    HabilitarProcesar(False);  // Deshabilitamos las funciones de procesado
  end;
end;

// InicializarTileset
procedure TfrmMain.InicializarTileset;
var
  x: integer;
begin
  for x :=0 to TotalBloques-1 do begin
    TileSet[x].activo := False;
  end;
  // Inicializamos el control de imagen 2 que mostrará el tileset
  Image2.Picture.Clear;
  Image2.Height:= 0;
end;

// GuardarTileset
procedure TfrmMain.GuardarTileset(Archivo: string);
begin
  try
  Image2.Picture.SaveToFile(Archivo, '.png');
  Mensaje('Tileset guardado: ' + ExtractFilename(Archivo), IMG_PROCESO);

  except
    on E:Exception do
      Mensaje('No se pudo exportar el tileset: ' + E.Message, IMG_ERROR);
  end
end;

// GuardarTilemapCSV
procedure TfrmMain.GuardarTilemapCSV(Archivo: string);
Var
  fsOut: TFileStream;
  TileMapCSV: String;
  MapIdx, TileCont, TilesHorizontales: Integer;
begin
  // Construimos el texto
  TilesHorizontales := ImgSrc.Width Div TileSize;
  TileCont := 1;
  TileMapCSV := IntToStr(TileMap[0]); // Agregamos el primer índice, luego el resto

  for MapIdx:=1 to TotalBloques-1 do begin
    if Tilecont = TilesHorizontales then begin
      TileMapCSV := TileMapCSV + LineEnding + IntToStr(TileMap[MapIdx]);
      TileCont := 1;
      end
    Else begin
      TileMapCSV := TileMapCSV + ',' + IntToStr(TileMap[MapIdx]);
      TileCont := TileCont + 1;
    end;
  end;

  // Guardamos
  try
    fsOut := TFileStream.Create(Archivo, fmCreate);
    fsOut.Write(TileMapCSV[1], length(TileMapCSV));
    fsOut.Free;
    Mensaje('Tilemap exportado: ' + ExtractFilename(Archivo), IMG_PROCESO);

  except
    on E:Exception do
      Mensaje('No se pudo exportar el tilemap: ' + E.Message, IMG_ERROR);
  end
end;

// ExportarTiled
procedure TfrmMain.ExportarTiled(Archivo: string);
Var
  fsOut: TFileStream;
  TileMapTMX: String;
  TilesetWidth, TilesetHeight, TileWidth, TileHeight, TileCount: Integer;
  ImageWidth, ImageHeight: Integer;
  ImageSrc: String;

begin
  ImageWidth:= Image2.Width;
  ImageHeight:= Image2.Height;
  TilesetWidth:= ImgSrc.Width Div TileSize;
  TilesetHeight:= ImgSrc.Height Div TileSize;
  TileWidth:= TileSize;
  TileHeight:= TileSize;
  TileCount:= TilesUnicos;
  ImageSrc:= ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.png';
  // Construimos el texto
  TileMapTMX:='<?xml version="1.0" encoding="UTF-8"?>' + LineEnding +
              '<map version="1.2" orientation="orthogonal" renderorder="right-down" width="' +
              IntToStr(TilesetWidth) + '" height="' + IntToStr(TilesetHeight) + '" tilewidth="' +
              IntToStr(TileWidth) + '" tileheight="' + IntToStr(TileHeight) +
              '" infinite="0" nextlayerid="2" nextobjectid="1">' + LineEnding +
              '<tileset firstgid="1" name="' + ImageSrc + '" tilewidth="' +
              IntToStr(TileWidth) + '" tileheight="' + IntToStr(TileHeight) +
              '" tilecount="' + IntToStr(TileCount) + '" columns="' + IntToStr(TilesetWidth) +
              '">' + LineEnding + '<image source="' + ImageSrc + '" width="' +
              IntToStr(ImageWidth) + '" height="' + IntToStr(ImageHeight) + '"/>' + LineEnding +
              '</tileset>' + LineEnding + '<layer id="1" name="' + ImageSrc + '" width="' +
              IntToStr(TilesetWidth) + '" height="' + IntToStr(TilesetHeight) + '">' + LineEnding +
              '<data encoding="csv">' + LineEnding;

  TileMapTMX:= TileMapTMX + TilemapToTMX(); // Incrustamos un formato CSV específico para Tiled
  TileMapTMX:= TileMapTMX + '</data>' + LineEnding + '</layer>' + LineEnding + '</map>' + LineEnding;

  // Guardamos
  try
    fsOut := TFileStream.Create(Archivo, fmCreate);
    fsOut.Write(TileMapTMX[1], length(TileMapTMX));
    fsOut.Free;
    Mensaje('Tilemap exportado: ' + ExtractFilename(Archivo), IMG_PROCESO);

  except
    on E:Exception do
      Mensaje('No se pudo exportar el tilemap: ' + E.Message, IMG_ERROR);
  end

end;

// TilemapToTMX
function TfrmMain.TilemapToTMX(): String;
Var
  TileMapCSV, Terminacion: String;
  MapIdx, TileCont, TilesHorizontales: Integer;
begin
  TileMapCSV := '';
  // Construimos el texto
  Terminacion := ',';
  TilesHorizontales := ImgSrc.Width Div TileSize;
  //TileMapCSV := IntToStr(TileMap[0]) + ','; // Agregamos el primer índice, luego el resto
  //TileCont := 2;
  Tilecont := 1;

  for MapIdx:=0 to TotalBloques-1 do begin
    if MapIdx = TotalBloques-1 then
      Terminacion := '';
    if Tilecont = TilesHorizontales then begin
      TileMapCSV := TileMapCSV + IntToStr(TileMap[MapIdx]) + Terminacion + LineEnding;
      TileCont := 1;
      end
    Else begin
      TileMapCSV := TileMapCSV + IntToStr(TileMap[MapIdx]) + Terminacion;
      TileCont := TileCont + 1;
    end;
  end;
  TilemapToTMX := TileMapCSV;
end;

// GuardarTilemapBIN
procedure TfrmMain.GuardarTilemapBIN(Archivo: string);
Var
  fsOut: TFileStream;
  TileMapBIN: array of byte;
  MapIdx: Integer;
begin
  // Construimos la concatenación de bytes;
  SetLength(TileMapBIN, TotalBloques);
  for MapIdx:=0 to TotalBloques-1 do begin
      TileMapBIN[MapIdx] := TileMap[MapIdx] - 1;  // El -1 es para optimizar procesos en MSX
  end;
  // Guardamos
  try
    try
      fsOut := TFileStream.Create(Archivo, fmCreate);
      fsOut.Write(TileMapBIN[0], length(TileMapBIN));
      Mensaje('Tilemap exportado: ' + ExtractFilename(Archivo), IMG_PROCESO);
    except
      on E:Exception do
        Mensaje('No se pudo exportar el tilemap: ' + E.Message, IMG_ERROR);
    end;
  finally
    fsOut.Free;
  end;

end;




























procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ImgSrc := TPicture.Create;
  ImgTileset := TPicture.Create;
  // Inicializamos la configuración por defecto
  TileSize := 8;
  TilesetSize := 256;
  VerGrid := True;
  mnuVerRejilla.Checked := True;  // Menú "Ver rejilla"
  Image1.Height:= 0;
  Image2.Height:= 0;
  MostrarInfoTileset;
  HabilitarGuardar(False);   // Deshabilitamos las funciones de guardado
  HabilitarProcesar(False);  // Deshabilitamos las funciones de procesado
  // Comenzamos!
  Mensaje('Tileset Generator v' + VERSION, IMG_INFORMACION);
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  mnuSalirClick(Sender);
end;

procedure TfrmMain.mnuExportarTiledClick(Sender: TObject);
var
  Archivo: String;
begin
  // Guardar TMX
  SaveDialog.Options:= [ofOverwritePrompt, ofPathMustExist, ofNoReadOnlyReturn];
  SaveDialog.Title:= 'Exportar Proyecto Tiled';
  SaveDialog.Filter:= 'Proyecto Tiled|*.tmx';
  SaveDialog.DefaultExt:= 'tmx';
  if not(SaveDialog.Execute) then Exit;
  // Construimos nombre
  Archivo := SaveDialog.FileName;
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo);
  // Guardamos
  ExportarTiled(Archivo + '.tmx');
  GuardarTileset(Archivo + '.png');
end;

procedure TfrmMain.mnuExportarTilemapCSVClick(Sender: TObject);
var
  Archivo: string;
begin
  SaveDialog.Options:= [ofOverwritePrompt, ofPathMustExist, ofNoReadOnlyReturn];
  //SaveDialog.FileName:= Nombre + '.csv';
  SaveDialog.Title:= 'Exportar Tilemap (CSV)';
  SaveDialog.Filter:= 'Archivo CSV|*.csv';
  SaveDialog.DefaultExt:= 'csv';
  if not(SaveDialog.Execute) then Exit;
  // Construimos nombre
  Archivo := SaveDialog.FileName;
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.csv';
  GuardarTilemapCSV(Archivo);
end;

procedure TfrmMain.mnuSalirClick(Sender: TObject);
begin
  // Liberación de recursos
  Mensaje('Limpiando recursos...',IMG_INFORMACION);
  imgList.Free;
  ImgSrc.Free;
  ImgTileset.Free;
  Image1.Free;
  Image2.Free;
end;

procedure TfrmMain.mnuVerRejillaClick(Sender: TObject);
begin
  if mnuVerRejilla.Checked then begin
    mnuVerRejilla.Checked := False;
  end
  else begin
    mnuVerRejilla.Checked := True;
  end;
  VerGrid := mnuVerRejilla.Checked;
  DibujaGrid;
end;

procedure TfrmMain.mnuAcercaClick(Sender: TObject);
begin
  frmAcerca.ShowModal;
end;

procedure TfrmMain.mnuAbrirClick(Sender: TObject);
begin
  spbAbrirClick(Sender);
end;

procedure TfrmMain.mnuGuardarTilesetTilemapClick(Sender: TObject);
begin
  spbGuardarClick(Sender);
end;

procedure TfrmMain.mnuGuardarTilesetClick(Sender: TObject);
var
  Archivo: string;
begin
  SaveDialog.Options:= [ofOverwritePrompt, ofPathMustExist, ofNoReadOnlyReturn];
  //SaveDialog.FileName:= Nombre + '_tileset';
  SaveDialog.Title:= 'Guardar Tileset';
  SaveDialog.Filter:= 'Imagen PNG|*.png';
  SaveDialog.DefaultExt:= 'png';
  if not(SaveDialog.Execute) then Exit;
  Archivo := SaveDialog.FileName;      // Construimos nombre
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.png';
  GuardarTileset(Archivo);
end;

procedure TfrmMain.mnuGuardarTilemapClick(Sender: TObject);
var
  Archivo: string;
begin
  // Guardar Tilemap binario
  SaveDialog.Options:= [ofOverwritePrompt, ofPathMustExist, ofNoReadOnlyReturn];
  // SaveDialog.FileName:= ExtractFileNameOnly(SaveDialog.FileName);
  SaveDialog.Title:= 'Guardar Tilemap binario';
  SaveDialog.Filter:= 'Binario BIN|*.bin';
  SaveDialog.DefaultExt:= 'bin';
  if not(SaveDialog.Execute) then Exit;
  Archivo := SaveDialog.FileName;      // Construimos nombre
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.bin';
  GuardarTilemapBIN(Archivo);
end;

procedure TfrmMain.spbAbrirClick(Sender: TObject);
var
  archivo: string;

begin
  OpenDialog.Options:= [ ofFileMustExist, ofPathMustExist, ofHideReadOnly ];
  OpenDialog.Title:= 'Abrir imagen';
  if not(OpenDialog.Execute) then Exit;
  Archivo := OpenDialog.Filename;

  Nombre := ExtractFileNameOnly(Archivo);
  ImgSrc.LoadFromFile(Archivo);
  Image1.Picture.Bitmap.PixelFormat := pf24bit;                // Esto hay que hacerlo para que no haya
  Image1.Picture.Bitmap.SetSize(ImgSrc.Width, ImgSrc.Height);  // problemas con el canal alpha de los PNG
  DibujaGrid;

  TotalBloques := Round(image1.Width Div TileSize) * Round(image1.Height Div TileSize);
  // Dimensionamos el array del tileset al tamaño total de bloques de la imagen original
  // para curarnos en salud
  setLength(TileSet, TotalBloques);
  // Dimensionamos el Tile Map
  setLength(TileMap, TotalBloques);

  // Inicializamos la tabla del tileset con todos los tiles inactivos
  //InicializarTileset;
  Mensaje('Fuente: ' + IntToStr(image1.Width) + ' x ' + IntToStr(image1.Height)
          + ' pixels. ' + IntToStr(TotalBloques) + ' bloques totales.', IMG_INFORMACION);

  // Reinicializamos el bitmap del tileset en memoria
  ImgTileset.Clear;
  imgTileset.Bitmap.SetSize(TileSetSize, 1024);
  imgTileset.Pixmap.Canvas.Refresh;
  InicializarTileset;

  MostrarInfoTileset;
  HabilitarGuardar(False);   // Deshabilitamos las funciones de guardado

  // Comprobamos divisibilidad por tamaño de tiles
  ComprobarDiv;

  // Actualizamos el caption de la ventana
  frmMain.Caption:= 'Tileset Generator v' + VERSION + ' [' + Nombre + ']';

end;

procedure TfrmMain.spbConfigClick(Sender: TObject);
var
  res: integer;

begin
  frmConfig.cboTileSize.Caption:= IntToStr(TileSize);
  res := frmConfig.ShowModal;
  if res = 1 then begin
    TileSize := StrToInt(frmConfig.cboTileSize.Caption);
    TileSetSize := frmConfig.speTilesetSize.Value;
    Mensaje('Tamaño tile: ' + IntToStr(TileSize) +
            '   Ancho tileset: ' + IntToStr(TilesetSize), IMG_AJUSTES);
    DibujaGrid;
    MostrarInfoTileset;
    ComprobarDiv;
  end;
end;



procedure TfrmMain.spbGenerarClick(Sender: TObject);
var
  x, y, TileId, MapIdx: Integer;
  // Tile temporal en RAM para realizar las comparaciones y equivalencias
  // *Almacena en un array los datos de color de cada pixel del tile
  TileTemp: tTile;

begin
  Mensaje('Generando el Tileset...', IMG_PROCESO);
  // Inicializamos la tabla del tileset con todos los tiles inactivos
  InicializarTileset;
  // El primer tile siempre es único
  CargaTileTemp(TileTemp, 0, 0);
  CopiaTile(TileTemp, TileSet, 0);

  TilesUnicos := 1;
  x := TileSize;
  y := 0;

  // Generar Tileset ...................................................
  While y < ImgSrc.Height do begin

      While x < ImgSrc.Width do begin

          CargaTileTemp(TileTemp, x, y);
          if ComparaTiles(TileTemp, TileSet) then begin
            //image1.Canvas.Rectangle(Rect(x,y,x+8,y+8));
            //image1.Update;
          end
          else begin
            // Tile nuevo!
            CopiaTile(TileTemp, TileSet, TilesUnicos);
            tilesunicos := tilesunicos + 1;
            //image1.Canvas.Rectangle(Rect(x,y,x+3,y+3));
            //image1.Update;
          end;

          x := x + TileSize;
      end;   // x
      y := y + TileSize;
      x := 0;
  end;       // y
  Mensaje('Tileset Generado: ' + IntToStr(TilesUnicos) + ' tiles.', IMG_INFORMACION);
  // Fin Generar Tileset .................................................

  // Mostrar Tileset
  MostrarTileset;

  // Generar Tilemap .....................................................
  x := 0;
  y := 0;
  TileId := -1;
  MapIdx := 0;
  While y < ImgSrc.Height do begin

      While x < ImgSrc.Width do begin

          CargaTileTemp(TileTemp, x, y);
          TileId := BuscaTile(TileTemp);
          if TileId >= 0 then begin
            TileMap[MapIdx] := TileId + 1; // sumamos 1 al Id para compatibilidad con Tiled
            MapIdx := MapIdx + 1;
          end;

          x := x + TileSize;
      end;   // x
      y := y + TileSize;
      x := 0;
  end;       // y
  Mensaje('Tilemap Generado.', IMG_INFORMACION);
  // Fin Generar Tilemap .................................................

  // Habilitamos las opciones de guardado
  HabilitarGuardar(True);
end;

procedure TfrmMain.spbGuardarClick(Sender: TObject);
var
  Archivo: string;
begin
  SaveDialog.Options:= [ofOverwritePrompt, ofPathMustExist, ofNoReadOnlyReturn];
  // Guardar Tileset
  // SaveDialog.FileName:= Nombre;
  SaveDialog.Title:= 'Guardar Tileset';
  SaveDialog.Filter:= 'Imagen PNG|*.png';
  SaveDialog.DefaultExt:= 'png';
  if not(SaveDialog.Execute) then Exit;
  Archivo := SaveDialog.FileName;      // Construimos nombre
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.png';
  GuardarTileset(Archivo);            // Guardamos
  // Guardar Tilemap binario
  SaveDialog.FileName:= ExtractFilePath(SaveDialog.FileName) + ExtractFileNameOnly(SaveDialog.FileName);
  SaveDialog.Title:= 'Guardar Tilemap binario';
  SaveDialog.Filter:= 'Binario BIN|*.bin';
  SaveDialog.DefaultExt:= 'bin';
  if not(SaveDialog.Execute) then Exit;
  Archivo := SaveDialog.FileName;      // Construimos nombre
  Archivo := ExtractFilePath(Archivo) + ExtractFileNameOnly(Archivo) + '.bin';
  GuardarTilemapBIN(archivo);         // Guardamos
end;




// .......................................................
//
//             WIP
//
// .......................................................
//
//
// CAMBIOS Y MEJORAS para gestión de archivos:
// https://wiki.freepascal.org/fileutil
// uses: LazFileUtils
// ExtractFileName(
// extractfilepath(
// ExtractFileExt(
// concatpaths(
// DeleteFile(FileName);
//
// Opciones de opendialog:
// http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/Dialogs_TOpenOptions.html
//
//
// Evitar sobreescritura de archivo png al exportar a Tiled
//
// Mejorar estética formulario y menús
//
// Mejorar formulario acerca de
//
// Multi-idioma???
//
// Zoom??
//
//


// .......................................................
//
//             BUGS
//
// .......................................................
//
//
// Resolver precisión al generar el alto de Image2 al dibujar el tileset.
// Mantener visible el splitter 1 en el resize de la ventana






















end.

