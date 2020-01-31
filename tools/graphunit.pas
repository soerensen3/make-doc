unit graphunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LvlGraphCtrl, Forms, Controls, Graphics, Dialogs, DOM, XMLRead;

type

  { TForm1 }

  TForm1 = class(TForm)
    LvlGraphControl1: TLvlGraphControl;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
  public
    procedure ScanFile( FN: String );
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  filename: String;
begin
  LvlGraphControl1.Clear;
  for filename in FileNames do
    ScanFile( FileName );
end;

procedure TForm1.ScanFile(FN: String);
var
  F: TXMLDocument;
  lst: TDOMNodeList;
  par, n: String;
  i: Integer;
begin
  ReadXMLFile( F, FN );
  try
    lst:= F.GetElementsByTagName( 'class' );
    for i:= 0 to lst.Count - 1 do
      begin
        if ( TDOMElement( lst[ i ]).hasAttribute( 'parent' )) then
          par:= TDOMElement( lst[ i ]).AttribStrings[ 'parent' ]
        else
          par:= 'TObject';

        if ( TDOMElement( lst[ i ]).hasAttribute( 'name' )) then
          n:= TDOMElement( lst[ i ]).AttribStrings[ 'name' ]
        else
          n:= 'Unknown';
        LvlGraphControl1.Graph.GetEdge( par, n, True );
      end;
  finally
    FreeAndNil( lst );
    FreeAndNil( F );
  end;
end;

end.

