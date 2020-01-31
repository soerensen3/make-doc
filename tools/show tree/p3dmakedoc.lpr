program p3dmakedoc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, LazFileUtils, p3dparsepascal, DOM, XMLWrite, p3dparsedoc
  { you can add units after this };

type

  { TP3DMakeDoc }

  TP3DMakeDoc = class(TCustomApplication)
    protected
      procedure DoRun; override;

    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
  end;

{ TP3DMakeDoc }

procedure TP3DMakeDoc.DoRun;
var
  OutputDir, FileName, OutFile: String;
  InputFiles: TStringList;
  i: Integer;
  Ext: RawByteString;
begin
  P3DParsePascalInit;

  InputFiles:= TStringList.Create;
  i:= 1;
  while i <= Paramcount do
    begin
      if ( ParamStr( i ) = '-o' ) then
        begin
          Inc( i );
          OutputDir:= ParamStr( i );
        end
      else
        InputFiles.Add( ParamStr( i ));
      Inc( i );
    end;


  for i:= 0 to InputFiles.Count - 1 do
    begin
      FileName:= InputFiles[ i ];
      Ext:= ExtractFileExt( Filename );
      OutFile:= AppendPathDelim( OutputDir ) + ExtractFileNameOnly( Filename );

      if ( not FileExistsUTF8( Filename )) then
        begin
          WriteLn( 'The specified file was not found: ' + Filename );
          Continue;
        end;
      case ( Ext ) of
        '.pas', '.pp', '.lpr':
          ParsePascalUnit( Filename, OutFile );
        '.lpi', '.lpk':
          ParsePascalPackage( FileName, OutFile );
        '':
          if ( DirectoryExistsUTF8( Filename )) then
            ParseDocFiles( Filename, OutputDir )
        else
          WriteLn( 'Unsupported file extension: ' + Ext );
      end;
    end;

  P3DParsePascalFinish;

  // stop program loop
  Terminate;
end;

constructor TP3DMakeDoc.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TP3DMakeDoc.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMakeDoc.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TP3DMakeDoc;
begin
  Application:=TP3DMakeDoc.Create(nil);
  Application.Title:='P3DMakeDoc';
  Application.Run;
  Application.Free;
end.

