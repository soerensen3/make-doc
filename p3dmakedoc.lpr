program p3dmakedoc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, LazFileUtils, p3dparsepascal, p3dparsedoc;

type

  TP3DCommandType = ( ctShowHelp, ctParsePascal{, ctGenerateHTML, ctGenerateDepedencyGraph} );

  { TP3DCommand }

  TP3DCommand = class
    CmdType: TP3DCommandType;
    InputFiles: TStringList;
    OutputDir: String;

    destructor Destroy; override;
  end;

  { TP3DMakeDoc }

  TP3DMakeDoc = class(TCustomApplication)
    protected
      procedure DoRun; override;

    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;
      procedure WriteHelp; virtual;
  end;

{ TP3DCommand }

destructor TP3DCommand.Destroy;
begin
  InputFiles.Free;
  inherited Destroy;
end;

{ TP3DMakeDoc }

procedure TP3DMakeDoc.DoRun;
var
  FileName, OutFile: String;
  i, j: Integer;
  Ext: RawByteString;

  Commands: array of TP3DCommand;
  CurrentCommand: TP3DCommand;

  procedure NewCommand( CT: TP3DCommandType );
  begin
    SetLength( Commands, Length( Commands ) + 1 );

    CurrentCommand:= TP3DCommand.Create;
    CurrentCommand.CmdType:= CT;
    CurrentCommand.InputFiles:= TStringList.Create;;
    Commands[ High( Commands )]:= CurrentCommand;
    CurrentCommand.OutputDir:= GetCurrentDirUTF8;
  end;

begin
  P3DParsePascalInit;

  i:= 1;

  try
    while i <= Paramcount do begin
      case ParamStr( i ) of
        '-h', '--help': NewCommand( ctShowHelp );
        'parse': NewCommand( ctParsePascal );
      else begin
        if ( not Assigned( CurrentCommand )) then
          raise Exception.Create( 'No command defined!' )
        else if ( ParamStr( i ) = '-o' ) then begin
          Inc( i );
          CurrentCommand.OutputDir:= ParamStr( i );
        end else
          CurrentCommand.InputFiles.Add( ParamStr( i ));
        end;
      end;
      Inc( i );
    end;


    for i:= 0 to high( Commands ) do begin
      CurrentCommand:= Commands[ i ];
      for j:= 0 to CurrentCommand.InputFiles.Count - 1 do begin
        FileName:= CurrentCommand.InputFiles[ j ];
        Ext:= ExtractFileExt( Filename );
        OutFile:= AppendPathDelim( CurrentCommand.OutputDir ) + ExtractFileNameOnly( Filename );

        case CurrentCommand.CmdType of
          ctShowHelp: WriteHelp;
          ctParsePascal:
            case ( Ext ) of
              '.pas', '.pp':
                ParsePascalUnit( Filename, OutFile );
              '.lpi', '.lpr':
                ParsePascalProgram( FileName, OutFile );
              '.lpk':
                ParsePascalPackage( FileName, OutFile );
              else
                WriteLn( 'Unsupported file extension: ' + Ext );
            end;
{          ctGenerateHTML: begin
              if ( not DirectoryExistsUTF8( FileName )) then
                Exception.Create( 'Can not generate doc because the specified input directory does not exist: ' + FileName );
              if ( not DirectoryExistsUTF8( CurrentCommand.OutputDir )) then
                Exception.Create( 'Can not generate doc because the specified output directory does not exist: ' + CurrentCommand.OutputDir );

              ParseDocDirectory( Filename, CurrentCommand.OutputDir )
            end;}
        end;
      end;
      MakeDotFile( OutFile + '.gv' );
    end;
  finally
    for i:= 0 to High( Commands ) do
      Commands[ i ].Free;
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
end;

var
  Application: TP3DMakeDoc;
begin
  Application:=TP3DMakeDoc.Create(nil);
  Application.Title:='P3DMakeDoc';
  Application.Run;
  Application.Free;
end.

