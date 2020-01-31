{ # license

    Copyright (c) <year> <copyright holders>

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
    THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  # description

    provides utilities for parsing the generated json file and generating a html report
}


unit p3dparsedoc;

{$mode objfpc}{$H+}

interface
  uses
    Classes, SysUtils, fpjson, jsonparser, RegExpr, LazFileUtils, FileUtil, UnitDictionary;

  procedure ParseDocDirectory( FilePath: String; TargetFilePath: String ); // Parses a directory for json files and runs ParseDocFile for each of them

  procedure ParseDocFile( FileName: String; TargetFilePath: String ); // Parses a json file and generates a html report that is saved to the
                                                                      // target path under the same name but with html extension

  procedure ParseDocPackage( FileName: String; TargetFilePath: String ); // Parses a json unit file and generates a html report that is saved to the
                                                                         // target path under the same name but with html extension

  procedure ParseDocUnit( FileName: String; TargetFilePath: String ); // Parses a json unit file and generates a html report that is saved to the
                                                                      // target path under the same name but with html extension

  function DirectoryFiles( FilePath: String; Mask: String ): TStringArray;

 const
   HTMLHead = '<html><head><title>%s</title><link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Roboto:regular,bold,italic,thin,light,bolditalic,black,medium&amp;lang=en"><link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons"><link rel="stylesheet" href="https://code.getmdl.io/1.3.0/material.indigo-red.min.css"><script defer src="https://code.getmdl.io/1.3.0/material.min.js"></script></head><body>';
   HTMLBody = '</body></html>';



implementation

procedure ParseDocDirectory( FilePath: String; TargetFilePath: String );
var
  JSONFile: String;
begin
  if ( not DirectoryExistsUTF8( FilePath )) then
    raise Exception.CreateFmt( 'The specified doc directory "%s" could not be found!', [ FilePath ]);

  for JSONFile in DirectoryFiles( FilePath, '*.json' ) do
    ParseDocFile( JSONFile, TargetFilePath );
end;

procedure ParseDocFile( FileName: String; TargetFilePath: String );
begin
  case ExtractFileExt( ExtractFileNameWithoutExt( FileName )) of
    '.unit': ParseDocUnit( FileName, TargetFilePath );
    '.package': ParseDocPackage( FileName, TargetFilePath );
    else
      raise Exception.Create( 'Cannot parse doc file: Unknown doc extension in file name "' + FileName + '"' );
  end;
end;

function FileToStr( FileName: String ): String;
var
  F: TStringList;
begin
  F:= TStringList.Create;
  try
    F.LoadFromFile( FileName );
    Result:= F.Text;
  finally
    F.Free;
  end;
end;

function ReplaceByObj( S: String; Obj: TJSONObject; const FreeObject: Boolean = False ): String;
var
  i: Integer;
begin
  Result:= S;
  for i:= 0 to Obj.Count - 1 do
    if ( Obj.Items[ i ].JSONType in [ jtArray, jtObject ]) then
      Result:= Result.Replace( '$' + Obj.Names[ i ] + '$', Obj.Items[ i ].AsJSON )
    else
      Result:= Result.Replace( '$' + Obj.Names[ i ] + '$', Obj.Items[ i ].AsString );
  if ( FreeObject ) then
    Obj.Free;
end;

function IterateOver( Obj: TJSONObject; Template: String ): String;
var
  i: Integer;
begin
  Result:= '';
  if ( Assigned( Obj )) then
    for i:= 0 to Obj.Count - 1 do
      if ( Obj.Items[ i ].JSONType = jtObject ) then
        Result+= ReplaceByObj( Template, TJSONObject( Obj.Items[ i ]));
end;

function LoadJSON( FileName: String ): TJSONData;
var
  p: TJSONParser;
  s: TFileStream;
begin
  s:= TFileStream.Create( FileName, fmOpenRead );
  p:= TJSONParser.Create( s );
  Result:= p.Parse;
  p.Free;
  s.Free;
end;

procedure ParseDocPackage( FileName: String; TargetFilePath: String );
var
  PackageName, content, TempTable, TempTableRow, TempList, TempListItem,
    TempBox, _dep, overview, _unit: String;
  json: TJSONObject;
  F: TStringList;

  function ParsePackageDeps: String;
  var
    DepsSect: TJSONArray;
    i: Integer;
    Dependency, depfilename, InDir: String;
  begin
    DepsSect:= json.Arrays[ 'Dependencies' ];
    Result:= '';

    InDir:= AppendPathDelim( ExtractFilePath( FileName ));

    if ( Assigned( DepsSect )) then begin
      for i:= 0 to DepsSect.Count - 1 do begin
        Dependency:= DepsSect.Strings[ i ];
        depfilename:= Dependency + '.unit';
        if ( FileExists( InDir + depfilename + '.json' )) then
          Result += TempListItem.Replace( '$item', '<a href="' + depfilename + '.html">' + Dependency + '</a>' )
        else
          Result += TempListItem.Replace( '$item', Dependency );
      end;
    end;
  end;

  function ParsePackageUnits: String;
  var
    UnitsSect: TJSONArray;
    i: Integer;
    _Unit, unitfilename, InDir: String;
  begin
    UnitsSect:= json.Arrays[ 'Units' ];
    Result:= '';

    InDir:= AppendPathDelim( ExtractFilePath( FileName ));

    if ( Assigned( UnitsSect )) then begin
      for i:= 0 to UnitsSect.Count - 1 do begin
        _Unit:= UnitsSect.Strings[ i ];
        unitfilename:= _Unit + '.unit';
        if ( FileExists( InDir + unitfilename + '.json' )) then
          Result += TempListItem.Replace( '$item', '<a href="' + unitfilename + '.html">' + _Unit + '</a>' )
        else
          Result += TempListItem.Replace( '$item', _Unit );
      end;
    end;
  end;

begin
  try
    json:= TJSONObject( LoadJSON( FileName ));

  except
    on E: Exception do
      raise Exception.Create( 'Malformed unit doc file: "' + FileName + '"' );
  end;

  PackageName:= TJSONObject( json ).Strings[ 'Name' ];

  content:= '';
  TempTable:= FileToStr( 'html/table.html' );
  TempTableRow:= FileToStr( 'html/table_row.html' );
  TempList:= FileToStr( 'html/list.html' );
  TempListItem:= FileToStr( 'html/list_item.html' );
  TempBox:= FileToStr( 'html/box.html' );

  _dep:= ReplaceByObj( TempList, TJSONObject( GetJSON( '{"title":"Dependencies","content":"' + StringToJSONString( ParsePackageDeps ) + '"}' )));
  _unit:= ReplaceByObj( TempList, TJSONObject( GetJSON( '{"title":"Units","content":"' + StringToJSONString( ParsePackageUnits ) + '"}' )));

  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Functions, procedures and operators","id":"Functions", "content":"' + StringToJSONString( IterateOver( json.Get( 'Functions', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Types","id":"Types", "content":"' + StringToJSONString( IterateOver( json.Get( 'Types', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Variables","id":"Variables", "content":"' + StringToJSONString( IterateOver( json.Get( 'Vars', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Constants","id":"Constants", "content":"' + StringToJSONString( IterateOver( json.Get( 'Consts', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  WriteLn('{"title":"' + PackageName + '", content":"' + StringToJSONString( json.Get( 'Comment', 'No description.' )) + '"}');
  overview := ReplaceByObj( TempBox, TJSONObject( GetJSON( '{"title":"' + PackageName + '", "content":"' + StringToJSONString( json.Get( 'Comment', 'No description.' )) + '"}' )));
  overview += _dep;
  overview += _unit;

  F:= TStringList.Create;
  F.Text:= //FileToStr( 'html/main_overview.html' ).Replace( '$title$', UnitName ).Replace( '$content$', content );
  ReplaceByObj( FileToStr( 'html/main_overview.html' ), TJSONObject( GetJSON( '{"title":"' + PackageName + '","overview":"'+ StringToJSONString( overview ) + '", "content":"' + StringToJSONString( content ) + '"}' )), True );
  F.SaveToFile( TargetFilePath + ExtractFileNameOnly( FileName ) + '.html' );
end;

procedure ParseDocUnit( FileName: String; TargetFilePath: String );
var
  json: TJSONObject;
  UnitName: String;
  TempTable: String;
  TempTableRow: String;
  TempList: String;
  TempListItem: String;
  TempBox: String;

  function ParseUnitUses: String;
  var
    UsesSect: TJSONArray;
    i: Integer;
    Dependency, depfilename: String;
  begin
    UsesSect:= json.Arrays[ 'Uses' ];
    if ( Assigned( UsesSect )) then
      begin
        for i:= 0 to UsesSect.Count - 1 do
          begin
            Dependency:= UsesSect.Strings[ i ];
            Result:= '';
            depfilename:= Dependency + '.unit';
            if ( FileExists( TargetFilePath + depfilename + '.json' )) then
              Result += TempListItem.Replace( '$item', '<a href="' + depfilename + '.html">' + Dependency + '</a>' )
            else
              Result += TempListItem.Replace( '$item', Dependency );
          end;
      end;
  end;

var
  F: TStringList;
  content, overview, _uses: String;
begin
  try
    json:= TJSONObject( LoadJSON( FileName ));

  except
    on E: Exception do
      raise Exception.Create( 'Malformed unit doc file: "' + FileName + '"' );
  end;

  UnitName:= TJSONObject( json ).Strings[ 'Name' ];

  content:= '';
  TempTable:= FileToStr( 'html/table.html' );
  TempTableRow:= FileToStr( 'html/table_row.html' );
  TempList:= FileToStr( 'html/list.html' );
  TempListItem:= FileToStr( 'html/list_item.html' );
  TempBox:= FileToStr( 'html/box.html' );

  _uses:= ReplaceByObj( TempList, TJSONObject( GetJSON( '{"title":"Uses","content":"' + StringToJSONString( ParseUnitUses ) + '"}' )));

  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Functions, procedures and operators","id":"Functions", "content":"' + StringToJSONString( IterateOver( json.Get( 'Functions', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Types","id":"Types", "content":"' + StringToJSONString( IterateOver( json.Get( 'Types', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Variables","id":"Variables", "content":"' + StringToJSONString( IterateOver( json.Get( 'Vars', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  content += ReplaceByObj( TempTable, TJSONObject( GetJSON( '{"title":"Constants","id":"Constants", "content":"' + StringToJSONString( IterateOver( json.Get( 'Consts', TJSONObject( nil )), TempTableRow )) + '"}' )), True );
  WriteLn('{"title":"' + UnitName + '", content":"' + StringToJSONString( json.Get( 'Comment', 'No description.' )) + '"}');
  overview := ReplaceByObj( TempBox, TJSONObject( GetJSON( '{"title":"' + UnitName + '", "content":"' + StringToJSONString( json.Get( 'Comment', 'No description.' )) + '"}' )));
  overview += _uses;

  F:= TStringList.Create;
  F.Text:= //FileToStr( 'html/main_overview.html' ).Replace( '$title$', UnitName ).Replace( '$content$', content );
  ReplaceByObj( FileToStr( 'html/main_overview.html' ), TJSONObject( GetJSON( '{"title":"' + UnitName + '","overview":"'+ StringToJSONString( overview ) + '", "content":"' + StringToJSONString( content ) + '"}' )), True );
  F.SaveToFile( TargetFilePath + ExtractFileNameOnly( FileName ) + '.html' );
end;

function DirectoryFiles( FilePath: String; Mask: String ): TStringArray;
var
  L: TStringList;
  i: Integer;
begin
  L:= FindAllFiles( FilePath, Mask, False );

  SetLength( Result, L.Count );

  for i:= 0 to L.Count - 1 do
    Result[ i ]:= L[ i ];
end;

end.

