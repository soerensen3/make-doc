unit p3dparsedoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, DOM, XMLRead, XMLWrite, RegExpr, strutils, math, LazFileUtils, FileUtil, UnitDictionary;

type

  { TDocPackage }

  TDocUnit = class;
  TDocUnitList = specialize TFPGList <TDocUnit>;

  TDocPackage = class ( TPersistent )
    private
      FActiveElementPostFix: String;
      FOutDir: String;
      FFileName: String;
      FName: String;
      FPackageNode: TDOMElement;
      FDictionary: TStringList;
      FUnits: TDocUnitList;
      FXML: TXMLDocument;
      procedure SetPackageNode(AValue: TDOMElement);
      procedure SetXML(AValue: TXMLDocument);
      property FileName: String read FFileName;
      property OutDir: String read FOutDir;

    public
      constructor Create(AFileName: String; AOutDir: String; ADictionary: TStringList );
      destructor Destroy; override;

      function ListOfAll( Name: String; const Base: TDOMElement = nil ): TList;
      function ParseDependencies( OutFile: String; Target: TDOMElement; const Src: TDOMElement = nil ): Boolean;
      function ParseUnits( OutFile: String; Target: TDOMElement; const Src: TDOMElement = nil ): Boolean;
      procedure PopulateDictionary;
      procedure WriteDocFiles( FinishedFiles: TStringList );

      function FindIdentifier( AName: String ): String;

    published
      property Name: String read FName write FName;
      property XML: TXMLDocument read FXML write SetXML;
      property PackageNode: TDOMElement read FPackageNode write SetPackageNode;
      property Dictionary: TStringList read FDictionary write FDictionary;
      property Units: TDocUnitList read FUnits write FUnits;
  end;

  { TDocUnit }

  TDocUnit = class ( TPersistent )
    private
      FActiveElementPostFix: String;
      FOutDir: String;
      FDictionary: TStringList;
      FFileName: String;
      FName: String;
      FPackage: TDocPackage;
      FUnitNode: TDOMElement;
      FUsedUnits: TStringList;
      FXML: TXMLDocument;
      function FullScope( const WithPackage: Boolean = False ): String;
      function GetScope: String;
      function GetFullScopeLnk: String;
      procedure SetUnitNode(AValue: TDOMElement);
      procedure SetXML(AValue: TXMLDocument);
      procedure ParseClass(OutFile: String; ClassNode: TDOMElement;
        FinishedFiles: TStringList);
      procedure ParseProc(OutFile: String; ProcNode: TDOMElement;
        FinishedFiles: TStringList);
      procedure ParseProcs(OutFile: String; ProcNodes: TList;
        FinishedFiles: TStringList);
      property FileName: String read FFileName;
      property OutDir: String read FOutDir;

      procedure FindUsedUnits( Src: TDOMElement );

    public
      function ListOfAll(Name: String; const Base: TDOMElement=nil ): TList;
      function FindDuplicates( Itm: TDOMElement; Base: TDOMElement = nil ): TList;
      procedure ParseProcedures(OutFile: String; FinishedFiles: TStringList;
        Target: TDOMElement; const Src: TDOMElement = nil );
      procedure ParseClasses( OutFile: String; FinishedFiles: TStringList; Target: TDOMElement;
        const Src: TDOMElement = nil );
      procedure ParseProperties( OutFile: String; Target: TDOMElement; const Src: TDOMElement = nil );

      constructor Create( AFileName: String; AOutDir: String; ADictionary: TStringList; APackage: TDocPackage );
      destructor Destroy; override;

      procedure PopulateDictionary;
      procedure PopulateClassDictionary( Base: TDOMElement );
      procedure WriteDocFiles( FinishedFiles: TStringList );
      function FindIdentifier( AName: String ): String;

    published
      property UsedUnits: TStringList read FUsedUnits write FUsedUnits;
      property Package: TDocPackage read FPackage write FPackage;
      property Name: String read FName write FName;
      property XML: TXMLDocument read FXML write SetXML;
      property UnitNode: TDOMElement read FUnitNode write SetUnitNode;
      property Dictionary: TStringList read FDictionary write FDictionary;
      property Scope: String read GetScope;
      property ActiveElementPostFix: String read FActiveElementPostFix write FActiveElementPostFix;
  end;

  procedure ParseDocFiles( InDir: String; OutDir: String );

implementation


type
  TFindIdentifierFunc = function ( Name: String ): String of object;

type TCharSet = set of Char;
Function PosEx2(c: TCharSet; const S: string; Offset: Cardinal): Integer;

var
  Len : longint;
  p: SizeInt;
  Ch: Char;
  p2: SizeInt;
begin
  Len := length(S);
  if (Offset < 1) or (Offset > SizeUInt(Length(S))) then exit(0);
  Len := length(S);
  p:= -1;
  for Ch in c do
    begin
      p2:= indexbyte(S[Offset],Len-offset+1,Byte(Ch));
      if ( p < 0 ) then
        p:= p2
      else if ( p2 >= 0 ) then
        p := Min( p, p2 );
    end;
  if (p < 0) then
    PosEx2 := 0
  else
    PosEx2 := p + sizeint(Offset);
end;
type

  { TRegReplacer }

  TRegReplacer = object
    FindIdent: TFindIdentifierFunc;
    function ReplaceCallback(ARegExpr : TRegExpr): string;
  end;

  function TRegReplacer.ReplaceCallback(ARegExpr: TRegExpr): string;
    function EscapeString( S: String ): String;
    begin
      Result:= ReplaceRegExpr( '[\.\^\$\*\+\?\(\)\[\{\\\|\^\-\]]', S, '\\{0}', True );
    end;

  var
    S: String;
    Name: RegExprString;
  begin
    //Name:= ReplaceRegExpr( '(function|procedure|property|constructor|destructor|:|operator)\<\/span\>\s*(<span class="symbol"\>)?', ARegExpr.Match[ 0 ], '', True );
    Name:= ReplaceRegExpr( '(function|procedure|property|constructor|destructor|:|operator)\s*', ARegExpr.Match[ 0 ], '', True );
    if ( Assigned( FindIdent )) then
      S:= FindIdent( Name );
    WriteLn( Name );
    if ( S > '' ) then
      Result:= ReplaceRegExpr( '\b' + EscapeString( Name ) + '\b', ARegExpr.Match[ 0 ], '<a href="' + S + '">' + Name + '</a>', False )
    else
      Result:= ARegExpr.Match[ 0 ];
  end;


function Pas2DocToHtml( S: String; const Short: Boolean = False; FindIdentCallback: TFindIdentifierFunc = nil  ): String;
  function ReplaceEx( ARegExpr: String; AInputStr : RegExprString; AReplaceFunc : TRegExprReplaceFunction): String;
   begin
    with TRegExpr.Create do try
      Expression := ARegExpr;
      Result := ReplaceEx(AInputStr, AReplaceFunc );
      finally Free;
     end;
   end;

  function FindIdentifiersCode( S: String ): String;
  const
    reg = '(((function|procedure|property|constructor|destructor)|\:)\<\/span\>\s*[_a-zA-Z][_a-zA-Z0-9]*)|(operator\<\/span\>\s*\<span class="symbol"\>[\+\-*/:=]{1,2})';

  var
    R: TRegReplacer;
  begin
    R.FindIdent:= FindIdentCallback;
    Result:= ReplaceEx( reg, S, @R.ReplaceCallback );
  end;

  function FindIdentifiers( S: String ): String;
  const
    reg = '\b[_a-zA-Z][_a-zA-Z0-9]*\b';

  var
    R: TRegReplacer;
  begin
    R.FindIdent:= FindIdentCallback;
    Result:= ReplaceEx( reg, S, @R.ReplaceCallback );
  end;

  function FindTag( S: String; var Offset: Integer; out beforeTag: Integer ): String;
  var
    i: Integer;
    i2: Integer;
  begin
    i:= PosEx( '@', S, OffSet );
    i2:= PosEx( '`', S, Offset );
    beforeTag:= 0;
    if (( i2 > 0 ) and (( i2 < i ) or ( i = 0 ))) then
      begin
        Result:= '`';
        Offset:= i2;
        beforeTag:= i2 - 1;
        exit;
      end;
    if ( i > 0 ) then
      begin
        Offset:= i + 1;
        i2:= PosEx2([ ' ', '`', '@', #8, #13, #10 ], S, Offset );
        if ( i2 - 1 = i ) then
          begin
            Result:= '';
            exit;
          end;
        Result:= Copy( S, i + 1, i2 - i - 1 );
        Offset:= i2;
        beforeTag:= i - 1;
      end
    else
      Result:= '';
  end;

  function ParseTags( S: String; var OffSet: Integer ): String;
  var
    Tag: String;
    OldOffSet: Integer;
    beforeTag: Integer;

    function AddText: String;
    begin
      Result:= Copy( S, OldOffSet, beforeTag - OldOffSet + 1 );
    end;

    {function CopyToNext( c: TCharSet; SkipChars: Integer ): String;
    var
      i: Integer;
    begin
      OldOffSet:= OffSet + SkipChars;
      repeat
        i:= PosEx2( c, S, OffSet + SkipChars );
        if ( i = 0 ) then
          break
        else
          OffSet:= i + 1;
      until S[ OffSet - 2 ] <> '@';
//      OldOffSet:= OldOffSet + Length( Tag ) + SkipChars + 1;
      Result:= Copy( S, OldOffSet, OffSet - OldOffSet - 1 );  //-1=Length ( C )
    end;}

    function Copy2SymbEx( S: String; Symbol: String; var OffSet: Integer ): String;
    var
      n: Integer;
    begin
      n:= PosEx( Symbol, S, OffSet );
      if n=0 then
        n:=Length(S)+1;

      Result:= Copy( S, OffSet, n - OffSet );
      OffSet:= n + 1;
    end;

    function MakeSection( S: String; var Offset: Integer ): String;
    var
      HLevel: String;
      HLink: String;
      HTitle: String;
    begin
      if ( S[ OffSet ] = '`' ) then
        Inc( OffSet );
      HLevel:= Copy2SymbEx( S, ',', Offset );
      HLink:= Copy2SymbEx( S, ',', Offset );
      HTitle:= Copy2SymbEx( S, ',', Offset );
      Result:= Format( '<h%0:s id="%1:s">%2:s</h%0:s><br />%3:s', [ HLevel, HLink, HTitle, ParseTags( S, Offset )]);
    end;

  begin
    if ( S[ OffSet ] = '`' ) then
      Inc( OffSet );
    Result:= '';
    repeat
      OldOffSet:= OffSet;
      Tag:= FindTag( S, OffSet, beforeTag );
      case lowercase( Tag ) of
        'br': Result:= Result + AddText + '@br'; //Replace them later
        'bold': Result:= Result + AddText + '<b>' +  ParseTags( S, OffSet ) + '</b>';
        'italic': Result:= Result + AddText + '<i>' +  ParseTags( S, OffSet ) + '</i>';
        'unorderedlist': Result:= Result + AddText + '<ul class="list">' + ParseTags( S, OffSet ) + '</ul>';
        'orderedlist': Result:= Result + AddText + '<ol class="list">' + ParseTags( S, OffSet ) + '</ol>';
        'item': Result:= Result + AddText + '<li>' + ParseTags( S, OffSet ) + '</li>';
        'section': Result:= Result + AddText + '<article>' + MakeSection( S, OffSet ) + '</article>';
        'code': Result:= Result + AddText + '<pre><code class="pascal">' + FindIdentifiers( ParseTags( S, OffSet )) + '</code></pre>';
        'link': Result:= Result + AddText + FindIdentifiers( ParseTags( S, OffSet ));
        'abbr':
          if ( Short ) then
            begin
              Result:= ParseTags( S, OffSet );
              exit;
            end
          else
            Result:= Result + AddText + ParseTags( S, OffSet );
        else
          begin
            if ( Tag = '`' ) then
              begin
                Result:= Result + AddText;
                Inc( OffSet );
                exit;
              end;
            if ( Tag = '' ) then
              begin
                OffSet:= Length( S ) + 1;
                Result:= Result + Copy( S, OldOffSet, OffSet - OldOffSet );
                break;
              end;
            Result:= Result + AddText + Tag;
          end;
      end;
    until False;
  end;

  function TextToHTML(Txt: string): string; //Modified some Code of CodeHelp.pas by Mattias Gaertner
  var
    p: Integer;
  begin
    Result:=Txt;
    p:=length(Result);
    while p>0 do
    begin
      case Result[p] of
      '<': Result:=copy(Result,1,p-1)+'&lt;'+copy(Result,p+1,length(Result));
      '>': Result:=copy(Result,1,p-1)+'&gt;'+copy(Result,p+1,length(Result));
      '&': Result:=copy(Result,1,p-1)+'&amp;'+copy(Result,p+1,length(Result));
      #10,#13:
        begin
          if (p>1) and (Result[p-1] in [#10,#13]) and (Result[p-1]<>Result[p]) then
            dec(p);
          Result:=copy(Result,1,p-1)+'@br '+copy(Result,p,length(Result));
        end;
      end;
      dec(p);
    end;
  end;

var
  Cursor: Integer;
begin
  Cursor:= 1;
  S:= TextToHTML( S );
  if ( S > '' ) then
    Result:= ReplaceStr( FindIdentifiersCode( ParseTags( S, Cursor )), '@br', '<br/>' )
  else
    Result:= '';
  //Result:= HighlightCodeHTML( S );
  //Result:= ReplaceStr( Result, '@br', '<br/>' );
end;



procedure ReadXMLFragment( AParentNode: TDOMNode; AStream: TStream);
var
  Parser: TDOMParser;
  Src: TXMLInputSource;
begin
  try
    Parser := TDOMParser.Create;
    Src := TXMLInputSource.Create(AStream);
    Parser.Options.PreserveWhitespace := True;
    Parser.ParseWithContext( Src, AParentNode, xaAppendAsChildren );
  finally
    Src.Free;
    Parser.Free;
  end;
end;

procedure ReadXMLFragmentStr( AParentNode: TDOMNode; Str: String );
var
  S: TStringStream;
begin
  try
    S:= TStringStream.Create( Str );
    ReadXMLFragment( AParentNode, S );
  except on e: Exception do
    begin
      WriteLn( 'ReadXMLFragmentError: ', e.Message, ' Fragment String:', Str );
    end;
  end;
  S.Free;
end;

procedure WriteHTML( Element: TDOMNode; const AFileName: String );
var
  F: TextFile;

  procedure WriteNode( Element: TDOMNode );
  var
    i: Integer;
  begin
    if ( not Assigned( Element )) then
      exit;
    if ( Element.NodeName = '#text' ) then
      Write( F, Element.TextContent )
    else
      begin
        Write( F, '<' + Element.NodeName );
        if ( Assigned( Element.Attributes )) then
          for i:= 0 to Element.Attributes.Length - 1 do
            Write( F, Format( ' %s="%s"', [ Element.Attributes[ i ].NodeName, Element.Attributes[ i ].NodeValue ]));

        if ( Element.ChildNodes.Count > 0 ) then
          begin
            Write( F, '>' );
            for i:= 0 to Element.ChildNodes.Count - 1 do
              WriteNode( Element.ChildNodes[ i ]);
            Write( F, '</' + Element.NodeName + '>' );
          end
        else
          case ( Element.NodeName ) of
            'br', 'img', 'link': Write( F, '>' );
          else
            Write( F, '></', Element.NodeName, '>' );
          end;
      end;
  end;

begin
  Assign( F, AFileName );
  Rewrite( F );
  Write( F, '<!DOCTYPE html>' );
  if ( Assigned( Element )) then
    WriteNode( Element.FindNode( 'html' ) );
  CloseFile( F );
end;

function AddTag( rt: TDOMElement; TagName: String ): TDOMElement;
begin
  Result:= rt.OwnerDocument.CreateElement( TagName );
  rt.AppendChild( Result );
end;


procedure XMLMergeInto( Dest: TDOMDocument; FileName: String );
  function CompareAttrib( Src, Dest: TDOMElement; Attrib: String ): Boolean;
  begin
    Result:= ( Src.GetAttribute( Attrib ) = Dest.GetAttribute( Attrib ));
  end;

  function CompareAttribs( Src, Dest: TDOMElement ): Boolean;
  var
    i: Integer;
  begin
    Result:= True;
    for i:= 0 to Src.Attributes.Length - 1 do
      if ( Src.Attributes[ i ].NodeName <> 'description' ) then
        if ( not CompareAttrib( Src, Dest, Src.Attributes[ i ].NodeName )) then
          begin
            Result:= False;
            break;
          end;
  end;

  function FindNode( Base: TDOMElement; Src: TDOMElement ): TDOMElement;
  var
    child: TDOMElement;
  begin
    child:= TDOMElement( Base.FirstChild );
    while ( Assigned( child )) do
      begin
        if (( child.NodeName = Src.NodeName ) and ( CompareAttribs( Src, child ))) then
          break;
        child:= TDOMElement( child.NextSibling );
      end;
    Result:= child; // child will be nil if not found
  end;

  procedure ImportNode( Src: TDOMElement; Dst: TDOMElement );
  var
    child: TDOMElement;
    dstChild: TDOMElement;
    i: Integer;
  begin
    //for i:= 0 to Src.Attributes.Length -1 do
    //Dst.SetAttribute( Src.Attributes[ i ].NodeName, Src.Attributes[ i ].NodeValue );
    if ( Src.hasAttribute( 'description' )) then
      begin
        Dst.SetAttribute( 'description', Src.GetAttribute( 'description' ));
        WriteLn( Dst.NodeName, '<', Dst.GetAttribute( 'name' ), '>.description --> ' + Src.GetAttribute( 'description' ));
      end;
    child:= TDOMElement( Src.FirstChild );
    while Assigned( child ) do
      begin
        dstChild:= FindNode( Dst, child );
        if ( not Assigned( dstChild )) then
          begin
            dstChild:= AddTag( Dst, Child.NodeName );
            WriteLn( '###', child.NodeName );
          end;
        ImportNode( child, dstChild );
        child:= TDOMElement( child.NextSibling );
      end;
  end;

var
  Src: TXMLDocument;
begin
  ReadXMLFile( Src, FileName );
  ImportNode( TDOMElement( Src.DocumentElement ), TDOMElement( Dest.DocumentElement ));
  WriteXML( Dest, 'tmp.xml' );
  Src.Free;
end;

function XMLListToSL( List: TList; const Qualifier: String = 'name' ): TStringList;
var
  i: Integer;
  Node: TDOMElement;
begin
  Result:= TStringList.Create;
  for i:= 0 to List.Count - 1 do
    begin
      Node:= TDOMElement( List[ i ]);
      if ( Node.hasAttribute( Qualifier )) then
        Result.Add( Node.GetAttribute( Qualifier ));
    end;
end;

function XMLListToSLEx( List: TList; const mask: String = '%name%'; FindIdent: TFindIdentifierFunc = nil ): TStringList;
var
  i: Integer;
  Node: TDOMElement;
  masklist: TStringList;
  j: Integer;
  repl: DOMString;
  str: String;
  IsCode: Boolean;
  ThisMask: String;
  IsShort: Boolean;

  procedure MakeMaskList;
  var
    i2: Integer;
    thismask: String;
  begin
    masklist:= TStringList.Create;

    i:= Pos( '%', mask );
    while i > 0 do
      begin
        i2:= PosEx( '%', mask, i + 1 );
        thismask:= Copy( mask, i + 1, i2 - i - 1 );
        if ( masklist.IndexOf( thismask ) = -1 ) then
          masklist.Add( thismask );
        i:= PosEx( '%', mask, i2 + 1 );
        //WriteLn( masklist[ masklist.Count - 1 ]);
      end;
  end;

begin
  MakeMaskList;
  Result:= TStringList.Create;
  for i:= 0 to List.Count - 1 do
    begin
      Node:= TDOMElement( List[ i ]);
      str:= mask;
      for j:= 0 to masklist.Count - 1 do
        begin
          IsCode:= masklist[ j ][ 1 ] = ':';
          IsShort:= masklist[ j ][ Length( masklist[ j ])] = ':';
          if ( IsCode or IsShort ) then
            ThisMask:= Copy( masklist[ j ], 1 + Ord( IsCode ), Length( masklist[ j ]) - Ord( IsCode ) - Ord( IsShort ))
          else
            ThisMask:= masklist[ j ];

          if ( Node.hasAttribute( ThisMask )) then
            repl:= Node.GetAttribute( ThisMask )
          else
            repl:= '';
          if ( IsCode ) then
            repl:= Pas2DocToHtml( repl, IsShort, FindIdent );
          str:= ReplaceStr( str, '%' + masklist[ j ] + '%', repl );
        end;
      Result.Add( str );
    end;
  masklist.Free;
end;

function SLToHtmlList( SL: TStringList ): String;
var
  i: Integer;
begin
  if ( SL.Count > 0 ) then
    begin
      Result:= '<ul class="list">' + LineEnding;
      for i:= 0 to SL.Count - 1 do
         Result+= '  <li>' + SL[ i ] + '</li>';
      Result+= '</ul>' + LineEnding;
    end
  else
    Result:= '';
end;

function SLToHtmlTable( SL: TStringList ): String;
var
  i: Integer;
begin
  if ( SL.Count > 0 ) then
    begin
      Result:= '<table class="list">' + LineEnding;
      for i:= 0 to SL.Count - 1 do
         Result+= '  <tr>' + SL[ i ] + '</tr>';
      Result+= '</table>' + LineEnding;
    end
  else
    Result:= '';
end;


function InitHtml( html: TXMLDocument; title: String ): TDOMElement;
var
  head: TDOMElement;
  tag: TDOMElement;
begin
  Result:= html.CreateElement( 'html' );
  html.AppendChild( Result );
  head:= AddTag( Result, 'head' );
  tag:= AddTag( head, 'link' );
  tag.SetAttribute( 'rel', 'stylesheet' );
  tag.SetAttribute( 'href', 'doc.css' );

  tag:= AddTag( head, 'link' );
  tag.SetAttribute( 'rel', 'stylesheet' );
  tag.SetAttribute( 'href', 'tomorrow.css' );

  tag:= AddTag( head, 'script' );
  tag.SetAttribute( 'src', 'highlight.pack.js' );

  tag:= AddTag( head, 'script' );
  tag.SetAttribute( 'src', 'highlightjs-line-numbers.min.js' );

  tag:= AddTag( head, 'script' );
  tag.TextContent:= 'hljs.initHighlightingOnLoad();hljs.initLineNumbersOnLoad();';

  AddTag( head, 'title' ).TextContent:= title;
end;

function SymbolToText( Symbols: String ): String;
var
  i: Integer;
begin
 Result:= '';
  for i:= 1 to Length( Symbols ) do
    case Symbols[ i ] of
      ':': Result+= 'colon';
      '/': Result+= 'slash';
      '*': Result+= 'star';
      '+': Result+= 'plus';
      '-': Result+= 'minus';
      '=': Result+= 'equal';
      '&': Result+= 'and';
      '%': Result+= 'percent';
      '$': Result+= 'dollar';
      '@': Result+= 'at';
      '\': Result+= 'backslash';
      '(': Result+= 'bracketl';
      ')': Result+= 'bracketr';
      '[': Result+= 'sqbracketl';
      ']': Result+= 'sqbracketr';
      '>': Result+= 'greater';
      '<': Result+= 'less';
      else
        Result+= Symbols[ i ];
    end;
end;

{ TDocPackage }

procedure TDocPackage.SetPackageNode(AValue: TDOMElement);
begin
  if FPackageNode=AValue then Exit;
  FPackageNode:=AValue;
  if ( Assigned( PackageNode )) then
    Name:= PackageNode.GetAttribute( 'name' );
end;

procedure TDocPackage.SetXML(AValue: TXMLDocument);
begin
  if FXML=AValue then Exit;
  FXML:=AValue;

  PackageNode:= TDOMElement( XML.FindNode( 'package' ));
end;

constructor TDocPackage.Create( AFileName: String; AOutDir: String;
  ADictionary: TStringList );
begin
  inherited Create;
  FUnits:= TDocUnitList.Create;
  FFileName:= AFileName;
  FOutDir:= AOutDir;
  ReadXMLFile( FXML, FileName );
  PackageNode:= TDOMElement( XML.FindNode( 'package' ));

  Dictionary:= ADictionary;
end;

destructor TDocPackage.Destroy;
var
  i: Integer;
begin
  for i:= 0 to FUnits.Count - 1 do
    FUnits[ i ].Free;
  FUnits.Free;
  FXML.Free;
  inherited Destroy;
end;

function TDocPackage.ListOfAll(Name: String; const Base: TDOMElement): TList;
var
  i: Integer;
  node: TDOMElement;
begin
  if ( Assigned( Base )) then
    node:= Base
  else if ( not Assigned( PackageNode )) then
    exit
  else
    node:= PackageNode;
  Result:= TList.Create;
  for i:= 0 to node.ChildNodes.Count - 1 do
    if ( node.ChildNodes[ i ].NodeName = Name ) then
      Result.Add( node.ChildNodes[ i ]);
end;

function TDocPackage.ParseDependencies(OutFile: String; Target: TDOMElement;
  const Src: TDOMElement): Boolean;
var
  ls: TList;
  sl: TStringList;
  i: Integer;
  s: TStringStream;
  str: String;
begin
  ls:= ListOfAll( 'file', Src );

  sl:= XMLListToSL( ls );
  ls.Free;
  str:= SLToHtmlList( sl );
  Result:= str > '';
  if ( Result ) then
    ReadXMLFragmentStr( Target, str );
  sl.Free;
end;

function TDocPackage.ParseUnits(OutFile: String; Target: TDOMElement;
  const Src: TDOMElement): Boolean;
var
  i: Integer;
  ul: TDOMElement;
begin
  Result:= Units.Count > 0;
  if ( not Result ) then
    Exit;
  ul:= AddTag( Target, 'ul' );
  for i:= 0 to Units.Count - 1 do
    begin
      AddTag( ul, 'li' ).TextContent:= Format( '<a href="%0:s.%1:s.unit.html">%1:s</a>', [ Name, Units[ i ].Name ]);
      WriteLn( Name, '.', Units[ i ].Name );
    end;
end;

procedure TDocPackage.PopulateDictionary;
var
  ls: TList;
  sl: TStringList;
  i: Integer;
begin
  ls:= ListOfAll( 'file', PackageNode );
  sl:= XMLListToSL( ls );
  Dictionary.Add( Name + '=' + Name + '.package.html' );
  for i:= 0 to sl.Count -1 do
    begin
      Units[ Units.Add( TDocUnit.Create( ExtractFilePath( FileName ) + Name + '.' + sl[ i ] + '.unit.xml', OutDir, Dictionary, Self ))].PopulateDictionary;
      WriteLn( sl[ i ], ':', Units[ i ].Name );
    end;
  sl.Free;
  ls.Free;
end;

procedure TDocPackage.WriteDocFiles( FinishedFiles: TStringList );
var
  idx: Integer;
  html: TXMLDocument;
  CurrentTag: TDOMElement;
  CurrentArt: TDOMElement;
  deps: TDOMElement;
  OutFile: String;
  i: Integer;
begin
  OutFile:= OutDir + ExtractFileNameOnly( FileName );
  if ( FinishedFiles.Find( OutFile, idx )) then
    exit;

  html:= TXMLDocument.Create;
  CurrentTag:= InitHtml( html, 'Documentation of ' + Name );
  CurrentTag:= AddTag( CurrentTag, 'body' );

  AddTag( CurrentTag, 'header' ).TextContent:= 'Description of Package ' + Pas2DocToHtml( '@link`'+ Name + '`', False, @FindIdentifier  );

  CurrentTag:= AddTag( CurrentTag, 'section' );
  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Dependencies of ' + Name;
  AddTag( CurrentArt, 'br' );
  deps:= TDOMElement( PackageNode.FindNode( 'dependencies' ));
  if ( Assigned( deps ) and ParseDependencies( OutFile, CurrentArt, deps )) then

  else
    AddTag( CurrentArt, 'span' ).TextContent:= 'This package has no dependencies.';

  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Units in ' + Name;
  AddTag( CurrentArt, 'br' );
  if ( not ParseUnits( OutFile, CurrentArt )) then
    AddTag( CurrentArt, 'span' ).TextContent:= 'This package has no units.';

  WriteHTML( html, OutFile + '.html' );
  html.Free;
  FinishedFiles.Add( OutFile );
  for i:= 0 to Units.Count - 1 do
    Units[ i ].WriteDocFiles( FinishedFiles );
end;

function TDocPackage.FindIdentifier(AName: String): String;
var
  S: String;
  idx: Integer;
begin
  Result:= '';
  idx:= Dictionary.IndexOfName( AName );
  if ( idx > -1 ) then
    Result:= Dictionary.ValueFromIndex[ idx ]
  else
    begin
      S:= Name + '.' + AName;
      idx:= Dictionary.IndexOfName( S );
      if ( idx > -1 ) then
        Result:= Dictionary.ValueFromIndex[ idx ]
      else
        begin
          S:= Name + '.' + AName;
          idx:= Dictionary.IndexOfName( S );
          if ( idx > -1 ) then
            Result:= Dictionary.ValueFromIndex[ idx ]
        end;
    end;
end;


procedure TDocUnit.SetXML(AValue: TXMLDocument);
begin
  if FXML=AValue then Exit;
  FXML:=AValue;

  UnitNode:= TDOMElement( XML.FindNode( 'unit' ));
end;


procedure TDocUnit.ParseClass(OutFile: String; ClassNode: TDOMElement; FinishedFiles: TStringList );
var
  html: TXMLDocument;
  CurrentTag: TDOMElement;
  CurrentArt: TDOMElement;
  methodsArt: TDOMElement;
  propertiesArt: TDOMElement;
  content: DOMString;

  procedure CleanEmptySections;
    procedure CleanEmpty( Base: TDOMElement );
    var
      i: Integer;
    begin
      //WriteLn( Base.NodeName );
      if (( Base.NodeName = 'article' ) or ( Base.NodeName = 'section' )) then
        begin
          for i:= Base.ChildNodes.Count - 1 downto 1 do
            if ( Base.ChildNodes[ i ].ChildNodes.Count <= 1 ) then
              Base.DetachChild( Base.ChildNodes[ i ]).Free
            else
              CleanEmpty( TDOMElement( Base.ChildNodes[ i ]));
          if ( Base.ChildNodes.Count < 2 ) then
            Base.ParentNode.DetachChild( Base ).Free;
        end;
    end;
  begin
    CleanEmpty( methodsArt );
    CleanEmpty( propertiesArt );
  end;

  procedure ParseSections;
  var
    ls: TList;
    i: Integer;
    Art: TDOMElement;
  begin
    ls:= ListOfAll( 'section', ClassNode );
    for i:= 0 to ls.Count - 1 do
      case ( TDOMElement( ls[ i ]).GetAttribute( 'visibility' )) of
        'public', 'published':
          begin
            Art:= AddTag( methodsArt, 'article' );
            AddTag( Art, 'h2' ).TextContent:= TDOMElement( ls[ i ]).GetAttribute( 'visibility' );
            ParseProcedures( OutFile, FinishedFiles, Art, TDOMElement( ls[ i ]));

            Art:= AddTag( propertiesArt, 'article' );
            AddTag( Art, 'h2' ).TextContent:= TDOMElement( ls[ i ]).GetAttribute( 'visibility' );
            ParseProperties( OutFile, Art, TDOMElement( ls[ i ]));
          end;
      end;
    ls.Free;
  end;

begin
  if ( FinishedFiles.IndexOf( OutFile ) > -1 ) then
    exit;

  html:= TXMLDocument.Create;

  ActiveElementPostFix:= ClassNode.GetAttribute( 'name' );

  CurrentTag:= InitHtml( html, 'Documentation of ' + GetScope );
  CurrentTag:= AddTag( CurrentTag, 'body' );
  AddTag( CurrentTag, 'header' ).TextContent:= 'Description of ' + Pas2DocToHtml( GetFullScopeLnk, False, @FindIdentifier  );;

  CurrentArt:= AddTag( CurrentTag, 'article' );
  content:= Pas2DocToHtml( ClassNode.GetAttribute( 'description' ), False, @FindIdentifier );
  if ( content > '' ) then
    ReadXMLFragmentStr( AddTag( CurrentArt, 'span' ), content );

  methodsArt:= AddTag( CurrentArt, 'section' );
  AddTag( methodsArt, 'h1' ).TextContent:= 'Methods<br>';
  propertiesArt:= AddTag( CurrentArt, 'section' );
  AddTag( propertiesArt, 'h1' ).TextContent:= 'Properties<br>';

  ParseSections;
  CleanEmptySections;

  WriteHTML( html, OutFile + '.html' );
  html.Free;
  FinishedFiles.Add( OutFile );
  ActiveElementPostFix:= '';
end;

procedure TDocUnit.ParseProc(OutFile: String; ProcNode: TDOMElement; FinishedFiles: TStringList );
var
  html: TXMLDocument;
  CurrentTag: TDOMElement;
  CurrentArt: TDOMElement;
  content: String;
begin
  if ( FinishedFiles.IndexOf( OutFile ) > -1 ) then
    exit;

  html:= TXMLDocument.Create;

  CurrentTag:= InitHtml( html, 'Documentation of ' + GetScope + ProcNode.GetAttribute( 'name' ));
  CurrentTag:= AddTag( CurrentTag, 'body' );
  AddTag( CurrentTag, 'header' ).TextContent:= 'Description of ' + GetScope + ProcNode.GetAttribute( 'name' );

  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Definition';
  AddTag( CurrentArt, 'br' );

  content:= Pas2DocToHtml( ProcNode.GetAttribute( 'definition' ), False, @FindIdentifier );
  if ( content > '' ) then
    ReadXMLFragmentStr( AddTag( CurrentArt, 'span' ), content );

  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Description';
  AddTag( CurrentArt, 'br' );
  content:= Pas2DocToHtml( ProcNode.GetAttribute( 'description' ), False, @FindIdentifier );
  if ( content > '' ) then
    ReadXMLFragmentStr( AddTag( CurrentArt, 'span' ), content );

  WriteHTML( html, OutFile + '.html' );
  html.Free;
  FinishedFiles.Add( OutFile );
end;

procedure TDocUnit.ParseProcs(OutFile: String; ProcNodes: TList;
  FinishedFiles: TStringList);
var
  html: TXMLDocument;
  CurrentTag: TDOMElement;
  CurrentArt: TDOMElement;
  content: String;
  ProcNode: TDOMElement;
  i: Integer;
begin
  if ( FinishedFiles.IndexOf( OutFile ) > -1 ) then
    exit;

  html:= TXMLDocument.Create;

  CurrentTag:= InitHtml( html, 'Documentation of ' + GetScope + TDOMElement( ProcNodes[ 0 ]).GetAttribute( 'name' ));
  CurrentTag:= AddTag( CurrentTag, 'body' );

  AddTag( CurrentTag, 'header' ).TextContent:= 'Description of ' + GetScope + TDOMElement( ProcNodes[ 0 ]).GetAttribute( 'name' );

  for i:= 0 to ProcNodes.Count -1 do
    begin
      ProcNode:= TDOMElement( ProcNodes[ i ]);
      CurrentArt:= AddTag( CurrentTag, 'article' );
      AddTag( CurrentArt, 'h1' ).TextContent:= 'Definition';
      AddTag( CurrentArt, 'br' );

      content:= Pas2DocToHtml( ProcNode.GetAttribute( 'definition' ), False, @FindIdentifier );
      if ( content > '' ) then
        ReadXMLFragmentStr( AddTag( CurrentArt, 'span' ), content );

      CurrentArt:= AddTag( CurrentTag, 'article' );
      AddTag( CurrentArt, 'h1' ).TextContent:= 'Description';
      AddTag( CurrentArt, 'br' );
      content:= Pas2DocToHtml( ProcNode.GetAttribute( 'description' ), False, @FindIdentifier );
      if ( content > '' ) then
        ReadXMLFragmentStr( AddTag( CurrentArt, 'span' ), content );
    end;

  WriteHTML( html, OutFile + '.html' );
  html.Free;
  FinishedFiles.Add( OutFile );
end;

procedure TDocUnit.FindUsedUnits(Src: TDOMElement);
var
  usesSect: TDOMNode;
  ANode: TDOMNode;
begin
  usesSect:= Src.FindNode( 'uses' );
  if ( not Assigned( usesSect )) then
    exit;
  ANode:= usesSect.FirstChild;
  while Assigned( ANode ) do
    begin
      UsedUnits.Add( TDOMElement( ANode ).GetAttribute( 'name' ));
      ANode:= ANode.NextSibling;
    end;
end;


procedure TDocUnit.SetUnitNode(AValue: TDOMElement);
begin
  if FUnitNode=AValue then Exit;
  FUnitNode:=AValue;
  if ( Assigned( UnitNode )) then
    Name:= UnitNode.GetAttribute( 'name' );
end;

function TDocUnit.FullScope(const WithPackage: Boolean): String;
var
  PkgStr: String;
begin
  if ( WithPackage and Assigned( Package )) then
    PkgStr:= Package.Name + '.'
  else
    PkgStr:= '';
  Result:= PkgStr + Name  + '.' + ActiveElementPostFix;
end;

function TDocUnit.GetScope: String;
begin
  Result:= FullScope();
end;

function TDocUnit.GetFullScopeLnk: String;
begin
  if ( Assigned( Package )) then
    Result:= '@link`' + Package.Name + '`.@link`' + Name  + '`.@link`' + ActiveElementPostFix + '`'
  else
    Result:= '@link`' + Name  + '`.@link`' + ActiveElementPostFix + '`';
end;

function TDocUnit.ListOfAll(Name: String; const Base: TDOMElement): TList;
var
  i: Integer;
  node: TDOMElement;
begin
  if ( Assigned( Base )) then
    node:= Base
  else if ( not Assigned( UnitNode )) then
    exit
  else
    node:= UnitNode;
  Result:= TList.Create;
  for i:= 0 to node.ChildNodes.Count - 1 do
    if ( node.ChildNodes[ i ].NodeName = Name ) then
      if ( Pos( '@exclude', TDOMElement( node.ChildNodes[ i ]).GetAttribute( 'description' )) = 0 ) then
        Result.Add( node.ChildNodes[ i ]);
end;

function TDocUnit.FindDuplicates(Itm: TDOMElement; Base: TDOMElement): TList;
var
  node: TDOMElement;
  i: Integer;
begin
  if ( Assigned( Base )) then
    node:= Base
  else if ( not Assigned( UnitNode )) then
    exit
  else
    node:= UnitNode;
  Result:= TList.Create;
  for i:= 0 to node.ChildNodes.Count - 1 do
    if (( node.ChildNodes[ i ].NodeName = Itm.NodeName ) AND
        ( TDOMElement( node.ChildNodes[ i ]).GetAttribute( 'name' ) = Itm.GetAttribute( 'name' ))) then
      if ( Pos( '@exclude', TDOMElement( node.ChildNodes[ i ]).GetAttribute( 'description' )) = 0 ) then
        Result.Add( node.ChildNodes[ i ]);
end;

procedure TDocUnit.ParseProcedures(OutFile: String; FinishedFiles: TStringList; Target: TDOMElement;
  const Src: TDOMElement);
var
  ls: TList;
  sl: TStringList;
  i: Integer;
  lsdup: TList;
  ident: String;
begin
  ls:= ListOfAll( 'procedure', Src );

  sl:= XMLListToSL( ls );
  if ( ls.Count = sl.Count ) then
    for i:= 0 to sl.Count -1 do
      begin
        lsdup:= FindDuplicates( TDOMElement( ls[ i ]), Src );
        ident:= SymbolToText( sl[ i ]);

        if ( lsdup.Count > 1 ) then
          ParseProcs( OutFile + '.' + ident, lsdup, FinishedFiles )
        else if ( TDOMElement( ls[ i ]).GetAttribute( 'type' ) <> 'operator' ) then
          ParseProc( OutFile + '.' + ident, TDOMElement( ls[ i ]), FinishedFiles );
        lsdup.Free;
      end;
  sl.Free;

  sl:= XMLListToSLEx( ls, '<td><pre><code class="pascal">%:definition%</code></pre></td><td><div class="description">%:description:%</div></td>', @FindIdentifier );
  ls.Free;
  ReadXMLFragmentStr( Target, SLToHtmlTable( sl ));
  sl.Free;
end;

procedure TDocUnit.ParseClasses(OutFile: String; FinishedFiles: TStringList;
  Target: TDOMElement; const Src: TDOMElement);
var
  ls: TList;
  i: Integer;
  sl: TStringList;
begin
  ls:= ListOfAll( 'class', Src );

  sl:= XMLListToSL( ls );
  for i:= 0 to sl.Count -1 do
    ParseClass( OutFile + '.' + sl[ i ], TDOMElement( ls[ i ]), FinishedFiles );
  sl.Free;

  sl:= XMLListToSLEx( ls, '<td><a href="' + ExtractFileName( OutFile ) + '.%name%.html">%name%</a></td><td><span class="description" align="right">%:description:%</span></td>', @FindIdentifier );
  ls.Free;
  ReadXMLFragmentStr( Target, SLToHtmlTable( sl ));
  sl.Free;
end;

procedure TDocUnit.ParseProperties(OutFile: String; Target: TDOMElement;
  const Src: TDOMElement);
var
  sl: TStringList;
  ls: TList;
begin
  ls:= ListOfAll( 'property', Src );
  sl:= XMLListToSLEx( ls, '<td><pre><code class="pascal">%:definition%</code></pre></td><td><span class="description" align="right">%:description:%</span></td>', @FindIdentifier );

  ls.Free;
  ReadXMLFragmentStr( Target, SLToHtmlTable( sl ));
  sl.Free;
end;

constructor TDocUnit.Create(AFileName: String; AOutDir: String;
  ADictionary: TStringList; APackage: TDocPackage);
var
  ImpXML: String;
begin
  inherited Create;

  FFileName:= AFileName;
  Dictionary:= ADictionary;
  Package:= APackage;
  FOutDir:= AOutDir;
  FUsedUnits:= TStringList.Create;

  ReadXMLFile( FXML, FileName );
  ImpXML:= ExtractFileName( AFileName ) + '.doc.xml';
  if ( FileExists( ExpandFileName( ImpXML ))) then
    XMLMergeInto( XML, ImpXML );
  UnitNode:= TDOMElement( XML.FindNode( 'unit' ));
end;

destructor TDocUnit.Destroy;
begin
  XML.Free;
  FUsedUnits.Free;
  inherited Destroy;
end;

procedure TDocUnit.PopulateDictionary;
var
  ls: TList;
  i: Integer;
  procname: DOMString;
begin
  Dictionary.Add( Format( '%1:s=%0:s.%1:s.unit.html', [ Package.Name, Name ]));
  ls:= ListOfAll( 'class' );
  for i:= 0 to ls.Count - 1 do
    begin
      Dictionary.Add( Format( '%1:s.%2:s=%0:s.%1:s.unit.%2:s.html', [ Package.Name, Name, TDOMElement( ls[ i ]).GetAttribute( 'name' )]));
      PopulateClassDictionary( TDOMElement( ls[ i ]));
    end;
  ls.Free;

  ls:= ListOfAll( 'procedure' );
  for i:= 0 to ls.Count - 1 do
    begin
      if ( TDOMElement( ls[ i ]).GetAttribute( 'type' ) = 'operator' ) then
        procname:= SymbolToText( TDOMElement( ls[ i ]).GetAttribute( 'name' ))
      else
        procname:= TDOMElement( ls[ i ]).GetAttribute( 'name' );
      WriteLn( Format( '%1:s.%2:s=%0:s.%1:s.unit.%2:s.html', [ Package.Name, Name, procname ]));
      Dictionary.Add( Format( '%1:s.%2:s=%0:s.%1:s.unit.%2:s.html', [ Package.Name, Name, procname ]));
    end;
  ls.Free;
end;

procedure TDocUnit.PopulateClassDictionary(Base: TDOMElement);
var
  ls: TList;
  i: Integer;
  CName: String;

  procedure AppendList( Name: String; Section: String );
  var
    CurNode: TDOMElement;
    ls2: TList;
  begin
    CurNode:= TDOMElement( Base.FirstChild );
    if ( not Assigned( CurNode )) then
      exit;
    while ( Assigned( CurNode ) and ( CurNode.GetAttribute( 'visibility' ) <> Section )) do
      CurNode:= TDOMElement( CurNode.NextSibling );
    if ( not Assigned( CurNode )) then
      exit;
    ls2:= ListOfAll( Name, CurNode );
    ls.AddList( ls2 );
    ls2.Free;
  end;

begin
  ls:= TList.Create;
  AppendList( 'procedure', 'public' );
  AppendList( 'procedure', 'published' );
  AppendList( 'property', 'public' );
  AppendList( 'property', 'published' );
  AppendList( 'variable', 'public' );
  AppendList( 'variable', 'published' );
  CName:= Base.GetAttribute( 'name' );
  for i:= 0 to ls.Count - 1 do
    Dictionary[ Dictionary.Add( Format( '%1:s.%2:s.%3:s=%0:s.%1:s.unit.%2:s.%3:s.html', [ Package.Name, Name, CName, TDOMElement( ls[ i ]).GetAttribute( 'name' )]))];
  ls.Free;
end;

procedure TDocUnit.WriteDocFiles(FinishedFiles: TStringList);
var
  idx: Integer;
  html: TXMLDocument;
  CurrentArt: TDOMElement;
  CurrentTag: TDOMElement;
  OutFile: String;
  content: String;

begin
  if ( FinishedFiles.Find( FileName, idx )) then
    exit;

  ActiveElementPostFix:= '';
  OutFile:= OutDir + ExtractFileNameOnly( FileName );
  html:= TXMLDocument.Create;
  CurrentTag:= InitHtml( html, 'Documentation of ' + GetScope );;
  CurrentTag:= AddTag( CurrentTag, 'body' );

  AddTag( CurrentTag, 'header' ).TextContent:= 'Description of Unit ' + Pas2DocToHtml( GetFullScopeLnk, False, @FindIdentifier  );

  content:= Pas2DocToHtml( UnitNode.GetAttribute( 'description' ), False, @FindIdentifier );
  if ( content > '' ) then
    ReadXMLFragmentStr( AddTag( CurrentTag, 'article' ), content );

  FindUsedUnits( UnitNode );

  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Used Units of ' + Name;
  AddTag( CurrentArt, 'br' );
  ReadXMLFragmentStr( CurrentArt, SLToHtmlList( UsedUnits ));

  CurrentTag:= AddTag( CurrentTag, 'section' );
  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Classes in ' + Name;
  AddTag( CurrentArt, 'br' );
  ParseClasses( OutFile, FinishedFiles, CurrentArt );

  CurrentArt:= AddTag( CurrentTag, 'article' );
  AddTag( CurrentArt, 'h1' ).TextContent:= 'Procedures / Functions in ' + Name;
  AddTag( CurrentArt, 'br' );
  ParseProcedures( OutFile, FinishedFiles, CurrentArt );

  WriteHTML( html, OutFile + '.html' );
  html.Free;
  FinishedFiles.Add( FileName );
end;

function TDocUnit.FindIdentifier(AName: String): String;
var
  S: String;
  idx: Integer;
begin
  Result:= '';

  idx:= Dictionary.IndexOfName( AName );
  if ( idx > -1 ) then
    Result:= Dictionary.ValueFromIndex[ idx ]
  else
    begin
      S:= GetScope() + '.' + AName;
      idx:= Dictionary.IndexOfName( S );
      if ( idx > -1 ) then
        Result:= Dictionary.ValueFromIndex[ idx ]
      else
        begin
          S:= Name + '.' + AName;
          idx:= Dictionary.IndexOfName( S );
          if ( idx > -1 ) then
            Result:= Dictionary.ValueFromIndex[ idx ]
        end;
    end;
end;

procedure ParseDocFiles( InDir: String; OutDir: String );
var
  SPackages: TStringList;
  SUnits: TStringList;
  Dictionary: TStringList;
  ScannedFiles: TStringList;
  i: Integer;

begin
  OutDir:= AppendPathDelim( OutDir );
  SPackages:= FindAllFiles( InDir, '*.package.xml', False );

  SUnits:= FindAllFiles( InDir, '*.unit.xml', False );

  ScannedFiles:= TStringList.Create;
  Dictionary:= TStringList.Create;
  Dictionary.Duplicates:= dupIgnore;

  for i:= 0 to SPackages.Count - 1 do
    with ( TDocPackage.Create( SPackages[ i ], OutDir, Dictionary )) do
      begin
        PopulateDictionary;
        WriteDocFiles( ScannedFiles );
        Free;
      end;

  //for i:= 0 to SUnits.Count - 1 do
  //  TDocUnit.Create( SUnits[ i ], OutDir, ScannedFiles ).Free;

  WriteLn( Dictionary.Text );

  SPackages.Free;
  SUnits.Free;
  ScannedFiles.Free;
  Dictionary.Free;
end;


end.

