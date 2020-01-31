unit p3dparsepascal;

{$mode objfpc}{$H+}


interface
  uses
    Classes,
    SysUtils,
    FileProcs,
    CodeCache,
    CodeToolManager,
    BasicCodeTools,
    DefineTemplates,
    FileUtil,
    strutils,
    FindDeclarationTool,
    CodeTree,
    PascalParserTool, IdentCompletionTool,
    DOM,
    XMLWrite,
    XMLRead;

  type

    { TP3DPascalParser }

    {Generic Parser base class}
    TP3DPascalParser = class ( TPersistent )
      private
        FDefinition: String;
        FDescription: String;
        FDOMNode: TDOMElement;
        FDOMNodeName: String;
        FName: String;
        FNode: TCodeTreeNode;
        FNodeType: String;

      public
        constructor Create( XMLParent: TDOMElement; ANode: TCodeTreeNode );

        procedure ReadProperties; virtual; abstract;
        function CreateXMLSection(AName: String; ParentNode: TDOMElement): TDOMElement;
        procedure WriteXML; virtual;
        procedure ReadComment;
        procedure WriteAttribute( Name, Value: String );
        property DOMNodeName: String read FDOMNodeName write FDOMNodeName;

      published
        property Node: TCodeTreeNode read FNode write FNode;
        property NodeType: String read FNodeType;
        property Name: String read FName;
        property Definition: String read FDefinition;
        property Description: String read FDescription;
        property DOMNode: TDOMElement read FDOMNode;
    end;
    { TP3DPascalParserUnit }

    {Parser for units.
    Will automatically call other parsers for sections}
    TP3DPascalParserUnit = class ( TP3DPascalParser )
      private
        FActNode: TCodeTreeNode;

      public
        constructor Create( FileName: String; XML: TXMLDocument );
        destructor Destroy; override;

        procedure ReadProperties; override;
        procedure ReadInterfaceSection;
        procedure WriteXML; override;

        //property Code: TCodeBuffer read FCode;
        property ActNode: TCodeTreeNode read FActNode;
    end;

    { TP3DPascalParserInterfaceSection }

    TP3DPascalParserInterfaceSection = class ( TP3DPascalParser )
      private
        FActNode: TCodeTreeNode;

      public
        procedure ReadProperties; override;
        procedure ReadInterfaceSection;
        procedure ReadUsesSection;
        procedure ReadDefinition;
        procedure ReadClass;
        procedure WriteXML; override;

        property ActNode: TCodeTreeNode read FActNode;
    end;
    { TP3DPascalParserDefinition }

    {Parser for pascal definitions}
    TP3DPascalParserDefinition = class ( TP3DPascalParser )
      public
        procedure ReadProperties; override;
        procedure ReadProcedure;
        procedure ReadVariable;
        procedure ReadConst;
        procedure ReadType;
        procedure ReadProperty;
        procedure ReadGeneric;
        procedure WriteXML; override;
    end;

    { TP3DPascalParserDefinitionClass }

    {Parser for classes and objects}
    TP3DPascalParserDefinitionClass = class ( TP3DPascalParser )
      private
        FActNode: TCodeTreeNode;
        FParentClass: String;

       public
        procedure ReadProperties; override;
        procedure ReadClassMember;
        procedure ReadClassDefinition(Section: TDOMElement);
        procedure WriteXML; override;

        property ParentClass: String read FParentClass;
        property ActNode: TCodeTreeNode read FActNode;
    end;

  //Init Pascal Reader
  procedure P3DParsePascalInit;
  //Finish Pascal Reader
  procedure P3DParsePascalFinish;

  procedure ParsePascalUnit( FileName: String; OutFile: String );
  procedure ParsePascalPackage( FileName: String; OutFile: String );

  const
    ConfigFilename = 'codetools.config'; //Filename of CodeTools config

  var
    {Code tool}
    Tool: TCodeTool;


implementation

procedure P3DParsePascalInit;
begin
  CodeToolBoss.SimpleInit( ConfigFilename );
end;

procedure P3DParsePascalFinish;
begin

end;

function GetDefinition( Node: TCodeTreeNode ): String;
var
  Caret: TCodeXYPosition;
  NewTopLine, X, Y: integer;
  Code: TCodeBuffer;
  CodeContexts: TCodeContextInfo;
  CurNode: TCodeTreeNode;
begin
  //Result:= Tool.ExtractCode( Node.StartPos, Node.EndPos, []);
  //Node:= Node.GetFindContextParent;
  //Caret:= CodeToolBoss.Positions.Items[ Node.StartPos ]^;
  Tool.JumpToNode( Node, Caret, NewTopLine, True );
  Code:= nil;
  CurNode:= Node.FirstChild;
  while Assigned( CurNode ) do
    begin
    //if ( not CodeToolBoss.FindDeclaration( Caret.Code, Caret.X, Caret.Y, Code, X, Y, NewTopLine ) or
    //  (Code = nil)) then
      Result:= //Tool.ExtractCode( CurNode.StartPos, CurNode.StartPos + 1, []);
               Tool.ExtractNode( CurNode, []);
      CurNode:= CurNode.NextBrother;
    end;
    //else
    //  Result:= Format( '[%s(%d,%d)|%s]', [ Code.Filename, X, Y, Tool.ExtractCode( Node.StartPos, Node.EndPos, [])]);
  //Result:= Tool.ExtractCode( Node.StartPos, Node.EndPos, []);
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

function GetPasDocCommentsAsHTML(Tool: TFindDeclarationTool;
  Node: TCodeTreeNode): string;
var
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodeXYPos, LastCodeXYPos: PCodeXYPosition;
  CommentCode: TCodeBuffer;
  CommentStart: integer;
  NestedComments: Boolean;
  CommentStr, LastComment: String;

  procedure AddComment;
  begin
    if (( not Assigned( CodeXYPos ) or ( not Assigned( LastCodeXYPos )) or
        ( CodeXYPos^.Code <> LastCodeXYPos^.Code ) or
        ( CodeXYPos^.Y - LastCodeXYPos^.Y > 10 ))) then
      begin
        // the last comment is at a different position => add a source link
        if ( LastComment <> '' ) then
          Result:= Result + Trim( DelChars( DelChars( LastComment, #10 ), #13 )) //TextToHTML(LastComment)
            +'@br ';
        LastComment:= Trim( CommentStr );
      end
    else
      begin
        // these two comments are very near together => combine them
        if ( LastComment <> '' ) then
          LastComment += '@br ';
        LastComment += CommentStr;
      end;
    LastCodeXYPos:= CodeXYPos;
  end;

begin
  Result:='';
  if (( not Assigned( Tool )) or ( not Assigned( Node ))) then
    exit;

  ListOfPCodeXYPosition:= nil;
  try
    if ( not Tool.GetPasDocComments( Node, ListOfPCodeXYPosition )) then
      exit;

    if ( not Assigned( ListOfPCodeXYPosition )) then
      exit;

    NestedComments:= Tool.Scanner.NestedComments;
    LastCodeXYPos:= nil;
    LastComment:= '';
    for i := 0 to ListOfPCodeXYPosition.Count - 1 do
      begin
        CodeXYPos:= PCodeXYPosition( ListOfPCodeXYPosition[ i ]);
        CommentCode:= CodeXYPos^.Code;
        CommentCode.LineColToPosition( CodeXYPos^.Y, CodeXYPos^.X, CommentStart );
        if (( CommentStart < 1 ) or ( CommentStart > CommentCode.SourceLength )) then
          continue;
        CommentStr:= ExtractCommentContent( CommentCode.Source,CommentStart,
                                            NestedComments, True, True, True );
        AddComment;
      end;

    CommentStr:= '';
    CodeXYPos:= nil;
    AddComment;

  finally
    FreeListOfPCodeXYPosition( ListOfPCodeXYPosition );
  end;
end;

{ TP3DPascalParserInterfaceSection }


procedure TP3DPascalParserInterfaceSection.ReadProperties;
begin
  case Node.Desc of
    ctnGenericType: DOMNodeName:= 'generic';
  else
    DOMNodeName:= 'interface';
  end;
  {        ctnTypeType, ctnTypeSection,
        ctnVarSection, ctnConstSection:
  }
end;

procedure TP3DPascalParserInterfaceSection.WriteXML;
begin
  FActNode:= Node.FirstChild;
  while ( Assigned( ActNode )) do
    begin
      case ActNode.Desc of
        ctnClass,ctnObject,ctnRecordType:
          if (( ActNode.SubDesc and ctnsForwardDeclaration ) = 0 ) then
            ReadDefinition;
        ctnProcedure,ctnProcedureHead,
          ctnClassOfType,ctnVariantType,
          ctnConstant, ctnConstDefinition,
          ctnVarDefinition:
          ReadDefinition;
        ctnTypeDefinition, ctnGenericType:
          if ( ActNode.FirstChild.Desc in [ ctnClass, ctnRecordType, ctnObject ]) then
            begin
              if (( ActNode.FirstChild.SubDesc and ctnsForwardDeclaration ) = 0 ) then
                ReadClass;
            end
          else
            ReadDefinition;
        ctnIdentifier, ctnOpenArrayType, ctnRangedArrayType,
        ctnSetType, ctnRangeType, ctnEnumerationType,
        ctnEnumIdentifier, ctnLabel, ctnFileType, ctnPointerType:
          ActNode.WriteDebugReport( '#', True );
        ctnTypeType, ctnTypeSection,
        ctnVarSection, ctnConstSection:
          ReadInterfaceSection;
        //ctnGenericType:
        //  ReadDefinition;
        ctnUsesSection:
          ReadUsesSection;
        else
          WriteLn( 'Intf/Type Section: Warning: ignored node type: ', ActNode.DescAsString );
      end;
      FActNode:= ActNode.NextBrother;
    end;
end;

procedure TP3DPascalParserInterfaceSection.ReadInterfaceSection;
var
  Parser: TP3DPascalParserInterfaceSection;
begin
  Parser:= TP3DPascalParserInterfaceSection.Create( DOMNode, ActNode );
  Parser.Free;
end;

procedure TP3DPascalParserInterfaceSection.ReadUsesSection;
var
  ANode: TCodeTreeNode;
  useDom: TDOMNode;
  UsesSection: TDOMElement;

  procedure ExtractUseUnit;
  begin
    CreateXMLSection( 'file', UsesSection ).AttribStrings[ 'name' ]:= Tool.ExtractUsedUnitName( ANode );
  end;

begin
  UsesSection:= CreateXMLSection( 'uses', DOMNode );
  ANode:= ActNode.FirstChild;
  while ( Assigned( ANode )) do
    begin
      if ( ANode.Desc = ctnUseUnit ) then
        ExtractUseUnit
      else
        WriteLn( 'Intf/Type Section: Warning: ignored node type: ', ANode.DescAsString );
      ANode:= ANode.NextBrother;
    end;
end;

procedure TP3DPascalParserInterfaceSection.ReadDefinition;
var
  Parser: TP3DPascalParserDefinition;
begin
  Parser:= TP3DPascalParserDefinition.Create( DOMNode, ActNode );
  Parser.Free;
end;

procedure TP3DPascalParserInterfaceSection.ReadClass;
var
  Parser: TP3DPascalParserDefinitionClass;
begin
  Parser:= TP3DPascalParserDefinitionClass.Create( DOMNode, ActNode );
  Parser.Free;
end;

{ TP3DPascalParserDefinitionClass }

procedure TP3DPascalParserDefinitionClass.ReadProperties;
var
  ParentNode: TCodeTreeNode;
  imax: Integer;
begin
  ReadComment;
  FDOMNodeName:= 'class';
  imax:= Node.EndPos;
  ParentNode:= Node;
  while ParentNode.StartPos < imax do
    begin
      ParentNode:= ParentNode.Next;
      if ( ParentNode.Desc = ctnClassInheritance ) then
        begin
          //ParentNode:= ParentNode.FirstChild;
          break;
        end;
    end;
  //ParentNode:= Tool.FindInheritanceNode( Node );
  FNode:= Node.FirstChild;
  FNodeType:= lowercase( Node.DescAsString );
  FName:= Tool.ExtractClassName( Node,false );
  if ( Assigned( ParentNode )) then
    FParentClass:= Tool.ExtractCode( ParentNode.StartPos + 1, ParentNode.EndPos - 1, [ phpWithoutBrackets ])
  else
    FParentClass:= 'TObject';
  FDefinition:= Format( '%s = %s ( %s );', [ Name, NodeType, ParentClass ]);
  //FNode:= FNode.NextBrother;
end;

procedure TP3DPascalParserDefinitionClass.ReadClassMember;
  function MakeSection( visibility: String ): TDOMElement;
  var
    i: Integer;
  begin
    Result:= nil;
    for i:= 0 to DOMNode.ChildNodes.Count - 1 do
      if ( TDOMElement( DOMNode.ChildNodes[ i ]).GetAttribute( 'visibility' ) = visibility ) then
        begin
          Result:= TDOMElement( DOMNode.ChildNodes[ i ]);
          break;
        end;
    if ( not Assigned( Result )) then
      begin
        Result:= CreateXMLSection( 'section', DOMNode );
        Result.SetAttribute( 'visibility', visibility );
      end;
  end;

var
  CurNode: TCodeTreeNode;
  scopeDom: TDOMElement;
begin
  CurNode:= ActNode.FirstChild;
  if ( not Assigned( CurNode )) then
    exit;
  scopeDom:= MakeSection( lowercase( ActNode.DescAsString ));
  while Assigned( CurNode ) do
    begin
      FActNode:= CurNode;
      case CurNode.Desc of
        ctnProcedure,ctnProcedureHead,
        ctnProperty,
        ctnVarDefinition:
          ReadClassDefinition( scopeDom );
        else
          WriteLn( CurNode.DescAsString );
      end;
      CurNode:= CurNode.NextBrother;
    end;
end;

procedure TP3DPascalParserDefinitionClass.ReadClassDefinition( Section: TDOMElement );
var
  Parser: TP3DPascalParserDefinition;
begin
  Parser:= TP3DPascalParserDefinition.Create( Section, ActNode );
  Parser.Free;
end;

procedure TP3DPascalParserDefinitionClass.WriteXML;
var
  CurNode: TCodeTreeNode;
begin
  inherited WriteXML;
  WriteAttribute( 'parent', ParentClass );

  CurNode:= Node.FirstChild;
  while Assigned( CurNode ) do
    begin
      FActNode:= CurNode;
      case CurNode.Desc of
        ctnClassPrivate, ctnClassProtected, ctnClassPublic, ctnClassPublished:
          ReadClassMember;
      end;
      CurNode:= CurNode.NextBrother;
    end;
end;

{ TP3DPascalParserDefinition }

procedure TP3DPascalParserDefinition.ReadProperties;
begin
  ReadComment;

  case Node.Desc of
    ctnGenericType: ReadGeneric;
    ctnProcedure,ctnProcedureHead: ReadProcedure;
    ctnVarDefinition: ReadVariable;
    ctnTypeDefinition: ReadType;
    ctnConstDefinition: ReadConst;
    ctnProperty: ReadProperty;
    else
      FNodeType:= lowercase( Tool.ExtractDefinitionName( Node ));
  end;
end;

procedure TP3DPascalParserDefinition.ReadProcedure;
begin
  FDOMNodeName:= 'procedure';
  FNodeType:= lowercase( Tool.ExtractDefinitionName( Node ));
  FDefinition:= Tool.ExtractProcHead( Node,
    [ phpWithStart, phpWithVarModifiers, phpWithParameterNames,
      phpWithDefaultValues, phpWithResultType, phpWithOfObject, phpCommentsToSpace ]);

  if ( FNodeType = 'operator' ) then
    FName:= ExtractWord( 2, FDefinition, [ ' ', #08 ])
  else
    FName:= Tool.ExtractProcName( Node, []);
end;

procedure TP3DPascalParserDefinition.ReadVariable;
begin
  FDOMNodeName:= 'variable';
  FNodeType:= 'variable';
  FName:= Tool.ExtractDefinitionName( Node );
  FDefinition:= GetDefinition( Node );
end;

procedure TP3DPascalParserDefinition.ReadType;
begin
  FDOMNodeName:= 'type';
  FNodeType:= 'type';
  FName:= Tool.ExtractDefinitionName( Node );
  FDefinition:= GetDefinition( Node );
end;

procedure TP3DPascalParserDefinition.ReadProperty;
begin
  FDOMNodeName:= 'property';
  FNodeType:= 'property';
  FName:= Tool.ExtractPropName( Node, False );
  FDefinition:= GetDefinition( Node );
end;

procedure TP3DPascalParserDefinition.ReadGeneric;
begin
  FDOMNodeName:= 'generic';
  FNodeType:= 'generic';
  FName:= Tool.ExtractDefinitionName( Node );
  FDefinition:= GetDefinition( Node );
end;

procedure TP3DPascalParserDefinition.WriteXML;
var
  Parser: TP3DPascalParserInterfaceSection;
begin
  inherited WriteXML;
  {if ( Node.Desc = ctnGenericType ) then
    begin
      Parser:= TP3DPascalParserInterfaceSection.Create( DOMNode, Node.FirstChild );
      Parser.Free;
    end;}
end;

procedure TP3DPascalParserDefinition.ReadConst;
begin
  FDOMNodeName:= 'const';
  FNodeType:= 'const';
  FName:= Tool.ExtractDefinitionName( Node );
  FDefinition:= GetDefinition( Node );
end;

{ TP3DPascalParserUnit }

constructor TP3DPascalParserUnit.Create(FileName: String; XML: TXMLDocument);
var
  Code: TCodeBuffer;
begin
  Code:= CodeToolBoss.LoadFile( ExpandFileName( FileName ), False, False );
  if ( not Assigned( Code )) then
    raise Exception.Create( 'Could not load the unit file: '+ FileName );
  Tool:= nil;
  if ( not CodeToolBoss.Explore( Code, Tool, False, True )) then
    raise Exception.Create( 'The code of the unit file could not be parsed as it contains errors: ' + FileName );

  if ( not Assigned( Tool )) then
    Tool:= TCodeTool( CodeToolBoss.GetCodeToolForSource( Code, False, False ));

  FNode:= Tool.Tree.Root;

  ReadProperties;

  FDOMNode:= XML.CreateElement( DOMNodeName );
  XML.AppendChild( DOMNode );
  WriteXML;
end;

destructor TP3DPascalParserUnit.Destroy;
begin
  FNode:= nil;
  FActNode:= nil;
  inherited Destroy;
end;

procedure TP3DPascalParserUnit.ReadProperties;
begin
  DOMNodeName:= 'unit';
  if ( not Node.Desc in [ ctnUnit, ctnProgram ]) then
    raise Exception.Create( Format( 'Error(%d): unit or program expected!', [ Node.StartPos ]));

  FName:= Tool.ExtractSourceName;
end;

procedure TP3DPascalParserUnit.ReadInterfaceSection;
var
  Parser: TP3DPascalParserInterfaceSection;
begin
  Parser:= TP3DPascalParserInterfaceSection.Create( DOMNode, ActNode );
  Parser.Free;
end;

procedure TP3DPascalParserUnit.WriteXML;
var
  CurNode: TCodeTreeNode;
begin
  inherited WriteXML;

  CurNode:= Node.NextBrother;

  while ( Assigned( CurNode )) do
    begin
      case CurNode.Desc of
        ctnInterface:
          begin
            FActNode:= CurNode;
            ReadInterfaceSection;
          end;
        ctnImplementation:
          break;
        else
          WriteLn( 'Warning: ignored node type: ', CurNode.DescAsString );
      end;
      CurNode:= CurNode.NextBrother;
    end;
end;

{ TP3DPascalParser }

constructor TP3DPascalParser.Create(XMLParent: TDOMElement; ANode: TCodeTreeNode);
begin
  inherited Create;

  Node:= ANode;

  ReadProperties;
  FDOMNode:= CreateXMLSection( DOMNodeName, XMLParent );
  WriteXML;
end;

function TP3DPascalParser.CreateXMLSection( AName: String; ParentNode: TDOMElement): TDOMElement;
begin
  if ( Assigned( ParentNode )) then
    begin
      Result:= ParentNode.OwnerDocument.CreateElement( AName );
      ParentNode.AppendChild( Result );
    end;
end;

procedure TP3DPascalParser.WriteXML;
begin
  WriteAttribute( 'name', Name );
  WriteAttribute( 'type', NodeType );
  WriteAttribute( 'description', Description );
  WriteAttribute( 'definition', Definition );
end;

procedure TP3DPascalParser.ReadComment;
begin
  FDescription:= GetPasDocCommentsAsHTML( Tool, Node );
end;

procedure TP3DPascalParser.WriteAttribute(Name, Value: String);
begin
  if ( Value > '' ) then
    DOMNode.AttribStrings[ Name ]:= Value;
end;

procedure ParsePascalUnit( FileName: String; OutFile: String );
var
  xml: TXMLDocument;
begin
  xml:= TXMLDocument.Create;
  TP3DPascalParserUnit.Create( FileName, xml ).Free;
  WriteXMLFile( xml, OutFile + '.unit.xml' );
end;

procedure ParsePascalPackage( FileName: String; OutFile: String );
  function LoadUnitFromDom( Dom: TDOMNode ): String;
  var
    fn: TDOMNode;
    UnitFn: TDOMNode;
    UnitFnStr: String;
    unitIdent: DOMString;
  begin
    Result:= '';
    fn:= Dom.FindNode( 'Filename' );
    if ( not Assigned( fn )) then
      exit;
    UnitFn:= fn.Attributes.GetNamedItem( 'Value' );
    if ( not Assigned( UnitFn )) then
      exit;
    UnitFnStr:= ExtractFilePath( FileName ) + UnitFn.TextContent;
    if ( Assigned( Dom.FindNode( 'UnitName' ))) then // Pascal units have unitname tag, others don't
      begin
        unitIdent:= TDOMElement( Dom.FindNode( 'UnitName' )).GetAttribute( 'Value' );
        ParsePascalUnit( UnitFnStr, OutFile + '.' + unitIdent );
        Result:= unitIdent;
      end;
  end;

  function FindPackageName( Node: TDOMNode ): String;
  begin
    Result:= '';
    if ( not Assigned( Node )) then
      exit;
    Node:= node.FindNode( 'Name' );
    if ( not Assigned( Node )) then
      exit;
    Node:= Node.Attributes.GetNamedItem( 'Value' );
    if ( not Assigned( Node )) then
      exit;
    Result:= Node.TextContent;
  end;

  function FindPackageFiles( Node: TDOMNode ): TDOMNode;
  begin
    Result:= node.FindNode( 'Files' );
  end;

var
  xml: TXMLDocument;
  xmlExp: TXMLDocument;
  node: TDOMNode;
  ext: String;
  root: TDOMElement;
  unitnm: String;
  i: Integer;

  function AddElement( Name: String; rt: TDOMNode ): TDOMElement;
  begin
    Result:= xmlExp.CreateElement( Name );
    rt.AppendChild( Result );
  end;

  procedure ReadPackageDependencies( Node: TDOMNode );
  var
    pkgs: TDOMElement;
    i: Integer;
    fn: TDOMNode;
  begin
    Node:= node.FindNode( 'RequiredPkgs' );
    pkgs:= AddElement( 'dependencies', root );
    for i:= 0 to Node.ChildNodes.Count - 1 do
      begin
        fn:= Node.ChildNodes[ i ].FindNode( 'PackageName' );
        if ( not Assigned( fn )) then
          Continue;
        fn:= fn.Attributes.GetNamedItem( 'Value' );
        if ( not Assigned( fn )) then
          Continue;
        AddElement( 'file', pkgs ).SetAttribute( 'name', fn.TextContent );
      end;
  end;

  procedure AddIncludePath( Directory: String );
  var
    IncPathTemplate: TDefineTemplate;
  begin
    Directory:= ExpandFileName( Directory );

    // add a sub template to extend the include search path #IncPath.
    IncPathTemplate:= TDefineTemplate.Create(
      '', // optional: the name of the template, useful for finding it later
      '', // optional: a description
      IncludePathMacroName,
      IncludePathMacro + ';' + Directory
      ,da_DefineRecurse
      );
    // add the include path template to the tree
    CodeToolBoss.DefineTree.Add( IncPathTemplate );
  end;

  procedure ReadPackageSearchPaths( Node: TDOMNode );
  var
    Paths: WideString;
    i: Integer;
    BasePath: RawByteString;
    Path: String;
  begin
    Node:= Node.FindNode( 'CompilerOptions' );
    if ( Assigned( Node )) then
      Node:= Node.FindNode( 'SearchPaths' );
    if ( Assigned( Node )) then
      Node:= Node.FindNode( 'IncludeFiles' );
    if ( Assigned( Node )) then
      begin
        BasePath:= ExtractFilePath( FileName );
        Paths:= TDOMElement( Node ).AttribStrings[ 'Value' ];
        for i:= 1 to WordCount( Paths, [ ';' ]) do
          begin
            Path:= ExtractWord( i, Paths, [ ';' ]);
            Path:= CreateAbsolutePath( Path, BasePath );
            AddIncludePath( Path );
          end;
      end;
  end;

begin
  ReadXMLFile( xml, FileName );
  node:= xml.FindNode( 'CONFIG' );
  ext:= ExtractFileExt( FileName );
  if ( Assigned( node )) then
    case ext of
      '.lpk':
        begin
          node:= node.FindNode( 'Package' );
          if ( not Assigned( node )) then
            exit;

          xmlExp:= TXMLDocument.Create;
          root:= xmlExp.CreateElement( 'package' );
          xmlExp.AppendChild( root );

          root.SetAttribute( 'name', FindPackageName( node ));
          ReadPackageDependencies( node );

          ReadPackageSearchPaths( node );

          node:= FindPackageFiles( node );

          for i:= 0 to node.ChildNodes.Count - 1 do
            begin
              unitnm:= LoadUnitFromDom( node.ChildNodes[ i ]);
              if ( unitnm > '' ) then
                AddElement( 'file', root ).SetAttribute( 'name', unitnm );
            end;

          WriteXMLFile( xmlExp, OutFile + '.package.xml' );
        end;
    end;
  xml.Free;
  xmlExp.Free;
end;


end.

