unit p3dparsepascal;

{$mode objfpc}{$H+}

{$ModeSwitch nestedprocvars}

interface
  uses
    Classes,
    SysUtils,
    LazFileUtils,
    FileProcs,
    CodeCache,
    CodeToolsConfig,
    CodeToolManager,
    BasicCodeTools,
    DefineTemplates,
    Generics.Collections,
    //FileUtil,
    strutils,
    FindDeclarationTool,
    CodeTree,
    CodeAtom,
    DOM,
    XMLRead,
    JsonTools,
    jsonparser;

  //Init Pascal Reader
  procedure P3DParsePascalInit;
  //Finish Pascal Reader
  procedure P3DParsePascalFinish;

  procedure ParsePascalUnit( FileName: String; OutFile: String );
  procedure ParsePascalProgram( FileName: String; OutFile: String );
  procedure ParsePascalPackage( FileName: String; OutFile: String );

  const
    ConfigFilename = 'config.xml'; //Filename of CodeTools config

  var
    {Code tool}
    Tool: TCodeTool;
    GlobalRefContext: TJsonNode = nil;
    UnitDict: specialize TObjectDictionary < String, TJsonNode > = nil;


implementation

procedure P3DParsePascalInit;
var
  Opts: TCodeToolsOptions;
begin
//  Opts:= TCodeToolsOptions.Create;
//  Opts.LoadFromFile( ConfigFilename );
  CodeToolBoss.CompilerDefinesCache.LoadFromFile( ConfigFilename );
  UnitDict:= specialize TObjectDictionary < String, TJsonNode >.Create;
  //Opts.SaveToFile(ConfigFilename);
//  Opts.Free;
end;

procedure P3DParsePascalFinish;
begin
  FreeAndNil( UnitDict );
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

function NodeDescriptionAsString(Desc: TCodeTreeNodeDesc): string;
begin
  case Desc of
    ctnNone: Result:='None';

    ctnClass: Result:='Class';
    ctnClassInterface: Result:='ClassInterface';
    ctnDispinterface: Result:='Dispinterface';
    ctnObject: Result:='Object';
    ctnObjCClass: Result:='ObjCClass';
    ctnObjCCategory: Result:='ObjCCategory';
    ctnObjCProtocol: Result:='ObjCProtocol';
    ctnCPPClass: Result:='CPPClass';
    ctnTypeHelper: Result:='TypeHelper';
    ctnRecordHelper: Result:='RecordHelper';

    ctnClassInheritance: Result:='ClassInheritance';
    ctnClassGUID: Result:='GUID';
    ctnClassPrivate: Result:='Private';
    ctnClassProtected: Result:='Protected';
    ctnClassPublic: Result:='Public';
    ctnClassPublished: Result:='Published';
    ctnClassRequired: Result:='RequiredSection';
    ctnClassOptional: Result:='OptionalSection';
    ctnClassClassVar: Result:='ClassVar';
    ctnClassAbstract: Result:='Abstract';
    ctnClassSealed: Result:='Sealed';
    ctnClassExternal: Result:='External';
    ctnClassHelper: Result:='ClassHelper';
    ctnHelperFor: Result:='HelperFor';

    ctnProcedure: Result:='Procedure';
    ctnProcedureHead: Result:='ProcedureHead';
    ctnParameterList: Result:='ParameterList';

    ctnBeginBlock: Result:='BeginBlock';
    ctnAsmBlock: Result:='AsmBlock';

    ctnProgram: Result:='Program';
    ctnPackage: Result:='Package';
    ctnLibrary: Result:='Library';
    ctnUnit: Result:='Unit';
    ctnUseUnitNamespace: Result:='Namespace';
    ctnUseUnitClearName: Result:='UnitNameWithoutNamespace';
    ctnInterface: Result:='InterfaceSection';
    ctnImplementation: Result:='Implementation';
    ctnInitialization: Result:='Initialization';
    ctnFinalization: Result:='Finalization';
    ctnEndPoint: Result:='End.';

    ctnTypeSection: Result:='TypeSection';
    ctnVarSection: Result:='VarSection';
    ctnConstSection: Result:='ConstSection';
    ctnResStrSection: Result:='ResourceStringSection';
    ctnPropertySection: Result:='PropertySection';
    ctnUsesSection: Result:='UsesSection';
    ctnRequiresSection: Result:='RequiresSection';
    ctnContainsSection: Result:='ContainsSection';
    ctnExportsSection: Result:='ExportsSection';

    ctnTypeDefinition: Result:='Type';
    ctnVarDefinition: Result:='Var';
    ctnConstDefinition: Result:='Const';
    ctnGlobalProperty: Result:='GlobalProperty';
    ctnUseUnit: Result:='use unit';
    ctnVarArgs: Result:='VarArgs';

    ctnProperty: Result:='Property'; // can start with 'class property'
    ctnMethodMap: Result:='MethodMap';

    ctnIdentifier: Result:='Identifier';
    ctnOpenArrayType: Result:='OpenArrayType';
    ctnOfConstType: Result:='OfConst';
    ctnRangedArrayType: Result:='RangedArrayType';
    ctnRecordType: Result:='RecordType';
    ctnRecordCase: Result:='RecordCase';
    ctnRecordVariant: Result:='RecordVariant';
    ctnProcedureType: Result:='ProcedureType';
    ctnSetType: Result:='SetType';
    ctnRangeType: Result:='SubrangeType';
    ctnEnumerationType: Result:='EnumerationType';
    ctnEnumIdentifier: Result:='EnumerationIdentifier';
    ctnLabel: Result:='LabelIdentifier';
    ctnTypeType: Result:='TypeType';
    ctnFileType: Result:='FileType';
    ctnPointerType: Result:='PointerType';
    ctnClassOfType: Result:='ClassOfType';
    ctnVariantType: Result:='VariantType';
    ctnSpecialize: Result:='SpecializeType';
    ctnSpecializeType: Result:='SpecializeTypename';
    ctnSpecializeParams: Result:='SpecializeParameterlist';
    ctnSpecializeParam: Result:='SpecializeParameter';
    ctnGenericType: Result:='GenericType';
    ctnGenericName: Result:='GenericTypeName';
    ctnGenericParams: Result:='GenericTypeParams';
    ctnGenericParameter: Result:='GenericTypeParameter';
    ctnGenericConstraint: Result:='GenericTypeParameterConstraint';
    ctnReferenceTo: Result:='ReferenceTo';
    ctnConstant: Result:='Constant';
    ctnHintModifier: Result:='HintModifier';

    ctnWithVariable: Result:='WithVariable';
    ctnWithStatement: Result:='WithStatement';
    ctnOnBlock: Result:='OnBlock';
    ctnOnIdentifier: Result:='OnIdentifier';
    ctnOnStatement: Result:='OnStatement';
  else
    Result:='invalid descriptor ('+IntToStr(Desc)+')';
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

  function ShiftLeft(const Comment: String) : String;
  var
    Lines : TStringList;
    S : String;
    I, J, LeftMost : Integer;
  begin
    try
      Lines := nil;
      Lines := TStringList.Create;
      Lines.Text := Comment;

      LeftMost := Length(Comment);

      for I := 0 to Lines.Count - 1 do
      begin
        if LeftMost <= 1 then
          Break;

        S := Lines[I];
        J := 1;
        while (J <= Length(S)) and (J < LeftMost) and (S[J] = ' ') do
          Inc(J);

        if J < LeftMost then
          LeftMost := J;
      end;

      if LeftMost > 1 then
        for I := 0 to Lines.Count - 1 do
          Lines[I] := Copy(Lines[I], LeftMost, Length(Lines[I]) - LeftMost + 1);

      Result := Lines.Text;
    finally
      FreeAndNil(Lines);
    end;
  end;

  procedure AddComment;
  begin
    if   ( not Assigned( CodeXYPos )
      or ( not Assigned( LastCodeXYPos ))
      or ( CodeXYPos^.Code <> LastCodeXYPos^.Code )
      or ( CodeXYPos^.Y - LastCodeXYPos^.Y > 10 )) then begin
        // the last comment is at a different position => add a source link
        if ( LastComment <> '' ) then
          Result:= Result + Trim( DelChars( DelChars( LastComment, #10 ), #13 )) //TextToHTML(LastComment)
            +'@br ';
        LastComment:= Trim( CommentStr );
    end else begin
      // these two comments are very near together => combine them
      if ( LastComment <> '' ) then
        LastComment += '@br ';
      LastComment += CommentStr;
    end;
    LastCodeXYPos:= CodeXYPos;
  end;

  {procedure AddComment;
  begin
    if (CodeXYPos=nil) or (LastCodeXYPos=nil)
    or (CodeXYPos^.Code<>LastCodeXYPos^.Code)
    or (CodeXYPos^.Y-LastCodeXYPos^.Y>10) then begin
      // the last comment is at a different position => add a source link
      if LastComment<>'' then
        Result:=Result+'<span class="comment">'+TextToHTML(ShiftLeft(LastComment))
          +' ('+SourcePosToFPDocHint(LastCodeXYPos^,'Source')+')'
          +'</span><br>'+LineEnding;
      LastComment:=CommentStr;
    end else begin
      // these two comments are very near together => combine them
      if LastComment<>'' then
        LastComment+=LineEnding;
      LastComment+=CommentStr;
    end;
    LastCodeXYPos:=CodeXYPos;
  end;}

  function ExtractComment(const Source: String;
    CommentStart: Integer) : String;
  var
    CommentEnd, XPos: Integer;
  begin
    XPos := CodeXYPos^.X;
    CommentEnd := FindCommentEnd(Source, CommentStart, NestedComments);

    case Source[CommentStart] of
    '/':
      begin
        CommentStart := CommentStart + 2;
        XPos := 0;
      end;
    '(':
      begin
        CommentStart := CommentStart + 2;
        CommentEnd := CommentEnd - 2;
        XPos := XPos + 1;
      end;
    '{':
      begin
        CommentStart := CommentStart + 1;
        CommentEnd := CommentEnd - 1;
      end;
    end;
    Result:=Copy(Source, CommentStart, CommentEnd - CommentStart);

    Result := TrimRight(Result);

    if XPos > 0 then
      Result := StringOfChar(' ', XPos) + Result;
  end;

begin
  Result:='';
  if (Tool=nil) or (Node=nil) then exit;
  ListOfPCodeXYPosition:=nil;
  try
    if not Tool.GetPasDocComments(Node,ListOfPCodeXYPosition) then exit;
    if ListOfPCodeXYPosition=nil then exit;
    NestedComments := Tool.Scanner.NestedComments;
    LastCodeXYPos := nil;
    LastComment := '';
    for i := 0 to ListOfPCodeXYPosition.Count - 1 do
    begin
      CodeXYPos := PCodeXYPosition(ListOfPCodeXYPosition[i]);
      CommentCode := CodeXYPos^.Code;
      CommentCode.LineColToPosition(CodeXYPos^.Y,CodeXYPos^.X,CommentStart);
      if (CommentStart<1) or (CommentStart>CommentCode.SourceLength)
      then
        continue;
      CommentStr := ExtractComment(CommentCode.Source, CommentStart);
      AddComment;
    end;
    CommentStr:='';
    CodeXYPos:=nil;
    AddComment;
  finally
    FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
  end;
end;

{

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
    for i := 0 to ListOfPCodeXYPosition.Count - 1 do begin
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
}
function CreateNode( Parent: TJsonNode; Name: String; Value: String ): TJsonNode; alias : 'CreateStringNode';
begin
  if ( Parent.Kind = nkArray ) then
    Result:= Parent.Add( '', Value )
  else if ( Parent.Kind = nkObject ) then begin
    Result:= Parent.Add( Name, Value )
  end else
    WriteLn( 'Warning, Create node: Parent is no array and no object: ', Name, ' parent: ', Parent.AsJSON );
end;

function CreateNode( Parent: TJsonNode; Name: String ): TJsonNode;
begin
  Result:= Parent.Add( Name )
end;

procedure TryParsePascalUnit( FileName: String; OutFile: String );
begin
  if ( FileExists( FileName ) and ( not UnitDict.ContainsKey( ExtractFileNameOnly( FileName )))) then
    ParsePascalUnit( FileName, OutFile );
end;


procedure ParsePascalUnit( FileName: String; OutFile: String );
type
  TNode = TJsonNode;
  TScanProc = procedure ( Parent: TNode; Node: TCodeTreeNode ) is nested;
var
  UnitJSON: TJsonNode;
  Code: TCodeBuffer;
  CurNode: TCodeTreeNode;
  F: TFileStream;
  s: String;
  Units: TStrings;
  i: Integer;

  function GetCodePos( Node: TCodeTreeNode ): String;
  var
    CodePos: TCodeXYPosition;
  begin
    Tool.CleanPosToCaret( Node.StartPos,CodePos );
    Result:= ExtractRelativepath(ExtractFilePath(Tool.MainFilename),CodePos.Code.Filename) + ' (' + CodePos.Y.ToString + ')';
  end;

  function GetContext( Node: TCodeTreeNode ): String;
  var
    p: Integer;
  begin
    if ( Assigned( Node.Next )) then
      p:= Node.Next.StartPos - 1
    else
      p:= Node.EndPos;
    Result:= Node.DescAsString + ': ' + Tool.ExtractCode( Node.StartPos, p, [ ]);
    Result += LineEnding + ' file: ' + GetCodePos( Node );
    if ( Assigned( Node.Parent )) then
      Result += LineEnding + ' context: ' + NodeDescriptionAsString( Node.Parent.Desc );
  end;

  procedure NotSupported( Parent: TNode; Node: TCodeTreeNode );
  var
    capt: String;
  begin
    capt:= 'ERROR: Unsupported node in this context: ' + GetContext( Node );

    WriteLn( capt );
  end;

  {$INCLUDE p3dparsepascal.inc}
begin
  if ( not FileExistsUTF8( Filename )) then
    raise Exception.CreateFmt( 'The specified pascal unit "%s" could not be found!', [ FileName ]);
  Code:= CodeToolBoss.LoadFile( ExpandFileName( FileName ), False, False );
  if ( not Assigned( Code )) then
    raise Exception.Create( 'Could not load the unit file: '+ FileName );
  Tool:= nil;
  if ( not CodeToolBoss.Explore( Code, Tool, False, True )) then
    raise Exception.Create( 'The code of the unit file could not be parsed as it contains errors: ' + FileName );

  if ( not Assigned( Tool )) then
    Tool:= TCodeTool( CodeToolBoss.GetCodeToolForSource( Code, False, False ));

  UnitJSON:= TJsonNode.Create.AsObject;
  UnitDict.Add( ExtractFileNameOnly( FileName ), UnitJSON );
  WriteLn( 'Loaded ', ExtractFileNameOnly( FileName ));

  Tool.FindUsedUnitFiles( Units );
  for i:= 0 to Units.Count - 1 do
    TryParsePascalUnit( Units[ i ], ExtractFilePath( OutFile ) + ExtractFileNameOnly( Units[ i ]));

  Tool:= TCodeTool( CodeToolBoss.GetCodeToolForSource( Code, False, False ));


  GlobalRefContext:= UnitJSON;
  CurNode:= Tool.Tree.Root;
  CreateNode( UnitJSON, 'Name', Tool.ExtractSourceName());
  CreateNode( UnitJSON, 'Comment', ExtractCommentContent( Code.Source, 1, True, True, True, True ));

  // Unit Identifier
  // interface section
  // implementation section

  while Assigned( CurNode ) do begin
    case CurNode.Desc of
      ctnInterface: LoopUnitSections( UnitJSON, CurNode );
      ctnUnit:;
      ctnProgram: LoopUnitSections( UnitJSON, Tool.Tree.Root );
      ctnImplementation:;
      ctnEndPoint:;
      else
        NotSupported( UnitJSON, CurNode );
    end;
    CurNode:= CurNode.NextBrother;
  end;

  GlobalRefContext:= nil;

  try
    WriteLn( OutFile + '.unit.json' );
    WriteLn( 'Uses: ' + UnitJSON.Child( 'Uses' ).AsJson );
    F:= TFileStream.Create( OutFile + '.unit.json', fmCreate );
    s:= UnitJSON.AsJson;
    F.Write( s[ 1 ], Length( s ));
  finally
    F.Free;
    //UnitJSON.Free;
  end;
end;

procedure ParsePascalProgram( FileName: String; OutFile: String );
  function LoadUnitFromDom( Dom: TDOMNode ): String;
  var
    fn: TDOMNode;
    UnitFn: TDOMNode;
    UnitFnStr, Ext: String;
    unitIdent: DOMString;
    Ex: String;
  begin
    Result:= '';
    fn:= Dom.FindNode( 'Filename' );
    if ( not Assigned( fn )) then
      exit;
    UnitFn:= fn.Attributes.GetNamedItem( 'Value' );
    if ( not Assigned( UnitFn )) then
      exit;
    UnitFnStr:= UnitFn.TextContent;
    Ex:= ExtractFileExt( UnitFnStr );
    if (( Ex = '.pp' ) or ( Ex = '.pas' ) or ( Ex = '.lpr' )) then begin
    //if ( Assigned( Dom.FindNode( 'UnitName' ))) then begin // Pascal units have unitname tag, others don't
      unitIdent:= ExtractFileNameOnly( UnitFnStr );
      TryParsePascalUnit( ExtractFilePath( FileName ) + UnitFnStr, ExtractFilePath( OutFile ) + unitIdent );
      Result:= unitIdent;
    end;
  end;

  function FindProgramName( Node: TDOMNode ): String;
  begin
    Result:= ExtractFileNameOnly( FileName );
    if ( not Assigned( Node )) then
      exit;
    Node:= node.FindNode( 'General' );
    if ( not Assigned( Node )) then
      exit;
    Node:= node.FindNode( 'Title' );
    if ( not Assigned( Node )) then
      exit;
    Node:= Node.Attributes.GetNamedItem( 'Value' );
    if ( not Assigned( Node )) then
      exit;
    Result:= Node.TextContent;
  end;

  function FindProgramFiles( Node: TDOMNode ): TDOMNode;
  begin
    Result:= node.FindNode( 'Units' );
  end;

var
  xml: TXMLDocument;
  PackageJSON, Units: TJsonNode;
  rootnode, projopt, files, compopt: TDOMNode;
  ext: String;
  unitnm: String;
  i: Integer;
  F: TFileStream;
  s: String;

  procedure ReadProgramDependencies( Node: TDOMNode );
  var
    pkgs: TJsonNode;
    i: Integer;
    fn: TDOMNode;
  begin
    Node:= node.FindNode( 'RequiredPackages' );
    if ( not Assigned( Node )) then
      exit;
    pkgs:= CreateNode( PackageJSON, 'Dependencies' ).AsArray;
    for i:= 0 to Node.ChildNodes.Count - 1 do begin
      fn:= Node.ChildNodes[ i ].FindNode( 'PackageName' );
      if ( not Assigned( fn )) then
        Continue;
      fn:= fn.Attributes.GetNamedItem( 'Value' );
      if ( not Assigned( fn )) then
        Continue;
      CreateNode( pkgs, '', fn.TextContent );
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

  procedure ReadProgramSearchPaths( Node: TDOMNode );
  var
    Paths: WideString;
    i: Integer;
    BasePath: RawByteString;
    Path: String;
  begin
    if ( Assigned( Node )) then
      Node:= Node.FindNode( 'SearchPaths' );
    if ( Assigned( Node )) then
      Node:= Node.FindNode( 'IncludeFiles' );
    if ( Assigned( Node )) then begin
      BasePath:= ExtractFilePath( FileName );
      Paths:= TDOMElement( Node ).AttribStrings[ 'Value' ];
      for i:= 1 to WordCount( Paths, [ ';' ]) do begin
        Path:= ExtractWord( i, Paths, [ ';' ]);
        Path:= CreateAbsolutePath( Path, BasePath );
        AddIncludePath( Path );
      end;
    end;
  end;

begin
  if ( not FileExistsUTF8( Filename )) then
    raise Exception.CreateFmt( 'The specified pascal program "%s" could not be found!', [ FileName ]);

  ext:= ExtractFileExt( FileName );

  if ( ext = '.lpr' ) then
    FileName:= ChangeFileExt( FileName, '.lpi' );
  ReadXMLFile( xml, FileName );
  rootnode:= xml.FindNode( 'CONFIG' );
  if ( Assigned( rootnode )) then begin
    // PROJECT OPTIONS -->
    projopt:= rootnode.FindNode( 'ProjectOptions' );
    if ( not Assigned( projopt )) then
      exit;

    PackageJSON:= TJsonNode.Create;
    CreateNode( PackageJSON, 'Type', 'program' );
    CreateNode( PackageJSON, 'Name', FindProgramName( projopt ));
    ReadProgramDependencies( projopt );

    files:= FindProgramFiles( projopt );

    Units:= CreateNode( PackageJSON, 'Units' ).AsArray;

    if ( Assigned( files )) then
      for i:= 0 to files.ChildNodes.Count - 1 do begin
        unitnm:= LoadUnitFromDom( files.ChildNodes[ i ]);
        if ( unitnm > '' ) then
          CreateNode( Units, '', unitnm );
      end;
    // <-- PROJECT OPTIONS


    // COMPILER OPTIONS -->
    compopt:= rootnode.FindNode( 'CompilerOptions' );

    if ( Assigned( compopt )) then
      ReadProgramSearchPaths( compopt );
    // <-- COMPILER OPTIONS

    try
      WriteLn( OutFile + '.program.json' );
      F:= TFileStream.Create( OutFile + '.program.json', fmCreate );
      PackageJSON.SaveToStream( F );
    finally
      F.Free;
      PackageJSON.Free;
    end;
  end;
  xml.Free;
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
    if ( Assigned( Dom.FindNode( 'UnitName' ))) then begin // Pascal units have unitname tag, others don't
      unitIdent:= TDOMElement( Dom.FindNode( 'UnitName' )).GetAttribute( 'Value' );
      TryParsePascalUnit( UnitFnStr, ExtractFilePath( OutFile ) + unitIdent );
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
  PackageJSON, Units: TJsonNode;
  node: TDOMNode;
  ext: String;
  unitnm: String;
  i: Integer;
  F: TFileStream;
  s: String;

  procedure ReadPackageDependencies( Node: TDOMNode );
  var
    pkgs: TJsonNode;
    i: Integer;
    fn: TDOMNode;
  begin
    Node:= node.FindNode( 'RequiredPkgs' );
    pkgs:= CreateNode( PackageJSON, 'Dependencies' ).AsArray;
    for i:= 0 to Node.ChildNodes.Count - 1 do begin
      fn:= Node.ChildNodes[ i ].FindNode( 'PackageName' );
      if ( not Assigned( fn )) then
        Continue;
      fn:= fn.Attributes.GetNamedItem( 'Value' );
      if ( not Assigned( fn )) then
        Continue;
      CreateNode( pkgs, '', fn.TextContent );
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
    if ( Assigned( Node )) then begin
      BasePath:= ExtractFilePath( FileName );
      Paths:= TDOMElement( Node ).AttribStrings[ 'Value' ];
      for i:= 1 to WordCount( Paths, [ ';' ]) do begin
        Path:= ExtractWord( i, Paths, [ ';' ]);
        Path:= CreateAbsolutePath( Path, BasePath );
        AddIncludePath( Path );
      end;
    end;
  end;

begin
  if ( not FileExistsUTF8( Filename )) then
    raise Exception.CreateFmt( 'The specified pascal package "%s" could not be found!', [ FileName ]);

  ReadXMLFile( xml, FileName );
  node:= xml.FindNode( 'CONFIG' );
  ext:= ExtractFileExt( FileName );
  if ( Assigned( node )) then
    case ext of
      '.lpk': begin
        node:= node.FindNode( 'Package' );
        if ( not Assigned( node )) then
          exit;

        PackageJSON:= TJsonNode.Create;
        CreateNode( PackageJSON, 'Type', 'package' );
        CreateNode( PackageJSON, 'Name', FindPackageName( node ));
        ReadPackageDependencies( node );

        ReadPackageSearchPaths( node );

        node:= FindPackageFiles( node );

        Units:= CreateNode( PackageJSON, 'Units' ).AsArray;
        for i:= 0 to node.ChildNodes.Count - 1 do begin
          unitnm:= LoadUnitFromDom( node.ChildNodes[ i ]);
          if ( unitnm > '' ) then
            CreateNode( Units, '', unitnm );
        end;

        try
          WriteLn( OutFile + '.package.json' );
          F:= TFileStream.Create( OutFile + '.package.json', fmCreate );
          PackageJSON.SaveToStream( F );
        finally
          F.Free;
          PackageJSON.Free;
        end;
      end;
    end;
  xml.Free;
end;


end.

