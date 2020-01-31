unit showtreeunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, LCLType,
    CodeCache,
    CodeToolManager,
    BasicCodeTools,
    DefineTemplates,
    strutils,
    FindDeclarationTool,
    CodeTree,
    PascalParserTool,
    IdentCompletionTool,
    DOM, XMLRead;

type

  { TForm1 }

  TForm1 = class(TForm)
    FindDialog1: TFindDialog;
    PageControl1: TPageControl;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FindTextInNodes(Root: TTreeNodes; NodeText: String; const StartFrom: TTreeNode = nil ): TTreeNode;

    { private declarations }
  public
    procedure ParseUnit( FileName: String );
    procedure ParsePackage( FileName: String );
    procedure ParseFile( FileName: String );
    { public declarations }
  end;

var
  Form1: TForm1;

const
  ConfigFilename = 'codetools.config'; //Filename of CodeTools config

var
  {Code tool}
  Tool: TCodeTool;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  ParseFile( FileNames[ 0 ]);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (( Key = VK_F ) and ( ssCtrl in Shift )) then
    FindDialog1.Execute;
end;

function TForm1.FindTextInNodes(Root: TTreeNodes; NodeText: String; const StartFrom: TTreeNode): TTreeNode;
begin
  if ( Assigned( StartFrom )) then
    Result:= StartFrom.GetNext
  else
    Result:= Root.GetFirstNode;
  while Assigned(Result) and ( Pos( NodeText, Result.Text ) = 0 ) do
    Result:= Result.GetNext;
end;

procedure TForm1.FindDialog1Find(Sender: TObject);
var
  Ctrl: TControl;
  Node: TTreeNode;
begin
  if ( Assigned( PageControl1.ActivePage ) and ( PageControl1.ActivePage.ControlCount > 0 )) then
  Ctrl:= PageControl1.ActivePage.Controls[ 0 ];
  if ( Ctrl is TTreeView ) then
    begin
      Node:= FindTextInNodes( TTreeView( Ctrl ).Items, FindDialog1.FindText, TTreeView( Ctrl ).Selected );
      if ( Assigned( Node )) then
        Node.Selected:= True;
    end;
end;

procedure TForm1.ParseFile(FileName: String);
begin
  case ExtractFileExt( FileName ) of
    '.lpr', '.pas', '.pp': ParseUnit( FileName );
    '.lpk': ParsePackage( FileName );
  end;
end;

procedure TForm1.ParsePackage(FileName: String);
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
        ParseUnit( UnitFnStr );
        Result:= unitIdent;
      end;
  end;

var
  xml: TXMLDocument;
  node: TDOMNode;
  i: Integer;
begin
  ReadXMLFile( xml, FileName );
  node:= xml.FindNode( 'CONFIG' );
  node:= node.FindNode( 'Package' );
  ReadPackageSearchPaths( node );
  node:= node.FindNode( 'Files' );
  for i:= 0 to node.ChildNodes.Count - 1 do
    LoadUnitFromDom( node.ChildNodes[ i ]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CodeToolBoss.SimpleInit( ConfigFilename );

  if ( ParamStr( 1 ) > '' ) then
    ParseFile( ParamStr( 1 ));
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


procedure TForm1.ParseUnit(FileName: String);
var
  TreeView: TTreeView;

  procedure CreateNode( Parent: TTreeNode; Node: TCodeTreeNode );
  var
    TN: TTreeNode;
    capt: String;
    curchild: TCodeTreeNode;
    p: Integer;
  begin
    if ( Assigned( Node.Next )) then
      p:= Node.Next.StartPos - 1
    else
      p:= Node.EndPos;

    capt:= Node.DescAsString + ': ' + Tool.ExtractCode( Node.StartPos, p, []) + ' // ' + GetPasDocCommentsAsHTML( Tool, Node );
    TN:= TreeView.Items.AddChild( Parent, capt );

    curchild:= Node.FirstChild;
    while ( Assigned( curchild )) do
      begin
        CreateNode( TN, curchild );
        curchild:= curchild.NextBrother;
      end;
  end;

var
  TN: TTreeNode;
  Code: TCodeBuffer;
  CurNode: TCodeTreeNode;
  Page: TTabSheet;
  CommentStr: String;
begin
  Code:= CodeToolBoss.LoadFile( ExpandFileName( FileName ), False, False );
  if ( not Assigned( Code )) then
    raise Exception.Create( 'Could not load the unit file: '+ FileName );
  Tool:= nil;
  if ( not CodeToolBoss.Explore( Code, Tool, False, True )) then
    raise Exception.Create( 'The code of the unit file could not be parsed as it contains errors: ' + FileName );

  if ( not Assigned( Tool )) then
    Tool:= TCodeTool( CodeToolBoss.GetCodeToolForSource( Code, False, False ));

  Page:= PageControl1.AddTabSheet;
  Page.Caption:= ExtractFileName( FileName );
  TreeView:= TTreeView.Create( Page );
  TreeView.Parent:= Page;
  TreeView.Align:= alClient;
  TreeView.BeginUpdate;
  TreeView.OnKeyUp:= @FormKeyUp;
  CommentStr:= ExtractCommentContent( Code.Source, 1,
                                      True, True, True, True );
  TN:= TreeView.Items.AddFirst( nil, ExtractFileName( FileName ) + ' // ' + CommentStr);
  TreeView.AutoExpand:= True;
  CurNode:= Tool.Tree.Root;
  while Assigned( CurNode ) do
    begin
      CreateNode( TN, CurNode );
      CurNode:= CurNode.NextBrother;
    end;
  TreeView.EndUpdate;
end;

end.

