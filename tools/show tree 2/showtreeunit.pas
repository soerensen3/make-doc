unit showtreeunit;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
    CodeCache,
    CodeToolManager,
    CodeAtom,
    BasicCodeTools,
    DefineTemplates,
    strutils,
    FindDeclarationTool,
    CodeTree,
    PascalParserTool,
    IdentCompletionTool,
    DOM, XMLRead,
    fpjson;

type

  { TForm1 }

  TForm1 = class(TForm)
    ErrorList: TListView;
    FindDialog1: TFindDialog;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    procedure ErrorListDblClick(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FindTextInNodes(Root: TTreeNodes; NodeText: String; const StartFrom: TTreeNode = nil ): TTreeNode;
  private
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
var
  i: Integer;
begin
  for i:= low( FileNames ) to high( FileNames ) do
    ParseFile( FileNames[ i ]);
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

procedure TForm1.ErrorListDblClick(Sender: TObject);
begin
  if ( Assigned( ErrorList.Selected )) then
    TTreeNode( ErrorList.Selected.Data ).Selected:= True;
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

function FindNodeClassParent( Node: TCodeTreeNode ): TCodeTreeNode;
var
  imax: Integer;
  ParentNode: TCodeTreeNode;
begin
  imax:= Node.EndPos;
  ParentNode:= Node;

  Result:= nil;
  while Assigned( ParentNode ) and ( ParentNode.StartPos < imax ) do
    if ( ParentNode.Desc = ctnClassInheritance ) then
      begin
        Result:= ParentNode;
        break;
      end
    else
      ParentNode:= ParentNode.Next;
end;

function FindNodeClassParentString( Node: TCodeTreeNode; const Default: String = 'TObject' ): String;
var
  imax: Integer;
  ParentNode: TCodeTreeNode;
begin
  imax:= Node.EndPos;
  ParentNode:= Node;

  Result:= Default;
  while Assigned( ParentNode ) and ( ParentNode.StartPos < imax ) do
    if ( ParentNode.Desc = ctnClassInheritance ) then
      begin
        Result:= Tool.ExtractCode( ParentNode.StartPos + 1, ParentNode.EndPos - 1, [ phpWithoutBrackets ]);
        break;
      end
    else
      ParentNode:= ParentNode.Next;
end;

procedure TForm1.ParseUnit(FileName: String);
type
  TScanProc = procedure ( Parent: TTreeNode; Node: TCodeTreeNode ) is nested;
  TNode = TTreeNode;
var
  TreeView: TTreeView;

  procedure TypeDefinition( Parent: TNode; Node: TCodeTreeNode ); forward;

  function CreateNode( Parent: TNode; Name: String; Value: String ): TNode;
  begin
    Result:= TreeView.Items.AddChild( Parent, Name + ': ' + Value );
  end;

  function CreateNode( Parent: TNode; Name: String ): TNode;
  begin
    Result:= TreeView.Items.AddChild( Parent, Name );
  end;

  procedure NotSupported( Parent: TNode; Node: TCodeTreeNode );
  var
    TN: TNode;
    capt: String;
    curchild: TCodeTreeNode;
    p: Integer;
  begin
    if ( Assigned( Node.Next )) then
      p:= Node.Next.StartPos - 1
    else
      p:= Node.EndPos;
    capt:= 'ERROR: Unsupported node in this context: ' + Node.DescAsString + ': ' + Tool.ExtractCode( Node.StartPos, p, []);
    TN:= CreateNode( Parent, capt );
    ErrorList.AddItem( ExtractFileName( FileName ) + ': ' + capt, TN );
    //TN:= Parent;
    curchild:= Node.FirstChild;
    while ( Assigned( curchild )) do
      begin
        //ScanSections( TN, curchild );
        curchild:= curchild.NextBrother;
      end;
  end;

  {$INCLUDE p3dparsepascal.inc}
var
  TN: TTreeNode;
  Code: TCodeBuffer;
  CurNode: TCodeTreeNode;
  Page: TTabSheet;
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
  TreeView.OnKeyUp:= @FormKeyUp;
  TreeView.BeginUpdate;
  TN:= TreeView.Items.AddFirst( nil, ExtractFileName( FileName ));
  TreeView.AutoExpand:= True;
  CurNode:= Tool.Tree.Root;
  while Assigned( CurNode ) do
    begin
      LoopUnitSections( TN, CurNode );
      CurNode:= CurNode.NextBrother;
    end;
  TreeView.EndUpdate;
end;

end.

