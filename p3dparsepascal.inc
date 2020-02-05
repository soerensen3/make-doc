{ p3dparsepascal.inc

  Copyright (c) 2017 Johannes Rosleff Sörensen

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
  THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  Description:
    Introduces function to parse TCodeTree nodes and generate a code tree file.
}

  // Asserts that Section is initialized. If Section is Nil a new section by the name of SectName of type TP is created.
  function AssertSection( Parent: TNode; var Section: TNode; SectName: String; const TP: TJsonNodeKind = nkArray ): TNode;
  begin
    if ( not Assigned( Section )) then
      Section:= Parent.Add( SectName, TP );
    Result:= Section;
  end;

  procedure GetSourceParams( Parent: TNode; Node: TCodeTreeNode );
  var
    comment: String;
  begin
    CreateNode( Parent, 'Code', Tool.ExtractNode( Node, []));
    CreateNode( Parent, 'SourcePos', GetCodePos( Node ));
    comment:= GetPasDocCommentsAsHTML( Tool, Node );
    if ( comment > '' ) then
      CreateNode( Parent, 'Comment', comment );
  end;

  // Loops through a Code Tree section's child nodes and calls ScanProc for each node.
  procedure LoopSimple( Parent: TNode; Node: TCodeTreeNode; ScanProc: TScanProc );
  var
    Child: TCodeTreeNode;
  begin
    Child:= Node.FirstChild;
    while ( Assigned( Child )) do begin
      ScanProc( Parent, Child );
      Child:= Child.NextBrother;
    end;
  end;

  procedure LoopClassMembers( Parent: TNode; Root: TCodeTreeNode ); forward;

  procedure ParseUseUnit( Parent: TNode; Node: TCodeTreeNode );
  begin
    if ( Node.Desc = ctnUseUnit ) then
      CreateNode( Parent, '', Trim( Tool.ExtractUsedUnitName( Node )))
    else
      NotSupported( Parent, Node );
  end;

  function CreateTypeNode( Parent: TNode; TypeName: String; Context: TNode ): TNode;
    function WalkJSON( ThisParent: TNode ): String;
    begin
      if ( Assigned( ThisParent ) and ( ThisParent <> ThisParent.Root )) then
        Result:= WalkJSON( ThisParent.Parent ) + '.' + ThisParent.Name
      else
        Result:= '';
    end;

    function FindType( Name: String; Context: TNode ): TNode;
    var
      tp, params: TJsonNode;
      i: Integer;
    begin
      if ( Context = nil ) then
        exit( nil );
      tp:= Context.Child( 'Types' );
      if ( Assigned( tp )) then
        tp:= tp.Child( Name )
      else begin
        params:= Context.Child( 'GenericParams' );
        if ( Assigned( params )) then
          for i:= 0 to params.Count - 1 do
            if ( params.Child( i ).AsString = Name ) then begin
              tp:= params.Child( i );
              break;
            end;
      end;

      if ( Assigned( tp )) then
        Result:= tp
      else
        Result:= FindType( Name, Context.Parent );
    end;

  var
    tpcont: TNode;
  begin
    Result:= CreateNode( Parent, 'Type' );

    CreateNode( Result, 'Name', TypeName );
    CreateNode( Result, 'NodeType', 'TypeReference' );

    tpcont:= FindType( TypeName, Context );
    if ( Assigned( tpcont )) then
      CreateNode( Result, 'Context', WalkJSON( tpcont )).AsString;
    //CreateNode( Result, 'Test', WalkJSON( Context ));
  end;

  procedure ExtractTypeReference( Parent: TNode; Node: TCodeTreeNode; Context: TNode );
  var
    Identifier: String;
  begin
    Identifier:= Tool.ExtractDefinitionName( Node );
    CreateTypeNode( Parent, Identifier, Context );
  end;

  procedure ParseTypeReference( Parent: TNode; Node: TCodeTreeNode ); overload;
  begin
    ExtractTypeReference( Parent, Node, GlobalRefContext );
  end;

  procedure ParseTypeReference( Parent: TNode; Identifier: String ); overload;
  begin
    CreateTypeNode( Parent, Identifier, GlobalRefContext );
  end;

  procedure ParseVariable( Parent: TNode; Node: TCodeTreeNode ); forward;

  procedure ParseParams( Parent: TNode; Node: TCodeTreeNode );
  begin
    Parent:= CreateNode( Parent, 'Params' );
    LoopSimple( Parent, Node, @ParseVariable );
  end;

  procedure ExtractProcParamList( Parent: TNode; Node:TCodeTreeNode );
    function FindParamNode(): TCodeTreeNode;
    begin
      Result:=Node;
      if Result=nil then exit;
      if Result.Desc=ctnProcedure then begin
        Result:=Result.FirstChild;
        if Result=nil then exit;
      end;
      if Result.Desc<>ctnProcedureHead then exit(nil);
      Result:=Result.FirstChild;
    end;
  var
    Child: TCodeTreeNode;
  begin
    Child:= FindParamNode();
    if ( Assigned( Child )) then begin
      if ( Child.Desc = ctnParameterList ) then begin
        ParseParams( Parent, Child );
        Child:= Child.NextBrother;
      end;
      if ( Assigned( Child ) and ( Child.Desc = ctnIdentifier )) then
        ExtractTypeReference( Parent, Tool.FindTypeNodeOfDefinition( Child ), Parent.Parent );
    end;
  end;
{
  procedure ExtractType( Parent: TNode; Node: TCodeTreeNode );
  var
    Child: TCodeTreeNode;
  begin
    Child:= Node.FirstChild;
    Parent:= CreateNode( Parent, Tool.ExtractDefinitionName( Node ));
    if ( Node.Desc = ctnGenericType ) then
      begin
        Child:= Child.NextBrother;
        ExtractParams( Parent, Child );
        Child:= Child.NextBrother;
      end;
    if ( Assigned( Child )) then
      begin
        if ( Child.Desc = ctnParameterList ) then
          ExtractParams( Parent, Child );
        CreateNode( Parent, 'cttype', Child.DescAsString );
        TypeDefinition( Parent, Child );
      end;
  end;}

  // Parse a variable' name and type
  function ExtractVar( Parent: TNode; Node: TCodeTreeNode ): TNode;
  begin
    Result:= CreateNode( Parent, Tool.ExtractDefinitionName( Node ));
    ParseTypeReference( Result, Tool.FindTypeNodeOfDefinition( Node ));
    GetSourceParams( Result, Node );
  end;

  // Parse a const's name and assigned value
  function ParseConst( Parent: TNode; Node: TCodeTreeNode ): TNode;
  begin
    Result:= CreateNode( Parent, Tool.ExtractDefinitionName( Node ));
    CreateNode( Result, 'Value', Tool.ReadStringConstantValue( Node.FirstChild.StartPos ));
    GetSourceParams( Result, Node );
  end;

  // Parse a class', object's or interface's property
  // property Name: TType read FName write FName;

  procedure ParseProp( Parent: TNode; Node: TCodeTreeNode );
    procedure ParsePropParams; // Parse array parameters of the property
    var
      param: TNode;
      S: TStringList;
      item: String;
    begin
      param:= CreateNode( Parent, 'Params' );
      S:= TStringList.Create;
      while ( Tool.CurPos.Flag <> cafEdgedBracketClose ) do begin
        if ( Tool.CurPos.Flag = cafColon ) then begin // In Free Pascal it is allowed to separate multiple parameter names with a comma and define
                                                      // the type after a colon. like: Param1, Param2, Param3: TType. Therefore the names are first
                                                      // stored in an array and are only written when colon is reach where the type is specified.

          Tool.ReadNextAtom;
          for item in S do begin
            ParseTypeReference(
              CreateNode( param, item ), // Parent
                Tool.GetAtom );
          end;
          S.Clear;
        end else if ( Tool.CurPos.Flag = cafWord ) then
          S.Add( Tool.GetAtom );
        Tool.ReadNextAtom;
      end;
      S.Free;
    end;

    // property Name: TType index 0 read FName write FName;
    procedure ParsePropGetterSetter;
    begin
      while ( Tool.CurPos.Flag <> cafSemicolon ) do begin // parse until the semicolon is reached
        Tool.ReadNextAtom;
        if ( Tool.UpAtomIs( 'READ' )) then
          begin
            Tool.ReadNextAtom;
            CreateNode( Parent, 'read', Tool.GetAtom );
          end
        else if ( Tool.UpAtomIs( 'WRITE' )) then
          begin
            Tool.ReadNextAtom;
            CreateNode( Parent, 'write', Tool.GetAtom );
          end
        else if ( Tool.UpAtomIs( 'INDEX' )) then
          begin
            Tool.ReadNextAtom;
            CreateNode( Parent, 'index', Tool.GetAtom );
          end;
      end;
    end;

  var
    code: String;
  begin
    Parent:= CreateNode( Parent, Tool.ExtractPropName( Node, False ));
    ParseTypeReference( Parent, Tool.ExtractPropType( Node, False, False )); // Class.Visibility.Properties.PropertyName

    code:= Tool.ExtractNode( Node, []);
    CreateNode( Parent, 'Code', code );
    Tool.MoveCursorBehindPropName( Node );

    if ( Tool.PropertyNodeHasParamList( Node )) then
      ParsePropParams;

    ParsePropGetterSetter;
  end;

  function ExtractProc( Parent: TNode; Node: TCodeTreeNode ): TJsonNode;
  var
    Name: String;
  begin
    Tool.MoveCursorToProcName( Node, True );
    Name:= Tool.GetAtom;
    Result:= CreateNode( Parent, Name );

    GetSourceParams( Result, Node );

    if ( Node.Desc = ctnProcedure ) then
      Node:= Node.FirstChild;

    ExtractProcParamList( Result, Node );
  end;

  procedure ParseIdentifier( Parent: TNode; Node: TCodeTreeNode; Ident: String );
  begin
    CreateNode( Parent, Ident, Tool.ExtractIdentifier( Node.StartPos ));
  end;

  procedure ParseIdent( Parent: TNode; Node: TCodeTreeNode );
  begin
    CreateNode( Parent, '', Tool.ExtractIdentifier( Node.StartPos ));
  end;

  procedure ParseRecordCase( Parent: TNode; Node: TCodeTreeNode );
  var
    Child: TCodeTreeNode;
  begin
    Parent:= CreateNode( Parent, 'Case' );
    Child:= Node.FirstChild;

    CreateNode( CreateNode( Parent, 'CaseVar' ), Tool.ExtractNode( Child, []));
    Child:= Child.NextBrother;
    while Assigned( Child ) do begin
      LoopClassMembers( CreateNode( Parent, Tool.ExtractDefinitionName( Child )), Child );
      Child:= Child.NextBrother;
    end;
  end;

  procedure ParseRangedArray( Parent: TNode; Node: TCodeTreeNode );
  begin
    Node:= Node.FirstChild;
    CreateNode( Parent, 'Range', Tool.ExtractNode( Node, []));
    ParseTypeReference( Parent, Tool.ExtractNode( Node.NextBrother, []));
  end;

  procedure ParseOpenArray( Parent: TNode; Node: TCodeTreeNode );
  begin
    Node:= Node.FirstChild;
    ParseTypeReference( Parent, Node );
  end;

  procedure ParseInterface( Parent: TNode; Node: TCodeTreeNode );
  var
    OldGlobalRef: TJsonNode;
  begin
    OldGlobalRef:= GlobalRefContext;
    GlobalRefContext:= Parent;

    LoopClassMembers( Parent, Node );

    GlobalRefContext:= OldGlobalRef;
  end;

  procedure ParseClass( Parent: TNode; Node: TCodeTreeNode );
  var
    Child: TCodeTreeNode;
    SPrivate: TNode = nil;
    SProtected: TNode = nil;
    SPublic: TNode = nil;
    SPublished: TNode = nil;
    SRequired: TNode = nil;
    SOptional: TNode = nil;
    SParent: TNode = nil;
    OldGlobalRef: TJsonNode;

  begin
    {heritage:= FindNodeClassParent( Node );
    if ( Assigned( heritage )) then
      LoopSimple( CreateNode( Parent, 'parent' ), heritage, @TypeDefinition );}
    OldGlobalRef:= GlobalRefContext;
    GlobalRefContext:= Parent;


    Child:= Node.FirstChild;
    CreateNode( Parent, 'NodeType', NodeDescriptionAsString( Node.Desc ));

    while ( Assigned( Child )) do begin
      case Child.Desc of
        ctnClassExternal: ;
        ctnClassAbstract: ;
        ctnClassInheritance: LoopSimple( AssertSection( Parent, SParent, 'Parent' ), Child, @ParseTypeReference );
        ctnClassPrivate: LoopClassMembers( AssertSection( Parent, SPrivate, 'Private', nkObject ), Child );
        ctnClassProtected: LoopClassMembers( AssertSection( Parent, SProtected, 'Protected', nkObject ), Child );
        ctnClassPublic: LoopClassMembers( AssertSection( Parent, SPublic, 'Public', nkObject ), Child );
        ctnClassPublished: LoopClassMembers( AssertSection( Parent, SPublished, 'Published', nkObject ), Child );
        ctnClassRequired: LoopClassMembers( AssertSection( Parent, SRequired, 'Required', nkObject ), Child );
        ctnClassOptional: LoopClassMembers( AssertSection( Parent, SOptional, 'Optional', nkObject ), Child );
      else
        NotSupported( Parent, Child );
      end;
      Child:= Child.NextBrother;
    end;

    GlobalRefContext:= OldGlobalRef;
  end;

  procedure ParseEnumeration( Parent: TNode; Node: TCodeTreeNode );
  var
    Child: TCodeTreeNode;
    SEnum: TNode = nil;
  begin
    AssertSection( Parent, SEnum, 'EnumValues' );

    CreateNode( Parent, 'NodeType', NodeDescriptionAsString( Node.Desc ));

    Child:= Node.FirstChild;
    while ( Assigned( Child )) do begin
      case Child.Desc of
        ctnEnumIdentifier: CreateNode( SEnum, '', Tool.ExtractNode( Child, []));
      else
        NotSupported( SEnum, Child );
      end;
      Child:= Child.NextBrother;
    end;
  end;

  procedure ParseProcedure( Parent: TNode; Node: TCodeTreeNode );
  var
    Nd: TJsonNode;
  begin
    Nd:= ExtractProc( Parent, Node );
    CreateNode( Nd, 'NodeType', NodeDescriptionAsString( Node.Desc ));
  end;

  procedure ParseSet( Parent: TNode; Node: TCodeTreeNode );
  begin
    Parent:= CreateNode( Parent, 'Type', Tool.ExtractDefinitionName( Node.FirstChild ));
  end;

  procedure ParseSpecializeParams( Parent: TNode; Node: TCodeTreeNode );
  begin
    Parent:= CreateNode( Parent, 'SpecializeParams' ).AsArray;
    LoopSimple( Parent, Node, @ParseTypeReference );
  end;

  procedure ParseSpecialize( Parent: TNode; Node: TCodeTreeNode );
  begin
    CreateNode( Parent, 'NodeType', NodeDescriptionAsString( Node.Desc ));
    Node:= Node.FirstChild;

    ParseTypeReference( Parent, Tool.ExtractDefinitionName( Node ));

    Node:= Node.NextBrother;
    ParseSpecializeParams( Parent, Node );
  end;

  procedure ParseVariable( Parent: TNode; Node: TCodeTreeNode );
  begin
    case Node.Desc of
      ctnVarDefinition: ExtractVar( Parent, Node );
    else
      NotSupported( Parent, Node ); // Unexpected node type. If this function is called it means there is a bug in this code.
    end;
  end;

  procedure ParseConstant( Parent: TNode; Node: TCodeTreeNode );
  begin
    case Node.Desc of
      ctnConstDefinition: ParseConst( Parent, Node );
    else
      NotSupported( Parent, Node ); // Unexpected node type. If this function is called it means there is a bug in this code.
    end;
  end;

  procedure ExtractType( Parent: TNode; Node: TCodeTreeNode );
  begin
    case Node.Desc of
      ctnEnumerationType: ParseEnumeration( Parent, Node );
      ctnClassOfType: ParseTypeReference( Parent, Node.FirstChild );
      ctnProcedureType: ParseProcedure( Parent, Node );
      ctnSetType: ParseSet( Parent, Node );
      ctnSpecialize: ParseSpecialize( Parent, Node );
      ctnRangedArrayType: ParseRangedArray( Parent, Node );
      ctnOpenArrayType: ParseOpenArray( Parent, Node );
      ctnPointerType: ParseTypeReference( Parent, Node.FirstChild );

      ctnClassInterface,ctnDispinterface: ParseInterface( Parent, Node );
      ctnClass,ctnObject,ctnRecordType,
        ctnObjCClass,ctnObjCCategory,ctnCPPClass,
        ctnClassHelper,ctnRecordHelper,ctnTypeHelper,
        ctnObjCProtocol: ParseClass( Parent, Node );

      ctnIdentifier: ParseTypeReference( Parent, Node );
      else
        NotSupported( Parent, Node ); // Unexpected node type. If this function is called it means there is a bug in this code.
    end;

  end;

  procedure ParseType( Parent: TNode; Node: TCodeTreeNode );
  begin
    Parent:= CreateNode( Parent, Tool.ExtractDefinitionName( Node ));
    ExtractType( Parent, Node.FirstChild );
    GetSourceParams( Parent, Node );
    CreateNode( Parent, 'NodeType', NodeDescriptionAsString( Node.FirstChild.Desc ));
  end;

  procedure ParseGenericParams( Parent: TNode; Node: TCodeTreeNode );
  begin
    Parent:= CreateNode( Parent, 'GenericParams' ).AsArray;
    LoopSimple( Parent, Node, @ParseIdent );
  end;

  procedure ParseGenericType( Parent: TNode; Node: TCodeTreeNode );
  var
    Child: TCodeTreeNode;
  begin
    Parent:= CreateNode( Parent, Tool.ExtractDefinitionName( Node ));

    Child:= Node.FirstChild;
    Child:= Child.NextBrother;
    ParseGenericParams( Parent, Child );
    Child:= Child.NextBrother;

    CreateNode( Parent, 'IsGeneric' ).AsBoolean:= True;

    if ( Assigned( Child )) then
      ExtractType( Parent, Child );
    GetSourceParams( Parent, Node );
  end;

  // TypeDefinition |
  // GenericType
  procedure ParseTypeDefinition( Parent: TNode; Node: TCodeTreeNode );
  begin
    case Node.Desc of
      ctnTypeDefinition: ParseType( Parent, Node );
      ctnGenericType: ParseGenericType( Parent, Node );
      else
        NotSupported( Parent, Node ); // Unexpected node type. If this function is called it means there is a bug in this code.
    end;
  end;

  // [ Uses Section ]
  // Loop of
  // [ Type Section |
  //   Var Section |
  //   Const Section |
  //   Proc
  // ]

  procedure LoopUnitSections( Parent: TNode; Root: TCodeTreeNode );
  var
    STypes: TNode = nil;
    SUses: TNode = nil;
    SConsts: TNode = nil;
    SVars: TNode = nil;
    SFuncs: TNode = nil;
    Child: TCodeTreeNode;
  begin
    Child:= Root.FirstChild;
    while Assigned( Child ) do begin
      case Child.Desc of
        ctnTypeSection: LoopSimple( AssertSection( Parent, STypes, 'Types', nkObject ), Child, @ParseTypeDefinition );
        ctnVarSection: LoopSimple( AssertSection( Parent, SVars, 'Vars', nkObject ), Child, @ParseVariable );
        ctnConstSection: LoopSimple( AssertSection( Parent, SConsts, 'Consts', nkObject ), Child, @ParseConstant );
        ctnUsesSection: LoopSimple( AssertSection( Parent, SUses, 'Uses' ), Child, @ParseUseUnit );
        ctnProcedure: ParseProcedure( AssertSection( Parent, SFuncs, 'Functions', nkObject ), Child );
      else
        NotSupported( Parent, Child );
      end;
      Child:= Child.NextBrother;
    end;
  end;

  // Loops through all childs of a Code Tree class node and create sub sections for types, constants, fields, methods and properties.

  procedure LoopClassMembers( Parent: TNode; Root: TCodeTreeNode );
  var
    STypes: TNode = nil;
    SConsts: TNode = nil;
    SFields: TNode = nil;
    SClassFields: TNode = nil;
    SMethods: TNode = nil;
    SProperties: TNode = nil;
    SParent: TNode = nil;
    Child: TCodeTreeNode;
  begin
    Child:= Root.FirstChild;
    while Assigned( Child ) do begin
      case Child.Desc of
        // INTERFACE
        ctnClassGUID: CreateNode( Parent, 'GUID', Tool.ExtractNode( Child, []));
        ctnClassInheritance: LoopSimple( AssertSection( Parent, SParent, 'Parent' ), Child, @ParseTypeReference );

        // CLASS VISIBILITY SECTION, RECORD OR INTERFACE
        ctnTypeSection: LoopSimple( AssertSection( Parent, STypes, 'Types', nkObject ), Child, @ParseTypeDefinition );
        ctnVarDefinition: ExtractVar( AssertSection( Parent, SFields, 'Fields', nkObject ), Child );
        ctnClassClassVar: ExtractVar( AssertSection( Parent, SClassFields, 'ClassFields', nkObject ), Child.FirstChild );
        ctnConstSection: LoopSimple( AssertSection( Parent, SConsts, 'Consts', nkObject ), Child, @ParseConstant );
        ctnProcedure: ParseProcedure( AssertSection( Parent, SMethods, 'Methods', nkObject  ), Child );
        ctnProperty: ParseProp( AssertSection( Parent, SProperties, 'Properties', nkObject  ), Child );
        ctnRecordCase: ParseRecordCase( Parent, Child );
      else
        NotSupported( Parent, Child );
      end;
      Child:= Child.NextBrother;
    end;
  end;


