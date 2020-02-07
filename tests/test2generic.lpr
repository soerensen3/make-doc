program test2generic;

type

  { TTestGenericClass }

  generic TTestGenericClass < T > = class ( TObject )
    procedure Test;
    function TestFunc: T;
  end;

  {generic TTestGenericRecord < T > = record
    a: Integer;
    b: Boolean;
  end;}

  {generic ITestGenericInterface < T > = interface
    ['{9EB88F40-F533-4F27-A8F9-730721D786AF}']
    procedure Test;
  end;}

  generic TTestGenericProc < T > = procedure ( a: T );

{ TTestGenericClass }

procedure TTestGenericClass.Test;
begin

end;

function TTestGenericClass.TestFunc: T;
begin

end;


//type
//  TestProcTObject = specialize TTestGenericProc < TObject >;


var
  TestGenericClass: specialize TTestGenericClass < TObject >;
  //TestGenericRecord: specialize TTestGenericRecord < TObject >;
  //TestGenericIntf: specialize ITestGenericInterface < TObject >;
  //TestGenericProcVar: specialize TTestGenericProc < TObject >;

begin

end.

