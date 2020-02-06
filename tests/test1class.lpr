program test1class;

type

  { TTestClass }

  // This is a test class
  TTestClass = class 
    private
      FIpriv: Integer; // private integer field
      FBpriv: Boolean; // private boolean field

      procedure TestProcPriv( A: Integer ); // private procedure
      function TestFuncPriv( A: Integer ): String; // private function
      class function TestClassFuncPriv( A: Integer ): String; // private class function

      property Ipriv: Integer read FIpriv write FIpriv; // private property
      property Bpriv: Integer read FBpriv write FBpriv;

    protected
      FIprot: Integer; 
      FBprot: Boolean;

      procedure TestProcProt( A: Integer );
      function TestFuncProt( A: Integer ): String;
      class function TestClassFuncProt( A: Integer ): String;

      property Iprot: Integer read FIprot write FIprot;
      property Bprot: Integer read FBprot write FBprot;

    public
      FIpub: Integer;
      FBpub: Boolean;

      procedure TestProcPub( A: Integer );
      function TestFuncPub( A: Integer ): String;
      class function TestClassFuncPub( A: Integer ): String;

      constructor Create;
      destructor Destroy;

      property Ipub: Integer read FIpub write FIpub;
      property Bpub: Integer read FBpub write FBpub;


    published
      FIpublished: Integer;
      FBpublished: Boolean;

      procedure TestProcPublished( A: Integer );
      function TestFuncPublished( A: Integer ): String;
      class function TestClassFuncPublished( A: Integer ): String;

      property Ipublished: Integer read FIpublished write FIpublished;
      property Bpublished: Integer read FBpublished write FBpublished;
  end;

  { TTestClass2 }

  TTestClass2 = class ( TTestClass )

  end;

  ITest = interface
    ['{9FE7CF66-F9C8-4AC6-BB2D-6D4DA10EBBBD}']
    procedure TestIntfProc;
  end;

  { TTestClass3 }

  TTestClass3 = class ( TTestClass2, ITest )
    procedure TestIntfProc;
  end;

{ TTestClass3 }

procedure TTestClass3.TestIntfProc;
begin

end;

{ TTestClass }

procedure TTestClass.TestProcPriv(A: Integer);
begin

end;

function TTestClass.TestFuncPriv(A: Integer): String;
begin

end;

class function TTestClass.TestClassFuncPriv(A: Integer): String;
begin

end;

procedure TTestClass.TestProcProt(A: Integer);
begin

end;

function TTestClass.TestFuncProt(A: Integer): String;
begin

end;

class function TTestClass.TestClassFuncProt(A: Integer): String;
begin

end;

procedure TTestClass.TestProcPub(A: Integer);
begin

end;

function TTestClass.TestFuncPub(A: Integer): String;
begin

end;

class function TTestClass.TestClassFuncPub(A: Integer): String;
begin

end;

constructor TTestClass.Create;
begin

end;

destructor TTestClass.Destroy;
begin

end;

procedure TTestClass.TestProcPublished(A: Integer);
begin

end;

function TTestClass.TestFuncPublished(A: Integer): String;
begin

end;

class function TTestClass.TestClassFuncPublished(A: Integer): String;
begin

end;

begin
end.

