program makeconfig;
uses
  CodeToolsConfig, CodeToolManager;

var
  Options: TCodeToolsOptions;
begin
  Options:=TCodeToolsOptions.Create;
  Options.LoadFromFile('../config.xml');
  Options.InitWithEnvironmentVariables;
  Options.ConfigCaches.;

  Options.FPCPath:='/home/johannes/fpcupdeluxe/fpc';
  Options.FPCSrcDir:='/home/johannes/fpcupdeluxe/fpcsrc';
//  Options.LazarusSrcDir:='/home/johannes/fpcupdeluxe/lazarus';
  CodeToolBoss.Init(Options);
  Options.SaveToFile('../config.xml');
  Options.Free;
end.

