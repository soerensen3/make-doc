unit uLazMakeCTConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics, 
    Dialogs,
  LCLType, IDECommands, IDEWindowIntf, LazIDEIntf, MenuIntf, CodeToolManager;

procedure SaveCTConfig(Sender: TObject );
procedure Register; // Check the "Register Unit" of this unit in the package editor.implementation

var
  SaveDlg: TSaveDialog = nil;

implementation

procedure SaveCTConfig(Sender: TObject );
begin
  if ( SaveDlg.Execute ) then
    CodeToolBoss.CompilerDefinesCache.SaveToFile( SaveDlg.FileName );
end;

procedure Register;
var
  CmdCatViewMenu: TIDECommandCategory;
  SaveCTConfigCommand: TIDECommand;
  MenuItemCaption: String;
begin
  // register shortcut and menu item
  MenuItemCaption:='Save CodeTools config'; // <- this caption should be replaced by a resourcestring
  // search shortcut category
  CmdCatViewMenu:=IDECommandList.FindCategoryByName(CommandCategoryViewName);
  // register shortcut
  SaveCTConfigCommand:=RegisterIDECommand(CmdCatViewMenu,
    'Save CodeTools config',
    MenuItemCaption,
    IDEShortCut(VK_UNKNOWN, []), // <- set here your default shortcut
    CleanIDEShortCut, nil, @SaveCTConfig);
  // register menu item in View menu
  RegisterIDEMenuCommand(itmSecondaryTools,
    'Save CodeTools config',
    MenuItemCaption, nil, nil, SaveCTConfigCommand);
  SaveDlg:= TSaveDialog.Create( nil );
  SaveDlg.Filter:= '.xml';
end;

finalization
  FreeAndNil( SaveDlg );

end.

