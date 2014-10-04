unit main_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterCpp, SynMemo,
  synhighlighterunixshellscript, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, Unix, Math, contnrs, LCLIntf, Buttons,
  LazSynEditText, test_editor_2, popup_frame, simple_file_dialog, status_list;

const
  SolFile = 'solution.cpp';
  CompileFile = './compile.sh';
  ExecuteFile = './execute.sh';
  ExecutableFile = './solution';
  JudgeFile = './judge.sh';

  SolDir = 'saved-solution/';
  CompileDir = 'saved-compile/';
  ExecuteDir = 'saved-execute/';
  JudgeDir = 'saved-judge/';

type

  { TSynEditEx }

  TSynEditEx = class helper for TSynEdit // Thank user3661500
    function GetTextBuffer: TSynEditStrings;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    actCompile: TAction;
    actSave: TAction;
    actOpen: TAction;
    actJudgeAll: TAction;
    actTestEditor: TAction;
    actJudge: TAction;
    actRun: TAction;
    ActionList1: TActionList;
    Cham: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    ThemTest: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    BienDich: TButton;
    Chay: TButton;
    ImageList1: TImageList;
    Label3: TLabel;
    OutputMemo: TMemo;
    ExecuteEdit: TSynEdit;
    SynUNIXShellScriptSyn1: TSynUNIXShellScriptSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    CompileEdit: TSynEdit;
    JudgeEdit: TSynEdit;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    TestListBox: TListBox;
    LibFileListBox: TListBox;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynCppSyn1: TSynCppSyn;
    SolEdit: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure actCompileExecute(Sender: TObject);
    procedure actJudgeAllExecute(Sender: TObject);
    procedure actJudgeExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actTestEditorExecute(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure CompileEditChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure SomethingChange(Sender: TObject);
  private
    FStatus: TStatusList;
    FInputList, FOutputList: TStrings;
    FDeleting: TObjectList;
    procedure LoadAllFiles;
    function NotifyIf(Sender: TObject; Event: TNotifyEvent; B: Boolean
      ): Boolean;
    procedure PackStrings(List: TStrings);
    procedure SaveAllFiles;
    function ExternalExecute(Executable, Parameters: String): Integer;
    function SaveFileIf(AControl: TSynEdit; AFileName, AStatus: String): Boolean;
    procedure SetCount(List: TStrings; ACount: Integer);
    function TryLoadFile(List: TStrings; AFileName: String): Boolean;
    function TrySaveFile(List: TStrings; AFileName: String): Boolean;
    procedure UpdateControls(Sender: TObject);
    function DeleteLater(List: TStringList): TStringList;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

operator ** (A, B: String) StarStar: String;
begin
  Result := ConcatPaths([A, B]);
end;

{ TSynEditEx }

function TSynEditEx.GetTextBuffer: TSynEditStrings;
begin
  Result := TextBuffer;
end;

{ TMainForm }

procedure TMainForm.actCompileExecute(Sender: TObject);
var
  Args: String;
begin
  SaveAllFiles;
  Args := Format('"%s" "%s"', [SolFile, ExecutableFile]);
  if ExternalExecute(CompileFile, Args) = 0 then
    FStatus.Add('compiled');
end;

procedure TMainForm.actJudgeAllExecute(Sender: TObject);
var
  TestCount: Integer;
  Error: Integer=0;
  i: Integer;
  Args: String;
begin
  SaveAllFiles;
  if FStatus.Was('NOT compiled') then
    actCompile.Execute;
  if FStatus.Was('NOT compiled') then
    exit;

  TestCount := Min(FInputList.Count, FOutputList.Count);
  for i := 0 to TestCount-1 do
  begin
    Args := Format('"%s" "%s" "%s"', [ExecutableFile, FInputList[i], FOutputList[i]]);
    Error := ExternalExecute(JudgeFile, Args);
    if Error<>0 then break;
  end;
end;

procedure TMainForm.actJudgeExecute(Sender: TObject);
var
  Index: Integer;
  Args: String;
begin
  SaveAllFiles;
  Index := TestListBox.ItemIndex;
  if Index = -1 then exit;
  if FStatus.Was('NOT compiled') then
    actCompile.Execute;
  if FStatus.Was('NOT compiled') then
    exit;
  Args := Format('"%s" "%s" "%s"', [ExecutableFile, FInputList[Index], FOutputList[Index]]);
  ExternalExecute(JudgeFile, Args);
end;

procedure TMainForm.actOpenExecute(Sender: TObject);
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    ForceDirectories(SolDir);
    Dialog.InitialDir:=SolDir;
    if Dialog.Execute then
    begin
      if TryLoadFile(SolEdit.Lines, Dialog.FileName) then
        SolEdit.OnChange(SolEdit)
//        FStatus.Add('solution file changed') // be careful
      else
        ShowMessage('Load file failed');
    end;
    finally Dialog.Free;
  end;
end;

procedure TMainForm.actRunExecute(Sender: TObject);
var
  Args: String;
begin
  SaveAllFiles;
  if FStatus.Was('NOT compiled') then
    actCompile.Execute;
  if FStatus.Was('NOT compiled') then
    exit;
  Args := Format('"%s"', [ExecutableFile]);
  ExternalExecute(ExecuteFile, Args);
end;

procedure TMainForm.actSaveExecute(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  Dialog := TSaveDialog.Create(nil);
  try
    ForceDirectories(SolDir);
    Dialog.InitialDir:=SolDir;
    if Dialog.Execute then
    begin
      if not TrySaveFile(SolEdit.Lines, Dialog.FileName) then
        ShowMessage('Save file failed');
    end;
    finally Dialog.Free;
  end;
end;

procedure TMainForm.actTestEditorExecute(Sender: TObject);
begin
  if TTestEditor2.DefaultExecute(FInputList, FOutputList) = mrOK then
  UpdateControls(TestListBox);
end;

procedure TMainForm.Button11Click(Sender: TObject);
var
  S: String;
begin
  with TOpenDialog.Create(nil) do
  try
    Options:=Options+[ofAllowMultiSelect];
    if Execute then
    begin
      for S in Files do
      if DirectoryExists(S) then
      CopyDirTree(S, GetCurrentDir ** '/', [cffOverwriteFile, cffCreateDestDirectory])
      else CopyFile(S, GetCurrentDir ** ExtractFileName(S), [cffOverwriteFile, cffCreateDestDirectory]);
    end;
    Button13.Click;
    finally Free;
  end;
end;

procedure TMainForm.PackStrings(List: TStrings);
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to List.Count-1 do
  if List[i]<>'' then
  begin List[j] := List[i]; j += 1; end;
  while List.Count > j do List.Delete(List.Count-1);
end;

procedure TMainForm.Button12Click(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to LibFileListBox.Count-1 do
  if LibFileListBox.Selected[i] then
  DeleteFile(LibFileListBox.Items[i]);
  Button13.Click;
end;

procedure TMainForm.Button13Click(Sender: TObject);
begin
  LibFileListBox.Items.Assign(DeleteLater(FindAllFiles('')));
end;

procedure TMainForm.Button14Click(Sender: TObject);
begin
  OpenDocument('.');
end;

procedure TMainForm.CompileEditChangeUpdating(ASender: TObject;
  AnUpdating: Boolean);
begin

end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FInputList := TStringList.Create;
  FOutputList := TStringList.Create;
  FDeleting := TObjectList.Create(True);
  FStatus := TStatusList.Create(Self);
  SolEdit.GetTextBuffer.AddNotifyHandler(senrCleared, @SomethingChange);
  CompileEdit.GetTextBuffer.AddNotifyHandler(senrCleared, @SomethingChange);
  ExecuteEdit.GetTextBuffer.AddNotifyHandler(senrCleared, @SomethingChange);
  JudgeEdit.GetTextBuffer.AddNotifyHandler(senrCleared, @SomethingChange);
  ForceDirectories('lib');
  ChDir('lib');
  LoadAllFiles;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FInputList.Free;
  FOutputList.Free;
  FDeleting.Free;
  ChDir('..');
end;

procedure TMainForm.Label1Click(Sender: TObject);
var
  Dialog: TSimpleFileDialog;
  Ctrl: TSynEdit;
  ADir: String;
begin
  if Sender=Label1 then
    begin Ctrl:=CompileEdit; ADir:=CompileDir; end
  else if Sender=Label2 then
    begin Ctrl:=ExecuteEdit; ADir:=ExecuteDir; end
  else if Sender=Label4 then
    begin Ctrl:=JudgeEdit; ADir:=JudgeDir; end
  else
    RunError(123);

  Dialog := TSimpleFileDialog.Create(nil, ADir, Ctrl.Lines);
  if Dialog.ShowModal=mrOK then
    {Ctrl.OnChange(Ctrl)};
  Dialog.Free;
end;

procedure TMainForm.SomethingChange(Sender: TObject);
var
  S: TComponentName;
begin
  if (Sender=SolEdit) or (Sender=SolEdit.GetTextBuffer) then
    FStatus.Add(['solution file changed', 'NOT compiled'])
  else if (Sender=CompileEdit) or (Sender=CompileEdit.GetTextBuffer) then
    FStatus.Add(['compile file changed', 'NOT compiled'])
  else if (Sender=ExecuteEdit) or (Sender=ExecuteEdit.GetTextBuffer) then
    FStatus.Add('execute file changed')
  else if (Sender=JudgeEdit) or (Sender=JudgeEdit.GetTextBuffer) then
    FStatus.Add('judge file changed')
  else
    begin
      S := TObject(Sender).ClassName;
      raise Exception.Create(S);
    end;
end;

function TMainForm.TrySaveFile(List: TStrings; AFileName: String): Boolean;
begin
  Result := True;
  try
    List.SaveToFile(AFileName);
  except
    on E: Exception do
    Result := False;
  end;
end;

function TMainForm.SaveFileIf(AControl: TSynEdit; AFileName, AStatus: String): Boolean;
begin
  Result := FStatus.Was(AStatus);
  if Result then
  begin
    AControl.Lines.SaveToFile(AFileName);
    FStatus.Add('NOT '+AStatus);
  end;
end;

procedure TMainForm.SaveAllFiles;
begin
  SaveFileIf(SolEdit, SolFile, 'solution file changed');
  SaveFileIf(CompileEdit, CompileFile, 'compile file changed');
  SaveFileIf(ExecuteEdit, ExecuteFile, 'execute file changed');
  SaveFileIf(JudgeEdit, JudgeFile, 'judge file changed');
end;

function TMainForm.TryLoadFile(List: TStrings; AFileName: String): Boolean;
begin
  Result := FileExists(AFileName);
  if Result then
    try
      List.LoadFromFile(AFileName);
    except
      on E: Exception do
      Result := False;
    end
end;

function TMainForm.NotifyIf(Sender: TObject; Event: TNotifyEvent; B: Boolean): Boolean;
begin
  if B and Assigned(Event) then
  Event(Sender);
  Result := B;
end;

procedure TMainForm.LoadAllFiles;
begin
  FStatus.AddIf(['solution file changed', 'NOT compiled'],
    not TryLoadFile(SolEdit.Lines, SolFile));

  FStatus.AddIf(['compile file changed', 'NOT compiled'],
    not TryLoadFile(CompileEdit.Lines, CompileFile));

  NotifyIf(ExecuteEdit, ExecuteEdit.OnChange,
    not FStatus.AddIf('execute file changed',
    not TryLoadFile(ExecuteEdit.Lines, ExecuteFile)));

  NotifyIf(JudgeEdit, JudgeEdit.OnChange,
    not FStatus.AddIf('judge file changed',
    not TryLoadFile(JudgeEdit.Lines, JudgeFile)));
end;

function TMainForm.ExternalExecute(Executable, Parameters: String): Integer;
begin
  fpSystem(Format('chmod a+x "%s"', [Executable]));
  Result := fpSystem(Format('"%s" %s 1> shell_output 2>&1', [Executable, Parameters]));
  OutputMemo.Lines.LoadFromFile('shell_output');
  DeleteFile('shell_output');
  OutputMemo.Invalidate;
  Label3.Caption:=Format('Exit code: %d', [Result]);
end;

procedure TMainForm.SetCount(List: TStrings; ACount: Integer);
begin
  while List.Count < ACount do List.Add('');
  while List.Count > ACount do List.Delete(List.Count-1);
end;

procedure TMainForm.UpdateControls(Sender: TObject);
var
  i: Integer;
begin
  if Sender = TestListBox then
  with TestListBox do
  begin
    SetCount(Items, Min(FInputList.Count, FOutputList.Count));
    for i := 0 to Items.Count-1 do
    Items[i] := Format('Test %d: %s, %s', [i, ExtractFileName(FInputList[i]),
      ExtractFileName(FOutputList[i])]);
  end
  else
    raise Exception.Create('Sorry');
end;

function TMainForm.DeleteLater(List: TStringList): TStringList;
begin
  FDeleting.Add(List);
  Result := List;
end;

end.

