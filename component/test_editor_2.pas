unit test_editor_2;

{$mode objfpc}{$H+}
{$assertions on}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, Menus, Math, contnrs, LCLType, ComCtrls, ActnList, Process,
  algorithm, variants;

type

  { TTestEditor2 }

  TTestEditor2 = class(TForm)
    mm1: TMainMenu;
    pm2: TPopupMenu;
    ToolBar2: TToolBar;
    ComboBox1: TComboBox;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    SortTestButton: TButton;
    ActionList1: TActionList;
    Button2, Button3: TBitBtn;
    pm3, pm5, pm6: TPopupMenu;
    SpeedButton1: TSpeedButton;
    Panel1, Panel2, Panel4: TPanel;
    GroupBox1, GroupBox2: TGroupBox;
    Button1, DeleteTestButton: TButton;
    FileListBox, OutputListBox, InputListBox: TListBox;
    ToolButton3, ToolButton4, ToolButton5: TToolButton;
    TestlistSort2, TestlistSort3, TestlistSort1: TAction;
    mi10201, mi10202, mi10203, mi204, mi205, mi206: TMenuItem;
    FilelistRem, FilelistAddOIOI, FilelistAddIOIO: TAction;
    FilelistAddOutput, FilelistSort4, FilelistAddInput: TAction;
    FilelistSort3, FilelistSort2, FilelistSort1: TAction;
    mi10107, mi10108, mi10109, mi606, mi605, mi601, mi602: TMenuItem;
    mi603, mi604, mi10105, mi10106, mi501, mi502, mi503: TMenuItem;
    mi504, mi101, mi102, mi10101, mi10102, mi10103, mi10104: TMenuItem;
    mi12, mi13, mi14, mi15, mi16, mi17, mi18: TMenuItem;
    procedure Button4Click(Sender: TObject);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DeleteTestButtonClick(Sender: TObject);
    procedure FilelistRemExecute(Sender: TObject);
    procedure FilelistSortExecute(Sender: TObject);
    procedure FilelistAddExecute(Sender: TObject);
    procedure mi12Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure DeleteFileButtonClick(Sender: TObject);
    procedure InputListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ComboBox1Go(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SortTestButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TestlistSortExecute(Sender: TObject);
  private
    FDeleting: TObjectList;
    function Compare5(x, y: Integer): Boolean;
    function Compare6(x, y: Integer): Boolean;
    function Compare7(x, y: Integer): Boolean;
    function DeleteLater(Sender: TStringList): TStringList;
    function FindAllFiles(SearchPath: String; Depth: Integer): TStringList;
    procedure Pack(List: TStrings);
    function SmartImport: Boolean;
    function TestCount: Integer;
    function IList: TStrings;
    function OList: TStrings;
  public
    procedure LoadControls(AInputList, AOutputList: TStrings);
    procedure SaveControls(AInputList, AOutputList: TStrings);
    function Execute(AInputList, AOutputList: TStrings): TModalResult;
    class function DefaultExecute(AInputList, AOutputList: TStrings): TModalResult;
  end;

implementation

{$R *.lfm}

function CustomCompare1(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(List[Index1], List[Index2]);
end;

function CustomCompare2(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Length(List[Index1]) - Length(List[Index2]);
  if Result = 0 then
  Result := CompareStr(List[Index1], List[Index2]);
end;

function CustomCompare3(List: TStringList; Index1, Index2: Integer): Integer;

  function Filter(S: String): String;
  var
    c: Char;
  begin
    Result := '';
    for c in S do
    if not (c in ['0'..'9']) then
    Result += c;
  end;

begin
  Result := CompareStr(Filter(List[Index1]), Filter(List[Index2]));
  if Result = 0 then
  Result := CompareStr(List[Index1], List[Index2]);
end;

function CustomCompare4(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareStr(ExtractFileExt(List[Index1]), ExtractFileExt(List[Index2]));
  if Result = 0 then
  Result := CompareStr(List[Index1], List[Index2]);
end;

procedure CustomSort(List: TStrings; CompareFn: TStringListSortCompare);
var SList: TStringList;
begin
  SList := TStringList.Create;
  try
    SList.Assign(List);
    SList.CustomSort(CompareFn);
    List.Assign(SList);
    finally SList.Free;
  end;
end;

{ TTestEditor2 }

function TTestEditor2.FindAllFiles(SearchPath: String; Depth: Integer): TStringList;
var
  DirList: TStrings;
  Start, Finish, i: Integer;
begin
  Result := TStringList.Create;
  DirList := TStringList.Create;
  try
    DirList.Add(SearchPath);
    Start:=0; Finish:=1;
    for Depth := 1 to Depth do
    begin
      for i := Start to Finish-1 do
      DirList.AddStrings(DeleteLater(FindAllDirectories(DirList[i], False)));
      Start := Finish; Finish := DirList.Count;
      FDeleting.Clear;
    end;
    for i := 0 to DirList.Count-1 do
    Result.AddStrings(DeleteLater(FileUtil.FindAllFiles(DirList[i], '', False)));
    FDeleting.Clear;
    finally DirList.Free;
  end;
end;

procedure TTestEditor2.LoadControls(AInputList, AOutputList: TStrings);
begin
  InputListBox.Items.Assign(AInputList);
  OutputListBox.Items.Assign(AOutputList);
end;

procedure TTestEditor2.SaveControls(AInputList, AOutputList: TStrings);
begin
  AInputList.Assign(InputListBox.Items);
  AOutputList.Assign(OutputListBox.Items);
end;

function TTestEditor2.Execute(AInputList, AOutputList: TStrings): TModalResult;
begin
  LoadControls(AInputList, AOutputList);
  if AInputList.Count=0 then SpeedButton1.Click;
  Result := ShowModal;
  if Result = mrOK then
  SaveControls(AInputList, AOutputList);
end;

class function TTestEditor2.DefaultExecute(AInputList, AOutputList: TStrings
  ): TModalResult;
var
  Form: TTestEditor2;
begin
  Form := TTestEditor2.Create(nil);
  try
    Result := Form.Execute(AInputList, AOutputList);
    finally Form.Free;
  end;
end;

procedure TTestEditor2.ComboBox1Go(Sender: TObject);
begin
  if not DirectoryExists(ComboBox1.Text) then exit;
  FileListBox.Items.Assign(DeleteLater(FindAllFiles(ComboBox1.Text, 2)));
  SmartImport;
  FDeleting.Clear;
end;

procedure TTestEditor2.FilelistRemExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FileListBox.Count-1 do
  if FileListBox.Selected[i] then
  FileListBox.Items[i] := '';
  Pack(FileListBox.Items);
end;

procedure TTestEditor2.FilelistSortExecute(Sender: TObject);
begin
  if Sender = FilelistSort1 then
    CustomSort(FileListBox.Items, @CustomCompare1)
  else if Sender = FilelistSort2 then
    CustomSort(FileListBox.Items, @CustomCompare2)
  else if Sender = FilelistSort3 then
    CustomSort(FileListBox.Items, @CustomCompare3)
  else if Sender = FilelistSort4 then
    CustomSort(FileListBox.Items, @CustomCompare4)
  else
    raise Exception.Create('procedure TTestEditor2.FilelistActionExecute(Sender: TObject);');
end;

function ifthen(B:Boolean; IfTrue, IfFalse: TStrings): TStrings;
begin
  if B then
    Result := IfTrue
  else
    Result := IfFalse;
end;

procedure TTestEditor2.FilelistAddExecute(Sender: TObject);
var
  IsInpFirst, IsInpAfter: Boolean;
  Odd: Boolean = False;
  First, After: TStrings;
  i: Integer;
begin
  IsInpFirst := (Sender=FilelistAddInput) or (Sender=FilelistAddIOIO);
  IsInpAfter := (Sender=FilelistAddInput) or (Sender=FilelistAddOIOI);
  First := ifthen(IsInpFirst, InputListBox.Items, OutputListBox.Items);
  After := ifthen(IsInpAfter, InputListBox.Items, OutputListBox.Items);

  for i := 0 to FileListBox.Count-1 do
  if FileListBox.Selected[i] then
  begin
    Odd := not Odd;
    ifthen(Odd, First, After).Add(FileListBox.Items[i]);
    FileListBox.Items[i] := '';
  end;
  Pack(FileListBox.Items);
end;

procedure TTestEditor2.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then
  ComboBox1Go(Sender);
end;

procedure TTestEditor2.DeleteTestButtonClick(Sender: TObject);
begin
  pm3.PopUp;
end;

function TTestEditor2.SmartImport: Boolean;
var
  OutputString: String;
  List: TStrings;
  i: Integer;
  k: Integer;
begin
  if not RunCommand('../inputter', [ComboBox1.Text], OutputString) then
    exit(False);
  List := TStringList.Create;
  Result := False;
  try
    List.Text := OutputString;
    InputListBox.Items.Clear;
    OutputListBox.Items.Clear;
    for i := 0 to List.Count div 2-1 do
    begin
      InputListBox.Items.Add(List[i*2]);
      OutputListBox.Items.Add(List[i*2+1]);
    end;
    for i := 0 to List.Count-1 do
    begin
      k := FileListBox.Items.IndexOf(List[i]);
      if k<>-1 then FileListBox.Items.Delete(k);
    end;
    Result := True;
    finally List.Free;
  end;
end;

procedure TTestEditor2.Button4Click(Sender: TObject);
begin
  SmartImport;
end;

procedure TTestEditor2.mi12Click(Sender: TObject);
var
  i: Integer;
begin
  if (Sender=mi12) or (Sender=mi14) or (Sender=mi15) or (Sender=mi17) then
  with InputListBox do
  for i := 0 to Count-1 do
  if Selected[i] then
  begin
    if (Sender=mi15) or (Sender=mi17) then
      FileListBox.Items.Add(Items[i]);
    Items[i] := '';
  end;

  if (Sender=mi13) or (Sender=mi14) or (Sender=mi16) or (Sender=mi17) then
  with OutputListBox do
  for i := 0 to Count-1 do
  if Selected[i] then
  begin
    if (Sender=mi16) or (Sender=mi17) then
      FileListBox.Items.Add(Items[i]);
    Items[i] := '';
  end;

  Pack(InputListBox.Items);
  Pack(OutputListBox.Items);
end;

procedure TTestEditor2.MenuItem15Click(Sender: TObject);
begin

end;

procedure TTestEditor2.Pack(List: TStrings);
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to List.Count-1 do
  if List[i]<>'' then
  begin List[j] := List[i]; j += 1; end;
  while List.Count > j do List.Delete(List.Count-1);
end;

procedure TTestEditor2.DeleteFileButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FileListBox.Count-1 do
  if FileListBox.Selected[i] then
  FileListBox.Items[i] := '';
  Pack(FileListBox.Items);
end;

function ifthen(B: Boolean; IfTrue, IfFalse: TObject): TObject;
begin
  if B then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function ifthen(B: Boolean; IfTrue, IfFalse: Integer): Integer;
begin
  if B then
    Result := IfTrue
  else
    Result := IfFalse;
end;

procedure TTestEditor2.InputListBoxSelectionChange(Sender: TObject; User: boolean);
var
  A, B: TListBox;
  i: Integer;
begin
  A := TListBox(Sender);
  B := ifthen(Sender=InputListBox, OutputListBox, InputListBox) as TListBox;
  B.ItemIndex := ifthen(A.ItemIndex < B.Count, A.ItemIndex, -1);
  for i := 0 to A.Items.Count-1 do
    if (i<B.Items.Count) and A.Selected[i] then
      B.Selected[i] := True;
end;

procedure TTestEditor2.FormCreate(Sender: TObject);
begin
  FDeleting := TObjectList.Create(True);
  ComboBox1.Text := '';
end;

procedure TTestEditor2.FormDestroy(Sender: TObject);
begin
  FDeleting.Free;
end;

procedure TTestEditor2.SortTestButtonClick(Sender: TObject);
begin
  pm2.PopUp;
end;

procedure TTestEditor2.SpeedButton1Click(Sender: TObject);
var S: String;
begin
  if SelectDirectory('', ComboBox1.Text, S) then
  ComboBox1.Text := S;
  ComboBox1Go(Sender);
end;

function TTestEditor2.TestCount: Integer;
begin
  Result := Min(InputListBox.Items.Count, OutputListBox.Items.Count);
end;

function TTestEditor2.IList: TStrings;
begin
  Result := InputListBox.Items;
end;

function TTestEditor2.OList: TStrings;
begin
  Result := OutputListBox.Items;
end;

function TTestEditor2.Compare5(x, y: Integer): Boolean;
begin
  Result := InputListBox.Items[x] < InputListBox.Items[y];
end;

function TTestEditor2.Compare6(x, y: Integer): Boolean;
begin
  if Length(IList[x]) <> Length(IList[y]) then
    Result := Length(IList[x]) < Length(IList[y])
  else if Length(OList[x]) <> Length(OList[y]) then
    Result := Length(OList[x]) < Length(OList[y])
  else
    Result := IList[x] < IList[y];
end;

function TTestEditor2.Compare7(x, y: Integer): Boolean;
var
  XSize, YSize: Integer;
begin
  XSize := FileSize(IList[x]) + FileSize(OList[x]);
  YSize := FileSize(IList[y]) + FileSize(OList[y]);
  Result := XSize < YSize;
end;

procedure TTestEditor2.TestlistSortExecute(Sender: TObject);
var
  Index: array of Integer;
  Compare: TCompare;
begin
  if Sender=TestlistSort1 then
    Compare := @Compare5
  else if Sender=TestlistSort2 then
    Compare := @Compare6
  else if Sender=TestlistSort3 then
    Compare := @Compare7
  else
    Assert(False);

  CreateIndexList(Index, TestCount);
  try
    Sort(Index, Compare);
    ApplyPermutation(InputListBox.Items, Index);
    ApplyPermutation(OutputListBox.Items, Index);
    finally SetLength(index, 0);
  end;
end;

function TTestEditor2.DeleteLater(Sender: TStringList): TStringList;
begin
  FDeleting.Add(Sender);
  Result := Sender;
end;

end.

