unit simple_file_dialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type

  { TSimpleFileDialog }

  TSimpleFileDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    FDir: String;
    FList: TStrings;
  public
    constructor Create(AOwner: TComponent; ADir: String; AList: TStrings);
  end;

implementation

{$R *.lfm}

operator ** (A, B: String) StarStar: String;
begin
  Result := ConcatPaths([A, B]);
end;

{ TSimpleFileDialog }

procedure TSimpleFileDialog.FormShow(Sender: TObject);
var
  List: TStringList;
  i: Integer;
begin
  List := FindAllFiles(FDir, '', False);
  for i := 0 to List.Count-1 do
  List[i] := ExtractFileName(List[i]);
  ListBox1.Items.Assign(List);
  List.Free;
end;

procedure TSimpleFileDialog.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemAtPos(ListBox1.ScreenToClient(Mouse.CursorPos), True) <> -1 then
  Button1.Click;
end;

procedure TSimpleFileDialog.Button2Click(Sender: TObject);
begin
  try
    ForceDirectories(FDir);
    FList.SaveToFile(FDir ** Edit1.Text);
  except
    on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TSimpleFileDialog.Button1Click(Sender: TObject);
begin
  try
    FList.LoadFromFile(FDir ** Edit1.Text);
  except
    on E: Exception do
    ShowMessage(E.Message);
  end;
end;

procedure TSimpleFileDialog.ListBox1SelectionChange(Sender: TObject;
  User: boolean);
begin
  Edit1.Text := ListBox1.Items[ListBox1.ItemIndex];
end;

constructor TSimpleFileDialog.Create(AOwner: TComponent; ADir: String;
  AList: TStrings);
var
  S: String;
  i: Integer;
begin
  inherited Create(AOwner);
  FDir := ADir;
  FList := AList;
  S := DateTimeToStr(Now);
  for i := 1 to Length(S) do
  if not (S[i] in ['0'..'9', 'a'..'z', 'A'..'Z']) then
  S[i] := '_';
  Edit1.Text:=S;
  Edit1.SelectAll;
end;

end.

