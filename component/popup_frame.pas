unit popup_frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, SynEdit;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    Button1: TButton;
    GroupBox1: TGroupBox;
    LoadBtn: TButton;
    SaveBtn: TButton;
    DeleteBtn: TButton;
    FileEdit: TComboBox;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FileEditChange(Sender: TObject);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FEditor: TSynEdit;
    FSearchDir: String;
    procedure ReloadDir(Dir: String);
  public
    procedure Show(Editor: TSynEdit; SearchDir: String);
  end;

implementation

{$R *.lfm}

operator ** (A, B: String) StarStar: String;
begin
  Result := ConcatPaths([A, B]);
end;

{ TFrame1 }

procedure TFrame1.ReloadDir(Dir: String);
var
  List: TStringList;
  i: Integer;
begin
  List := FindAllFiles(Dir, '', False);
  for i := 0 to List.Count-1 do
  List[i] := ExtractFileName(List[i]);
  FileEdit.Items := List;
  List.Free;
end;

procedure TFrame1.SaveBtnClick(Sender: TObject);
begin
  ForceDirectories(FSearchDir);
  FEditor.Lines.SaveToFile(FSearchDir ** FileEdit.Text);
  FileEdit.Text := '';
  ReloadDir(FSearchDir);
end;

procedure TFrame1.LoadBtnClick(Sender: TObject);
begin
  FEditor.Lines.LoadFromFile(FSearchDir ** FileEdit.Text);
  FileEdit.Text := '';
end;

procedure TFrame1.DeleteBtnClick(Sender: TObject);
begin
  DeleteFile(FSearchDir ** FileEdit.Text);
  FileEdit.Text := '';
  ReloadDir(FSearchDir);
end;

procedure TFrame1.Button1Click(Sender: TObject);
begin
  Hide;
end;

procedure TFrame1.FileEditChange(Sender: TObject);
begin
  SaveBtn.Enabled := FileEdit.Text<>'';
  DeleteBtn.Enabled := (FileEdit.Text<>'') and FileExists(FSearchDir**FileEdit.Text);
  LoadBtn.Enabled := DeleteBtn.Enabled;
end;

procedure TFrame1.Show(Editor: TSynEdit; SearchDir: String);
begin
  inherited Show;
  FEditor := Editor;
  FSearchDir := SearchDir;
  ReloadDir(SearchDir);
  FileEdit.Text:='';
end;

end.

