unit status_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStatusList }

  TStatusList = class (TComponent)
  private
    FList: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(S: String);
    procedure Rem(S: String);
    procedure Add(S: array of String);
    procedure Rem(S: array of String);
    function Was(S: String): Boolean;
    function AddIf(S: String; B: Boolean): Boolean;
    function AddIf(S: array of String; B: Boolean): Boolean;
  end;

implementation

{ TStatusList }

constructor TStatusList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TStringList.Create;
end;

destructor TStatusList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TStatusList.Add(S: String);
begin
  if LeftStr(S, 4) = 'NOT ' then
    Rem(Copy(S, 5, MaxInt))
  else if not Was(S) then
    FList.Add(S);
end;

procedure TStatusList.Rem(S: String);
begin
  if LeftStr(S, 4) = 'NOT ' then
    Add(Copy(S, 5, MaxInt))
  else if Was(S) then
    FList.Delete(FList.IndexOf(S));
end;

procedure TStatusList.Add(S: array of String);
var
  p: String;
begin
  for p in S do
  Add(p);
end;

procedure TStatusList.Rem(S: array of String);
var
  p: String;
begin
  for p in S do
  Rem(p);
end;

function TStatusList.AddIf(S: String; B: Boolean): Boolean;
begin
  if B then
    Add(S);
  Result := B;
end;

function TStatusList.AddIf(S: array of String; B: Boolean): Boolean;
var
  p: String;
begin
  if B then
  for p in S do
    Add(S);
  Result := B;
end;

function TStatusList.Was(S: String): Boolean;
begin
  if LeftStr(S, 4)='NOT ' then
    Result := not Was(Copy(S, 5, MaxInt))
  else
    Result := FList.IndexOf(S) <> -1;
end;

end.

