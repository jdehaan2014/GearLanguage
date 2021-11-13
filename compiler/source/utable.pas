unit uTable;

{$mode delphi}{$H+}
{$POINTERMATH ON}

// Based on:
// http://craftinginterpreters.com/hash-tables.html

interface

uses
  SysUtils, uValue;

const
  TABLE_MAX_LOAD = 0.75;

type

  PEntry = ^TEntry;
  TEntry = record
    Key: Pointer; //PObjString;
    Value: TValue;
  end;


  PTable = ^TTable;
  TTable = record
    //private
    //  Capacity: LongInt;
    public
      Count: LongInt;
      Capacity: LongInt;
      Entries: PEntry;
      procedure Init;
      procedure Free;
      function GetValue(Key: Pointer; var Value: TValue): Boolean;
      function Retrieve(const Key: String): TValue;
      function SetValue(Key: Pointer; Value: TValue): Boolean;
      procedure Store(const Key: String; Value: TValue);
      function Delete(Key: Pointer): Boolean;
      procedure AddAll(TableFrom: TTable);
      function FindString(Chars: PChar; Length: Integer; Hash: LongWord): Pointer;
    private
      function FindEntry(Entries: PEntry; const Capacity: LongInt; Key: Pointer): PEntry;
      procedure AdjustCapacity(const NewCapacity: LongInt);

  end;


procedure TableRemoveWhite(Table: PTable);
procedure MarkTable(Table: PTable);

implementation
uses uMemory, uObject;

procedure TableRemoveWhite(Table: PTable);
var
  i: Integer;
  Entry: PEntry;
begin
  for i := 0 to Table^.Capacity-1 do
    begin
      Entry := @Table^.Entries[i];
      if (Entry^.Key <> Nil) and (not PObjString(Entry^.Key)^.Obj.isMarked) then
        Table^.Delete(Entry^.Key);
    end;
end;

procedure MarkTable(Table: PTable);
var
  i: Integer;
  Entry: PEntry;
begin
  for i := 0 to Table^.Capacity do
    begin
      Entry := @Table^.Entries[i];
      MarkObject(PObj(Entry^.Key));
      MarkValue(Entry^.Value);
    end;
end;

{ TTable }

procedure TTable.Init;
begin
  Count := 0;
  Capacity := -1;
  Entries := Nil;
end;

procedure TTable.Free;
begin
  Reallocate(Entries, SizeOf(TEntry)*(Capacity + 1), 0); // Free_Array
  Init;
end;

function TTable.GetValue(Key: Pointer; var Value: TValue): Boolean;
var
  Entry: PEntry;
begin
  if Count = 0 then Exit(False);

  Entry := FindEntry(Entries, Capacity, PObjString(Key));
  if Entry^.Key = Nil then Exit(False);

  Value := Entry^.Value;
  Result := True;
end;

function TTable.Retrieve(const Key: String): TValue;
var
  Entry: PEntry;
begin
  if Count = 0 then Exit(NilVal);

  Entry := FindEntry(Entries, Capacity, CopyString(Key));
  if Entry^.Key = Nil then Exit(NilVal);

  Result := Entry^.Value;
end;

function TTable.SetValue(Key: Pointer; Value: TValue): Boolean;
var
  Entry: PEntry;
  isNewKey: Boolean;
  NewCapacity: LongInt;
begin
  if Count + 1 > (Capacity + 1) * TABLE_MAX_LOAD then
    begin
      NewCapacity := Grow_Capacity(Capacity + 1) - 1;
      AdjustCapacity(NewCapacity);
    end;
  Entry := FindEntry(Entries, Capacity, PObjString(Key));
  isNewKey := Entry^.Key = Nil;
  if isNewKey and Entry^.Value.isNil then Inc(Count);
  Entry^.Key := Key;
  Entry^.Value := Value;
  Exit(isNewKey);
end;

procedure TTable.Store(const Key: String; Value: TValue);
begin
  SetValue(CopyString(Key), Value);
end;


function TTable.Delete(Key: Pointer): Boolean;
var
  Entry: PEntry;
begin
  if Count = 0 then Exit(False);

  // find the entry
  Entry := FindEntry(Entries, Capacity, PObjString(Key));
  if Entry^.Key = Nil then Exit(False);

  // Place a tombstone in the entry.
  Entry^.Key := Nil;
  Entry^.Value := BoolVal(True);

  Result := True;
end;

procedure TTable.AddAll(TableFrom: TTable);
var
  i: Integer=0;
  Entry: PEntry;
begin
  while i <= TableFrom.Capacity do
    begin
      Entry := @TableFrom.Entries[i];
      if Entry^.Key <> Nil then
        SetValue(Entry^.Key, Entry^.Value);
      Inc(i);
    end;
end;

function TTable.FindString(Chars: PChar; Length: Integer; Hash: LongWord
  ): Pointer;
var
  Index: LongWord;
  Entry: PEntry;
  Key: PObjString;
begin
  if Count = 0 then Exit(Nil);

  Index := Hash and Capacity;

  while True do
    begin
      Entry := @Entries[Index];
      Key := PObjString(Entry^.Key);
      if Key = Nil then
        begin
          // Stop if we find an empty non-tombstone entry.
          if Entry^.Value.isNil then Exit(Nil);
        end
      else if (Key^.Length = Length) and (Key^.Hash = Hash) and
              (StrLComp(Key^.Chars, Chars, Length) = 0) then
        Exit(Key);

      Index := (Index + 1) and Capacity;
    end;
end;

function TTable.FindEntry(Entries: PEntry; const Capacity: LongInt; Key: Pointer
  ): PEntry;
var
  Index: LongWord;
  Entry, Tombstone: PEntry;
begin
  Index := PObjString(Key)^.Hash and Capacity;
  Tombstone := Nil;
  while True do
    begin
      Entry := @Entries[Index];

      if Entry^.Key = Nil then
        begin
          if Entry^.Value.isNil then
            Exit(IfThen<PEntry>(Tombstone<>Nil, Tombstone, Entry))
          else
            if Tombstone = Nil then Tombstone := Entry;
        end
      else if Entry^.Key = PObjString(Key) then
        Exit(Entry);
      Index := (Index+1) and Capacity;
    end;
end;

procedure TTable.AdjustCapacity(const NewCapacity: LongInt);
var
  NewEntries, Entry, Dest: PEntry;
  i: Integer=0;
begin
  NewEntries := PEntry(Reallocate(NiL, 0, SizeOf(TEntry) * (NewCapacity+1)));
  while i <= NewCapacity do
    begin
      NewEntries[i].Key := Nil;
      NewEntries[i].Value := NilVal;
      Inc(i);
    end;

  Count := 0;
  for i := 0 to Capacity do
    begin
      Entry := @Entries[i];
      if Entry^.Key = Nil then Continue;
      Dest := FindEntry(NewEntries, NewCapacity, Entry^.Key);
      Dest^.Key := Entry^.Key;
      Dest^.Value := Entry^.Value;
      Inc(Count);
    end;

  Reallocate(Entries, SizeOf(TEntry) * (NewCapacity+1), 0);
  Entries := NewEntries;
  Capacity := NewCapacity;
end;

end.


