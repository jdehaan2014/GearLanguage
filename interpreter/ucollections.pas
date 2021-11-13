unit uCollections;

{ This unit defines classes TArray, TDictionary and TStack based on the
  Free Pascal FGL library.

  Copyright (C) 2018 J. de Haan jdehaan2014@gmail.com

  This source is free software; you can redistribute it and/or modify it under the terms
  of the GNU General Public License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Variants;

type

  generic TArray<T> = class(specialize TFPGList<T>)
    function Contains(const AValue: T) : Boolean;
    function Contains(const AValue: T; var Index: LongInt) : Boolean;
    function Copy: TArray;
    procedure Concat(withArray: TArray);
  end;

  generic TArrayObj<T: class> = class(specialize TFPGObjectList<T>)
    function Contains(const AValue: T) : Boolean;
    function Contains(const AValue: T; var Index: LongInt) : Boolean;
    procedure Concat(withArray: TArrayObj);
  end;

  generic TDictionary<TKey, TData> = class(specialize TFPGMap<TKey, TData>)
    procedure SetValue(const AKey: TKey; AData: TData);
    function TryGetValue(const AKey: TKey; var AValue: TData): Boolean;
    function At(const Index: LongInt): TData;
    function Contains(const AKey: TKey): Boolean;
    function Contains(const AValue: TKey; var Index: LongInt) : Boolean;
    function Copy: TDictionary;
    procedure Concat(withDict: TDictionary);
  end;

  generic TDictionaryObj<TKey; TData: class> =
      class(specialize TFPGMapObject<TKey, TData>)
    procedure SetValue(const AKey: TKey; AData: TData);
    function TryGetValue(const AKey: TKey; var AValue: TData): Boolean;
    function At(const Index: LongInt): TData;
    function Contains(const AKey: TKey): Boolean;
    function Contains(const AValue: TKey; var Index: LongInt) : Boolean;
  end;

  generic TStack<T: TObject> = class
    private
      type TItems = specialize TArrayObj<T>;
    private
      FItems: TItems;
      function getCount: Integer;
      function getItem(i: Integer): T;
    public
      property Count: Integer read getCount;
      property Items[i: Integer]: T read getItem; default;
      constructor Create;
      destructor Destroy; override;
      procedure Push(Item: T);
      procedure Pop;
      function Top: T;
      function isEmpty: Boolean;
  end;

implementation

{ TArray }

function TArray.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TArray.Contains(const AValue: T; var Index: LongInt): Boolean;
begin
  Index := IndexOf(AValue);
  Result := Index >= 0;
end;

function TArray.Copy: TArray;
begin
  Result := TArray.Create;
  Result.Assign(Self);
end;

procedure TArray.Concat(withArray: TArray);
var
  i: Integer;
begin
  for i := 0 to withArray.Count-1 do
    Self.Add(withArray[i]);
end;

{ TArrayObj }

function TArrayObj.Contains(const AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TArrayObj.Contains(const AValue: T; var Index: LongInt): Boolean;
begin
  Index := IndexOf(AValue);
  Result := Index >= 0;
end;

procedure TArrayObj.Concat(withArray: TArrayObj);
var
  i: Integer;
begin
  for i := 0 to withArray.Count-1 do
    Self.Add(withArray[i]);
end;

{ TDictionary }

procedure TDictionary.SetValue(const AKey: TKey; AData: TData);
begin
  if Contains(AKey) then
    KeyData[AKey] := AData
  else
    Add(AKey, AData);
end;

function TDictionary.TryGetValue(const AKey: TKey; var AValue: TData): Boolean;
var
  Index: LongInt = -1;
begin
  Result := Contains(AKey, Index);
  if Result then
    AValue := Data[Index]
  else
    AValue := Default(TData);
end;

function TDictionary.At(const Index: LongInt): TData;
begin
  Result := Data[Index];
end;

function TDictionary.Contains(const AKey: TKey): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TDictionary.Contains(const AValue: TKey; var Index: LongInt): Boolean;
begin
  Index := IndexOf(AValue);
  Result := Index >= 0;
end;

function TDictionary.Copy: TDictionary;
var
  i: Integer;
begin
  Result := TDictionary.Create;
  for i := 0 to Self.Count-1 do
    Result.Add(Self.Keys[i], Self.Data[i]);
end;

procedure TDictionary.Concat(withDict: TDictionary);
var
  i: Integer;
begin
  for i := 0 to withDict.Count-1 do
    Self.Add(withDict.Keys[i], withDict.Data[i]);
end;

{ TDictionaryObj }

procedure TDictionaryObj.SetValue(const AKey: TKey; AData: TData);
begin
  if Contains(AKey) then
    KeyData[AKey] := AData
  else
    Add(AKey, AData);
end;

function TDictionaryObj.TryGetValue(const AKey: TKey; var AValue: TData
  ): Boolean;
var
  Index: LongInt = -1;
begin
  Result := False;
  AValue := Nil;
  if Contains(AKey, Index) then begin
    AValue := At(Index);
    Result := True;
  end;
end;

function TDictionaryObj.At(const Index: LongInt): TData;
begin
  Result := Data[Index];
end;

function TDictionaryObj.Contains(const AKey: TKey): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TDictionaryObj.Contains(const AValue: TKey; var Index: LongInt
  ): Boolean;
begin
  Index := IndexOf(AValue);
  Result := Index >= 0;
end;

{ TStack }

function TStack.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TStack.getItem(i: Integer): T;
begin
  if (i >= 0) and (i < FItems.Count) then
    Result := FItems[i]
  else
    Result := Nil;
end;

constructor TStack.Create;
begin
  FItems := TItems.Create();
end;

destructor TStack.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TStack.Push(Item: T);
begin
  FItems.Add(Item);
end;

procedure TStack.Pop;
begin
  if FItems.Last <> Nil then
    FItems.Remove(FItems.Last);
end;

function TStack.Top: T;
begin
  Result := FItems.Last;
end;

function TStack.isEmpty: Boolean;
begin
  Result := FItems.Count = 0;
end;

end.


