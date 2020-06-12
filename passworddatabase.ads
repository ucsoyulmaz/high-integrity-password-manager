with Ada.Containers.Formal_Ordered_Maps;
with Ada.Containers;
use Ada.Containers;

with MyString;

package PasswordDatabase with SPARK_Mode is
   Max_Password_Length : constant Natural := 100;
   Max_URL_Length : constant Natural := 1024;
   Max_Entries : constant Ada.Containers.Count_Type := 1000;

   package Passwords is new MyString(Max_MyString_Length => Max_Password_Length);
   package URLs is new MyString(Max_MyString_Length => Max_URL_Length);

   type Password is new Passwords.MyString;
   type URL is new URLs.MyString;

   type Database is private;

   procedure Init(D : out Database);

   function Has_Password_For(D : in Database; U : in URL) return Boolean;

   function Get(D : in Database; U : in URL) return Password with
     Pre => Has_Password_For(D,U);

   procedure Put(D : in out Database; U : in URL; P : in Password) with
     Pre => Length(D) < Max_Entries or Has_Password_For(D,U);

   procedure Remove(D : in out Database; U : in URL) with
     Pre => Has_Password_For(D,U);

   function From_String(S : String) return Password with
     Pre => S'Length <= Max_Password_Length;
   function From_String(S : String) return URL with
     Pre => S'Length <= Max_URL_Length;
   function To_String(P : Password) return String;
   function To_String(U : URL) return String;

   function Less(U1 : URL; U2 : URL) return Boolean;
   function Equal(U1 : URL; U2 : URL) return Boolean;

   function Equal(P1 : Password; P2 : Password) return Boolean;

   function Length(D : in Database) return Ada.Containers.Count_Type;
private
   pragma SPARK_Mode(Off);

   package PW_Ordered_Maps is new
     Ada.Containers.Formal_Ordered_Maps
       (Key_Type        => URL,
        Element_Type    => Password,
        "<" => Less);


   type Database is record
      passwords : PW_Ordered_Maps.Map(Capacity => Max_Entries);
   end record;


   function Get(D : in Database; U : in URL) return Password is
     (PW_Ordered_Maps.Element(Container => D.passwords,Key => U));
   function Has_Password_For(D : in Database; U : in URL) return Boolean is
     (PW_Ordered_Maps.Contains(Container => D.passwords, Key => U));

   function From_String(S : String) return Password is
     (Password(Passwords.From_String(S)));
   function From_String(S : String) return URL is
     (URL(URLs.From_String(S)));
   function To_String(P : Password) return String is
     (Passwords.To_String(Passwords.MyString(P)));
   function To_String(U : URL) return String is
     (URLs.To_String(URLs.MyString(U)));

   function Less(U1 : URL; U2 : URL) return Boolean is
      (URLs.Less(URLs.MyString(U1),URLs.MyString(U2)));
   function Equal(U1 : URL; U2 : URL) return Boolean is
      (URLs.Equal(URLs.MyString(U1),URLs.MyString(U2)));

   function Equal(P1 : Password; P2 : Password) return Boolean is
      (Passwords.Equal(Passwords.MyString(P1),Passwords.MyString(P2)));

   function Length(D : in Database) return Ada.Containers.Count_Type is
      (PW_Ordered_Maps.Length(D.passwords));
end PasswordDatabase;
