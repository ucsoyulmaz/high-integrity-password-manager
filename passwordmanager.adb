package body PasswordManager with
   SPARK_Mode
is

   procedure Init_Program(InitialPin : in String) is

   begin
      --Put(Character'Pos(InitialPin(InitialPin'First)));
      PINValueSaved := PIN.From_String (InitialPin);
      PasswordDatabase.Init(DB);

   end Init_Program;

   function Is_Pin_Correct (PINValueGiven : in String) return Boolean is
   begin
      declare
         PINValueToCheck : PIN.PIN := PIN.From_String(PINValueGiven);
      begin
      if PIN."=" (PINValueSaved, PINValueToCheck) then
         return True;
      else
         return False;
         end if;
      end;
   end Is_Pin_Correct;

   procedure Update_Pin (PINValueGiven : in String) is
   begin
      declare
         PINValueToUpdate  : PIN.PIN := PIN.From_String(PINValueGiven);
      begin

         PINValueSaved := PINValueToUpdate;
      end;

   end Update_Pin;

   procedure Put_Password (U1 : in String; P1 : String) is
   begin

      PasswordDatabase.Put
        (DB, PasswordDatabase.From_String (U1),
         PasswordDatabase.From_String (P1));

   end Put_Password;

   function Get_Password (U1 : in String) return String is
   begin

      -- Check whether DB contains a tupple with that URL
      if
        (PasswordDatabase.Has_Password_For
           (DB, PasswordDatabase.From_String (U1)))
      then
         return PasswordDatabase.To_String
             (PasswordDatabase.Get (DB, PasswordDatabase.From_String (U1)));
      end if;

      return "";
   end Get_Password;

   procedure Remove_Password (U1 : in String) is
   begin

      -- Check whether DB contains a tupple with that URL
      if
        (PasswordDatabase.Has_Password_For
           (DB, PasswordDatabase.From_String (U1)))
      then
         PasswordDatabase.Remove (DB, PasswordDatabase.From_String (U1));


      end if;

   end Remove_Password;

   function Get_DB_Size return Ada.Containers.Count_Type is
   begin
      return PasswordDatabase.Max_Entries;

   end Get_DB_Size;

   function Get_DB_Length return Ada.Containers.Count_Type is
   begin
      return PasswordDatabase.Length(DB);

   end Get_DB_Length;

   function Is_Website_Registered_Already(U1 : in String) return Boolean is
   begin
      declare
         IsItemInDB: Boolean;
         begin
         IsItemInDB := PasswordDatabase.Has_Password_For
           (DB, PasswordDatabase.From_String (U1));

         return IsItemInDB;
           end;
   end Is_Website_Registered_Already;

end PasswordManager;
