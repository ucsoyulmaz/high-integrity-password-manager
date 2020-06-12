with PIN;
with PasswordDatabase;
with Ada.Containers; use Ada.Containers;

generic

package PasswordManager with
   SPARK_Mode
is

   PINValueSaved : PIN.PIN;   -- A variable to store saved PIN state
   DB            : PasswordDatabase.Database; -- to store database state

   -- This procedure is responsible for initiating the pin
   -- The pin value is defined inside that function as a variable.
   procedure Init_Program(InitialPin: in String) with
   Pre => (InitialPin' Length = 4 and
             (for all I in InitialPin'Range => (Character'Pos(InitialPin(I)) >= Character'Pos('0')
              and Character'Pos('9') >= Character'Pos(InitialPin(I)))));

   -- This function checks whether the given PIN is the same PIN
   -- that we store in PINValueSaved variable.
   function Is_Pin_Correct (PINValueGiven : in String) return Boolean with
   Pre => (PINValueGiven' Length = 4 and
             (for all I in PINValueGiven'Range => (Character'Pos(PINValueGiven(I)) >= Character'Pos('0')
              and Character'Pos('9') >= Character'Pos(PINValueGiven(I)))));

   -- This function get a new pin as the input and updates the value of
   -- PINValueSaved with the value of the given PIN.
   procedure Update_Pin (PINValueGiven : in String) with
   Pre => (PINValueGiven' Length = 4 and
             (for all I in PINValueGiven'Range => (Character'Pos(PINValueGiven(I)) >= Character'Pos('0')
              and Character'Pos('9') >= Character'Pos(PINValueGiven(I)))));

   -- This function is responsible for adding a new password-url tupple
   -- to the DB.
   procedure Put_Password (U1 : in String; P1 : in String) with
     Pre => (((if U1'Length > 0 then U1'First <= U1'Last and U1'Length <= 1024) and
               (if P1'Length > 0 then P1'First <= P1'Last and P1'Length <= 100)) and
               (PasswordDatabase.Length(DB) < PasswordDatabase.Max_Entries or

                    (
                         (
                     if(if U1'Length > 0 then U1'First <= U1'Last and U1'Length <= 1024) then
                       PasswordDatabase.Has_Password_For(DB,PasswordDatabase.From_String(U1)
                         )

                    )

                    )
               )
            );

   -- This function is responsible for returning the password for a particular
   -- URL by using the saved password-url tupples in DB.
   function Get_Password (U1 : in String) return String with
   Pre => (if U1'Length > 0 then U1'First <= U1'Last and U1'Length <= 1024) ;

   -- This function is responsible for removing a password-url tupple from DB.
   procedure Remove_Password (U1 : in String) with
     Pre => (if U1'Length > 0 then U1'First <= U1'Last and U1'Length <= 1024) ;

   function Get_DB_Size return Ada.Containers.Count_Type;

   function Get_DB_Length return Ada.Containers.Count_Type;

   function Is_Website_Registered_Already(U1 : in String) return Boolean with
     Pre => (if U1'Length > 0 then U1'First <= U1'Last and U1'Length <= 1024) ;

end PasswordManager;
