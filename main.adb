-- Umut Cem Soyulmaz - 989654
-- Zhaolin Deng - 984027
-- Savan Kanabar - 965371
-----------------------------------------------------------------------------------------------------------------
-- The Unlock operation can only ever be performed when the password manager is in the Locked state.
-----------------------------------------------------------------------------------------------------------------
-- The boolean var IsUnlocked is used for checking the lock state of the password manager. \\
-- When the program is executed, the state of Lock will be checked. \\
-- If the state is locked>, and the first command line input is Unlock, \\
   -- it will be checked as the first token in an if statement, otherwise the command line will be discarded. \\
-- This way we make sure that when the password manager is Locked, 
   -- we cannot perform any other operation on it apart from Unlock. \\
-----------------------------------------------------------------------------------------------------------------
-- The Get, Put, Remove and Lock operations can only ever be performed when the password manager is in 
   --the unlocked state.
-----------------------------------------------------------------------------------------------------------------
-- Once the state of password manager is verified as Unlocked by an if statement at line 168: \\

-- In line 199, the command line of Lock operation will be checked as the first token by an if statement, 
   -- and that statement will also check the token number to ensure that there are 2 tokens 
   -- and also check the second token to make sure its length is 4.\\

-- In line 231, the command line of Put operation will be checked as the first token by an if statement, 
   -- and that statement will also check the token number to ensure they are 3.\\

-- In line 269, the command line of Get operation will be checked as the first token by an if statement, 
   -- and that statement will also check the token number to ensure they are 2.\\

-- In line 285, the command line of Remove operation will be checked as the first token by an if statement, 
   -- and that statement will also check the token number to ensure they are 2.\\
-----------------------------------------------------------------------------------------------------------------
-- The Lock operation, when it is performed, should update the master PIN with the new PIN that is supplied.
-----------------------------------------------------------------------------------------------------------------
-- Once the lock operation is successfully performed and  the first token recognized as ‘Lock’, 
   -- the second string after a whitespace will be recognized as the second token by procedure Tokenise 
   -- in MyStringTokeniser and the command line will check whether there are only 2 tokens and 
   -- if the second token’s length is 4 by using an if statement. After that, the PINValueSaved which is 
   -- the master PIN will be assigned as the second token value which is the new PIN by procedure 
   -- Update_Pin in passwordmanager. \\
-----------------------------------------------------------------------------------------------------------------
-- Additional Properties
-----------------------------------------------------------------------------------------------------------------
-- After lock operation is performed, state should change to locked> \\
-- After unlock operation is performed, state should change to unlocked> \\
-- unlock will only be performed when PIN matches master PIN \\
-- get operation will only be performed when the (password and) URL exist \\
-- remove operation will only be perform when the URL exist \\
-- After put operation is performed, password should be put in to the passworddatabase \\
-- After get operation is performed, password should be shown in next line \\
-- After remove operation is performed, password should be removed from the passworddatabase \\
-- The pin should be exactly 4 digits long. No more, no less. \\
-- The pin can only include digits from 0-9 \\ 
-- Put will only be performed if there is space in the passworddatabase. If the entry already exist in DB
   -- Put for that entry will update the password even if DB is full \\
-- Any other input apart from get, put, rem and lock for the first token is invalid when the password manager 
   -- is in Unlock state. \\
-- Only Unlock is valid when password manager is Locked \\
-- URL length must be < 1025 chars \\
-- Password length must be < 101 chars \\
-- The state of password manager is checked at each iteration of loop \\

-- Currently, there is a warning -> medium prove issue, here Spark cannot prove that there is still space in 
   -- passworddatabase for a new entry. We tried several fixes but the issue is that we are unable to access 
   -- Ada.Containers.Count_Type so Max_Entries cannot be used to compare with DB'Length directly and thus
   -- precondition of function From_String cannot hold.
   
-- The loop invariants in our code will work as additional safegurads to ensure that the properties mentioned
   -- above mentioned properties are held and that there is no undesired flow which can violate system behaviour.
pragma SPARK_Mode (On);

with PasswordDatabase;
with MyCommandLine;
with MyString;
with MyStringTokeniser;     use MyStringTokeniser;
with PIN;
with PasswordManager;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;

procedure Main is

   package Lines is new MyString (Max_MyString_Length => 2048);
   IsUnlocked    : Boolean;
   IsPinAccepted : Boolean;
   package PM is new PasswordManager;
   S : Lines.MyString;
   T : MyStringTokeniser.TokenArray (1 .. 3) :=
     (others => (Start => 1, Length => 0));
   NumTokens    : Natural;
   InputPinText : String         := "1234";
   UrlText      : Lines.MyString := Lines.From_String ("www");
   IsItemInDB   : Boolean;

begin

   -- If the program is initiated from the terminal with a initial password
   -- Example: "./main 5555"

   declare
      IsPinValid    : Boolean := True;
      IsLoopRunning : Boolean := True;

   begin

      if (MyCommandLine.Argument_Count = 1) then
         if (MyCommandLine.Argument (1)'Length = 4) then
            InputPinText := MyCommandLine.Argument (1);

            for I in InputPinText'Range loop
               declare
                  Ch : Character := InputPinText (I);
               begin
                  if (Character'Pos (Ch) < Character'Pos ('0')) then
                     IsPinValid := False;
                  elsif (Character'Pos ('9') < Character'Pos (Ch)) then
                     IsPinValid := False;
                  end if;
               end;
            end loop;
         else
            IsPinValid := False;
         end if;
      else
         IsPinValid := False;
      end if;

      if (IsPinValid) then
         -- Initiate the pin
         PM.Init_Program (InputPinText);

         -- Set the program state as locked.
         IsUnlocked := False;

         declare
            -- These variables are used to represent the tokes inside the given
            -- input string.
            TokenStr1 : Lines.MyString;
            TokenStr2 : Lines.MyString;
            TokenStr3 : Lines.MyString;

         begin
            -- While loop for executing the infinite terminal actions
            While_Loop :
            while (IsLoopRunning) loop

               -- For PIN
               pragma Loop_Invariant
                 (for all I in InputPinText'Range =>
                    (Character'Pos (InputPinText (I)) >=
                     Character'Pos ('0') and
                     Character'Pos ('9') >= Character'Pos (InputPinText (I))));

               -- For Put
               pragma Loop_Invariant
                 ((Lines.To_String (UrlText)'First <=
                   Lines.To_String (UrlText)'Last) and
                  (Lines.To_String (UrlText)'Length > 0));

               pragma Loop_Invariant
                 ((PM.Get_DB_Length < PM.Get_DB_Size) or
                  (PM.Get_DB_Length >= PM.Get_DB_Size));

         -- *******************************************************************
         -- For the situation when the password manager is UNLOCKED.

               if (IsUnlocked) then
                  Put ("unlocked> ");
                  Lines.Get_Line (S);

                  MyStringTokeniser.Tokenise
                    (Lines.To_String (S), T, NumTokens);
                  if
                    (NumTokens >= 2 and
                     (((T (1).Length - 1 <= Lines.Length (S) - T (1).Start) and
                       (T (1).Length > 1)) and
                      ((T (2).Length - 1 <=
                        Lines.Length (S) - T (2).Start)) and
                      (T (2).Length > 1)))

                  then
                  -------------------------------------------------------------
                     -- Get the first token and put it inside TokStr1 variable
                     TokenStr1 :=
                       (Lines.Substring
                          (S, T (1).Start, T (1).Start + T (1).Length - 1));

                     -- Get the second token and put it inside TokStr2 variable
                     TokenStr2 :=
                       (Lines.Substring
                          (S, T (2).Start, T (2).Start + T (2).Length - 1));
                  -------------------------------------------------------------

            -- If that first token is "lock" and there is only one more token
            -- in the given input string, do the following:

                     if
                       (Lines.To_String (TokenStr1) = "lock" and
                        NumTokens = 2 and
                        Lines.To_String (TokenStr2)'Length = 4)
                     then
                        InputPinText := Lines.To_String (TokenStr2);

                        if
                          ((for all I in InputPinText'Range =>
                              (Character'Pos (InputPinText (I)) >=
                               Character'Pos ('0') and
                               Character'Pos ('9') >=
                                 Character'Pos (InputPinText (I)))) /=
                           True)
                        then
                           IsPinValid := False;
                        end if;

                        if (IsPinValid) then

                  -- Tell everybody that the password
                  -- manager is locked. Thus, in the next iteration of the
                  -- while loop, it will start as an locked Password Manager.

                           PM.Update_Pin (InputPinText);
                           IsUnlocked := False;
                        else
                           IsLoopRunning := False;
                        end if;

                  -- If that first token is "put" and there are two more tokens
                  -- in the given input string, do the following:
                     elsif
                       (Lines.To_String (TokenStr1) = "put" and NumTokens = 3)
                     then

                  -------------------------------------------------------------
                     -- Get the second token and put it inside TokStr3 variable
                        TokenStr3 :=
                          Lines.Substring
                            (S, T (3).Start, T (3).Start + T (3).Length - 1);
                  -------------------------------------------------------------
                        UrlText := TokenStr2;

                        if (Lines.To_String (TokenStr2)'Length <= 1024) then
                           IsItemInDB :=
                             PM.Is_Website_Registered_Already
                               (Lines.To_String (TokenStr2));

                           -- Execute the put operation
                           if
                             ((Lines.To_String (TokenStr2)'Length <= 1024 and
                               Lines.To_String (TokenStr3)'Length <= 100) and

                              ((PM.Get_DB_Length <= PM.Get_DB_Size) or

                               (IsItemInDB)))
                           then
                              PM.Put_Password
                                (Lines.To_String (UrlText),
                                 Lines.To_String (TokenStr3));

                           else
                              IsLoopRunning := False;
                           end if;
                        else
                           IsLoopRunning := False;
                        end if;
               -- If that first token is "get" and there is only one more token
               -- in the given input string, do the following:
                     elsif
                       (Lines.To_String (TokenStr1) = "get" and NumTokens = 2)
                     then
                        if (Lines.To_String (TokenStr2)'Length <= 1024) then
                           -- Print the retrieved password result for the given URL.
                           if( PM.Is_Website_Registered_Already (Lines.To_String (TokenStr2))) then
                              Put_Line
                                (PM.Get_Password (Lines.To_String (TokenStr2)));
                           end if;

                        else
                           IsLoopRunning := False;
                        end if;

               -- If that first token is "rem" and there is only one more token
               -- in the given input string, do the following:
                     elsif
                       (Lines.To_String (TokenStr1) = "rem" and NumTokens = 2)
                     then

                        if (Lines.To_String (TokenStr2)'Length <= 1024) then
                           -- Execute the remove operation
                           PM.Remove_Password (Lines.To_String (TokenStr2));
                        else
                           IsLoopRunning := False;
                        end if;
                     else
                        IsLoopRunning := False;
                     end if;

                  else
                     IsLoopRunning := False;

                  end if;
         -- *******************************************************************

         -- *******************************************************************
         -- For the situation when the password manager is LOCKED.
               else
                  Put ("locked> ");
                  Lines.Get_Line (S);
                  MyStringTokeniser.Tokenise
                    (Lines.To_String (S), T, NumTokens);

                  if
                    (NumTokens >= 2 and
                     (((T (1).Length - 1 <= Lines.Length (S) - T (1).Start) and
                       (T (1).Length > 1)) and
                      ((T (2).Length - 1 <=
                        Lines.Length (S) - T (2).Start)) and
                      (T (2).Length > 1)))
                  then
                  -------------------------------------------------------------
                     -- Get the first token and put it inside TokStr1 variable
                     TokenStr1 :=
                       (Lines.Substring
                          (S, T (1).Start, T (1).Start + T (1).Length - 1));
                  -------------------------------------------------------------

                     if
                       (Lines.To_String (TokenStr1) = "unlock" and
                        NumTokens = 2 and T (2).Length = 4)
                     then

                  -------------------------------------------------------------
                     -- Get the second token and put it inside TokStr2 variable
                        TokenStr2 :=
                          (Lines.Substring
                             (S, T (2).Start, T (2).Start + T (2).Length - 1));
                  -------------------------------------------------------------

                        -- Use that second token as the PIN
                        InputPinText := Lines.To_String (TokenStr2);

                        if
                          ((for all I in InputPinText'Range =>
                              (Character'Pos (InputPinText (I)) >=
                               Character'Pos ('0') and
                               Character'Pos ('9') >=
                                 Character'Pos (InputPinText (I)))) /=
                           True)
                        then
                           IsPinValid := False;
                        end if;

                        if (IsPinValid) then
                  -- Tell everybody that the password
                  -- manager is locked. Thus, in the next iteration of the
                  -- while loop, it will start as an locked Password Manager.

                           -- Check whether the PIN is correct
                           IsPinAccepted := PM.Is_Pin_Correct (InputPinText);

                           if (IsPinAccepted) then
                              IsUnlocked := True;
                           end if;
                        else
                           IsLoopRunning := False;
                        end if;

                     else
                        IsLoopRunning := False;
                     end if;

                  else
                     IsLoopRunning := False;

                  end if;

               end if;
         -- *******************************************************************
            end loop While_Loop;

         end;
      end if;
      Put_Line ("Invalid Input, BYE!");
   end;

end Main;
