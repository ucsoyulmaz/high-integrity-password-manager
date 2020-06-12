
package body MyStringTokeniser with SPARK_Mode is

   -- ***************************** ANSWER FOR TASK 1.2 ************************************

   -- The statement "Loop_Invariant (OutIndex = Tokens'First + Count);" is written along with
   -- "pragma" keyword, thus, it indicates that this is an assertion statement. If the assertion
   -- can not be hold, the postcondition statement "Count <= Tokens'Length" can not be proven
   -- with all the possible cases. To explain the behaviour of that pragma, let's examine each
   -- element one by one:

   -- "OutIndex" represents the index of token array which must not be larger than the size of
   -- the array (refer that statement in the while loop: "OutIndex <= Tokens'Last").

   -- "Tokens'First" represents the index of the first element in tokens array. This index
   -- value is determined in main.adb before calling the Tokenise procedure. It might be
   -- chosen as 1 or 5 or 200 but the following indexes should be in an incremental order.
   -- (Each token array must hold that equation: " Tokens'First" + Tokens'Length = Tokens'Last ")

   -- The "Count" parameter is responsible for representing the how many times that the loop
   -- is executed.



   -- Summary:

   -- Since the index of token array does not neccassarily start from index 0 (it can be
   -- any number); to represent which index value of the token array is currently being
   -- traversed, we need to calculate the summation value of "how many times that the loop
   -- is executed" with "the index of first element in the token array".

   -- This summation is simply represented with "Tokens'First + Count"

   -- By using this assertion, we can test the state of the loop at the beginning of each
   -- iteration about whether it satisfies the STATEMENT 1 in the Post Condition or not.


   -- **************************************************************************************

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         pragma Loop_Invariant (OutIndex = Tokens'First + Count);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
