with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   -- ***************************** ANSWER FOR TASK 1.1 ************************************

   -- STATEMENT 1: "Count <= Tokens'Length"
   --    Since the Count parameter is responsible for representing the number of tokens as an
   --    array index; in a fully safe environment this value can not be larger than the size
   --    of the array which contains all the tokens which in the given input. The size of the
   --    token array is initialized in main.adb before calling Tokenise procedure, thus, the
   --    size must be chosen large enough to store all the tokens in the given input string.
   --    Thus, this post condition statement is responsible for preventing system from being
   --    affected by such particular unexpected overflow cases.




   -- As the "Index" variable is responsible for changing index of Tokens array in the For
   -- Loop, each Token(Index) value contains the each token retrieved from the input.

      -- STATEMENT 2: "Tokens(Index).Start >= S'First"
      --
      --    For each different index value,Token(Index).Start represents the first
      --    character of each token in the array. If S'First (the first character of
      --    the input) is considered as the lower bound, the characters of each token must
      --    start from an index position which is larger than this lower bound. Therefore it
      --    guarantees that all the tokens will be above that lower bound of given input
      --    string, thus, we can make sure that there will not be any token which contains
      --    some characters coming before the first character of the input string.

      -- STATEMENT 3: "Tokens(Index).Length > 0"
      --
      --    For each different index value,Token(Index).Length represents the length of
      --    each token in the array. The statement as a whole says that the length of each
      --    token must be larger than 0. In other words, it guarantees that there will not
      --    be a single token which does not contain any characters inside itself. Thus,
      --    every token must containt at least one character to be classified as a token.

      -- STATEMENT 4: "Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start"
      --
      --    For each different index value, Token(Index).Length represents the
      --    length of each token in the array. Since S'Last represents the index
      --    of last character in the given input string, the statement says that
      --    the length of a token can not exceed the difference between S'Last index
      --    and the index of its first character. This statement guarantees all the
      --    tokens will be under the upper bound of given input string, thus, we
      --    can make sure that there will not be any token which contains some characters
      --    coming after the last character of the input string.

   -- If STATEMENT 2 & 3 are satisfied, STATEMENT 4, which is inside the same for loop,
   -- is evaluated as well. Then, the program can guarantee that all of the tokens will
   -- be inside the boundaries of the given input string (Boundaries: S'First <-> S'Last).
   -- Thus, there won't be any unexpected overflow cases which will be generated after
   -- executing this procedure.

   -- **************************************************************************************

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);


end MyStringTokeniser;
