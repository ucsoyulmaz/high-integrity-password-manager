package body PasswordDatabase is

   procedure Init(D : out Database) with SPARK_Mode => Off is
   begin
      PW_Ordered_Maps.Clear(D.passwords);
   end Init;

   procedure Put(D : in out Database; U : in URL; P : in Password) is
   begin
      PW_Ordered_Maps.Include(D.passwords,U,P);
   end Put;

   procedure Remove(D : in out Database; U : in URL) is
   begin
      PW_Ordered_Maps.Delete(D.passwords,U);
   end Remove;

end PasswordDatabase;
