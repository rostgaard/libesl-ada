with Ada.Characters.Handling;

function ESL.Case_Insensitive_Equal (Left, Right : String) return Boolean is
   use Ada.Characters.Handling;
begin
   return To_Lower (Left) = To_Lower (Right);
end ESL.Case_Insensitive_Equal;
