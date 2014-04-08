with Ada.Characters.Handling,
     Ada.Strings.Hash;

function ESL.Unbounded_Case_Insensitive_Hash
  (Item : in Ada.Strings.Unbounded.Unbounded_String)
  return Ada.Containers.Hash_Type is
   use Ada.Characters.Handling, Ada.Strings.Unbounded;
begin
   return Ada.Strings.Hash (To_Lower (To_String (Item)));
end ESL.Unbounded_Case_Insensitive_Hash;
