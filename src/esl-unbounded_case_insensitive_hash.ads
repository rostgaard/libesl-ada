with Ada.Containers,
     Ada.Strings.Unbounded;

function ESL.Unbounded_Case_Insensitive_Hash
  (Item : in Ada.Strings.Unbounded.Unbounded_String)
  return Ada.Containers.Hash_Type;
