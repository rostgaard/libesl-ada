with Ada.Containers.Vectors;

package ESL.Packet_Listener is

   type Instance is tagged null record;

   package Filter is new Ada.Containers.Vectors (Index_Type   => Natural,
                                                 Element_Type => Natural);

   procedure Add_Filter (Obj  : in out Instance;
                         Item : in     Filter.Vector);

   procedure Clear_Filters (Obj  : in out Instance);

   procedure Delete_Filter (Obj  : in out Instance;
                            Item : in     Filter.Vector);

end ESL.Packet_Listener;
