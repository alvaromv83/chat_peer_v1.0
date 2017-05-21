-- Autor: Álvaro Moles Vinader.

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Debug;
with Pantalla;

package body Maps_G is

	package DB renames Debug;
	package PNT renames Pantalla;

   procedure Free is new Ada.Unchecked_Deallocation (Cell, Cell_A);

	-- Procedimiento Get
   procedure Get (M       : Map;
                  Key     : in  Key_Type;
                  Value   : out Value_Type;
                  Success : out Boolean) is
      P_Aux : Cell_A;
   begin
      P_Aux := M.P_First;
      Success := False;
      while not Success and P_Aux /= null Loop
         if P_Aux.Key = Key then
            Value := P_Aux.Value;
            Success := True;
         end if;
         P_Aux := P_Aux.Next;
      end loop;
   end Get;

	-- Procedimiento Put
   procedure Put (M     : in out Map;
                  Key   : Key_Type;
                  Value : Value_Type;
                  Success : out Boolean) is
      P_Aux : Cell_A;
      Found : Boolean;
   begin
      -- Si ya existe Key, cambiamos su Value
      P_Aux := M.P_First;
      Found := False;
      Success := False;

      while not Found and P_Aux /= null loop
         if P_Aux.Key = Key then
            P_Aux.Value := Value;
            Found := True;
            Success := True;										
         end if;
         P_Aux := P_Aux.Next;
      end loop;

      -- Si no hemos encontrado Key añadimos al principio
      if not Found then
      	-- Creamos una celda nueva con anterior a null
      	M.P_First := new Cell'(Key, Value, null, M.P_First);									
			-- Si el siguiente no es nulo, hacemos que su anterior apunte a la 
			-- celda nueva
			if M.P_First.Next /= null then
         	M.P_First.Next.Prev := M.P_First;
			end if;
         M.Length := M.Length + 1;
         Success := True;
      end if;
      
   end Put;

	-- Procedimiento Delete
   procedure Delete (M      : in out Map;
                     Key     : in  Key_Type;
                     Success : out Boolean) is
      P_Current  : Cell_A;
      P_Previous : Cell_A;
   begin
      Success := False;
      P_Previous := M.P_First.Prev;
      P_Current  := M.P_First;
      while not Success and P_Current /= null  loop
         if P_Current.Key = Key then
            Success := True;
            M.Length := M.Length - 1;
            if P_Previous /= null then
               P_Previous.Next := P_Current.Next;
            end if;
            if M.P_First = P_Current then
					M.P_First.Prev := M.P_First;
               M.P_First := M.P_First.Next;
            end if;
            Free (P_Current);
         else
            P_Previous := P_Current;
            P_Current := P_Current.Next;
         end if;
      end loop;

   end Delete;

	-- Función Get_Keys
	function Get_Keys (M : Map) return Keys_Array_Type is
		I: Positive := 1;
		Keys_Array: Keys_Array_Type;
		P_Aux : Map;
	begin

		P_Aux := M;
		for I in 1..Max_Length loop
			if P_Aux.P_First /= null then
				Keys_Array(I) := P_Aux.P_First.Key;
				P_Aux.P_First := P_Aux.P_First.Next;
			else
				Keys_Array(I) := Null_Key;
			end if;
		end loop;

		return Keys_Array;
	end Get_Keys;
	
	-- Función Get_Values
	function Get_Values (M : Map) return Values_Array_Type is
		I: Positive := 1;
		Values_Array: Values_Array_Type;
		P_Aux : Map;
	begin

		P_Aux := M;
		for I in 1..Max_Length loop
			if P_Aux.P_First /= null then
				Values_Array(I) := P_Aux.P_First.Value;
				P_Aux.P_First := P_Aux.P_First.Next;
			else
				Values_Array(I) := Null_Value;
			end if;
		end loop;

		return Values_Array;
	end Get_Values;

	-- Función Map_Length
   function Map_Length (M : Map) return Natural is
   begin
      return M.Length;
   end Map_Length;

   procedure Print_Map (M : Map) is
      P_Aux : Cell_A;
   begin
      P_Aux := M.P_First;

      Ada.Text_IO.Put_Line ("Map");
      Ada.Text_IO.Put_Line ("===");

      while P_Aux /= null loop
         Ada.Text_IO.Put_Line (Key_To_String(P_Aux.Key) & " " &
                                 VAlue_To_String(P_Aux.Value));
         P_Aux := P_Aux.Next;
      end loop;
   end Print_Map;

end Maps_G;
