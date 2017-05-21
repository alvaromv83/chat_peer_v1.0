with Ada.Text_IO;
With Ada.Strings.Unbounded;
with Maps_G;

procedure Maps_Test is
   package ASU  renames Ada.Strings.Unbounded;
   package ATIO renames Ada.Text_IO;


   package Maps is new Maps_G (Key_Type   => ASU.Unbounded_String,
                               Value_Type => ASU.Unbounded_String,
                               "="        => ASU."=",
                               Key_To_String  => ASU.To_String,
                               Value_To_String  => ASU.To_String);


   Value   : ASU.Unbounded_String;
   Success : Boolean;

   A_Map : Maps.Map;
begin


   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map (A_Map);

   Maps.Put (A_Map,
             ASU.To_Unbounded_String ("facebook.com"),
             ASU.To_Unbounded_String ("69.63.189.16"));

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map(A_Map);


   ATIO.New_Line;
   Maps.Get (A_Map, ASU.To_Unbounded_String ("www.urjc.es"), Value, Success);
   if Success then
      ATIO.Put_Line ("Get: Dirección IP de www.urjc.es: " &
                       ASU.To_String(Value));
   else
      ATIO.Put_Line ("Get: NO hay una entrada para la clave www.urjc.es");
   end if;

   Maps.Put (A_Map, ASU.To_Unbounded_String ("google.com"),
              ASU.To_Unbounded_String ("66.249.92.104"));

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map(A_Map);

   Maps.Put (A_Map,
              ASU.To_Unbounded_String ("www.urjc.es"),
              ASU.To_Unbounded_String ("212.128.240.25"));

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map(A_Map);

   Maps.Put (A_Map,
              ASU.To_Unbounded_String ("facebook.com"),
              ASU.To_Unbounded_String ("69.63.189.11"));

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map(A_Map);

   ATIO.New_Line;
   Maps.Get (A_Map, ASU.To_Unbounded_String ("www.urjc.es"), Value, Success);
   if Success then
      ATIO.Put_Line ("Get: Dirección IP de www.urjc.es: " &
                       ASU.To_String(Value));
   else
      ATIO.Put_Line ("Get: NO hay una entrada para la clave www.urjc.es");
   end if;

   ATIO.New_Line;
   Maps.Delete (A_Map, ASU.To_Unbounded_String("google.com"), Success);
   if Success then
      ATIO.Put_Line ("Delete: BORRADO google.com");
   else
      ATIO.Put_Line ("Delete: google.com no encontrado");
   end if;

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map(A_Map);

   ATIO.New_Line;
   Maps.Delete (A_Map, ASU.To_Unbounded_String("www.urjc.es"), Success);
   if Success then
      ATIO.Put_Line ("Delete: BORRADO www.urjc.es");
   else
      ATIO.Put_Line ("Delete: www.urjc.es no encontrado");
   end if;

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map (A_Map);

   ATIO.New_Line;
   Maps.Delete (A_Map, ASU.To_Unbounded_String("bbb.bbb.bbb"), Success);
   if Success then
      ATIO.Put_Line ("Delete: BORRADO bbb.bbb.bbb");
   else
      ATIO.Put_Line ("Delete: bbb.bbb.bbb no encontrado");
   end if;

   ATIO.New_Line;
   ATIO.Put_Line ("Longitud de la tabla de símbolos: " &
                    Integer'Image(Maps.Map_Length(A_Map)));
   Maps.Print_Map (A_Map);



end Maps_Test;
