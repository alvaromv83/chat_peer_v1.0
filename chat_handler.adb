-- Cuerpo de Chat_Handler (ver especificación en chat_handler.ads)
-- Autor: Álvaro Moles Vinader.

with Gnat.Calendar.Time_IO;

package body Chat_Handler is

    package C_IO renames Gnat.Calendar.Time_IO;

    -- Función que devuelve un string con la fecha y hora
    function Image_Time (T: Ada.Calendar.Time) return string is
    begin
        return C_IO.Image (T, "%c");
    end Image_Time;
    
    -- Función para incrementar el número de secuencia
    function Seq_N_Increased (N: Seq_N_T) return Seq_N_T is
    begin
        return N + 1;
    end Seq_N_Increased;    
    
    -- Función para trocear EP
    function Cut_EP (EP: LLU.End_Point_Type) return string is
        I       : Positive := 1;
        Index   : Natural := 0;
        EP_Tail : ASU.Unbounded_String := ASU.To_Unbounded_String (LLU.Image(EP));
        IP      : ASU.Unbounded_String;
        Cut_EP  : ASU.Unbounded_String;
    begin
        for I in 1..4 loop
            Index   := ASU.Index (EP_Tail, " ");
            EP_Tail := ASU.Tail (EP_Tail, ASU.Length(EP_Tail)-Index);
            if I = 2 then
                Index := ASU.Index (EP_Tail, " ");
                IP    := ASU.Head(EP_Tail, ASU.Length(EP_Tail)-Index-2);
            elsif I = 4 then
                Index   := ASU.Index (EP_Tail, " ");
                EP_Tail := ASU.Tail (EP_Tail, ASU.Length(EP_Tail)-Index);
                Cut_EP  := IP & ":" & EP_Tail;
            end if;
        end loop;
        return ASU.To_String(Cut_EP);
    end Cut_EP;
    
    -- Procedimiento para comprobar si el EP_H_Creat está en latest_msgs
    procedure Check_EP_H (EP_H_Creat   : in LLU.End_Point_Type;
                          Seq_N        : in Seq_N_T; 
                          Found        : in out Boolean; 
                          Higher_Seq_N : in out Boolean) is
    Finish: Boolean := False;
    I : Positive := 1;
    begin
        Found        := False;
        Higher_Seq_N := False;
        
        while not Finish and I <= Latest_Msgs.Map_Length(Ltst_Msgs_Map) loop
        
            -- Si EP_H_Creat está en latest_messages...
            if EP_H_Creat = LM_Key_Array(I) then            
                Finish := True;
                Found  := True;
                
                -- Si Seq_N es mayor...
                if Seq_N > LM_Val_Array(I) then
                    Higher_Seq_N := True;
                end if;
            end if;
            I := I + 1;
        end loop;

    end Check_EP_H;
    
    -- Procedimiento para reenviar mensajes por inundación
    procedure Flood_Rsnd_Msg (EP_H_Rsnd : in LLU.End_Point_Type; 
                              P_Buffer  : access LLU.Buffer_Type) is
    I : Positive := 1;
    begin

        while I <= Neighbors.Map_Length(Neighbors_Map) and 
                NB_Key_Array(I) /= null loop
        
            if NB_Key_Array(I) /= EP_H_Rsnd then
                LLU.Send (NB_Key_Array(I), P_Buffer);
                DB.Put_Line ("        send to " & Cut_EP(NB_Key_Array(I)));
            end if;
            
            I := I + 1;        
        end loop;
        
        LLU.Reset (P_Buffer.All);

    end Flood_Rsnd_Msg;
    
    -- Procedimiento para enviar y recibir en el Handler
    procedure Handler (From     : in LLU.End_Point_Type; 
                       To       : in LLU.End_Point_Type; 
                       P_Buffer : access LLU.Buffer_Type) is
                             
        Msg_Mode     : CM.Message_Type;
        EP_H_Creat   : LLU.End_Point_Type;
        Seq_N        : Seq_N_T;
        EP_H_Rsnd    : LLU.End_Point_Type;
        EP_R_Creat   : LLU.End_Point_Type;
        Nick         : ASU.Unbounded_String;
        Text         : ASU.Unbounded_String;
        Confirm_Sent : Boolean;
        I            : Positive := 1;
        Found        : Boolean  := False;
        Higher_Seq_N : Boolean  := False;

    begin
        
        -- Extraemos del buffer el primer campo: Msg_Mode
        Msg_Mode := CM.Message_Type'Input (P_Buffer);
        
        
        ------------------------------------------------------------------------        
        --                       Recepción de mensaje INIT                    --
        ------------------------------------------------------------------------
        if Msg_Mode = CM.Init then    
        
            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N := Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd := LLU.End_Point_Type'Input (P_Buffer);
            EP_R_Creat := LLU.End_Point_Type'Input (P_Buffer);
            Nick := ASU.Unbounded_String'Input (P_Buffer);
            
            LLU.Reset (P_Buffer.All);
            
            DB.Put ("RCV Init ", Pantalla.Amarillo);
            DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                         Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick));
            
            -- Comprobamos si EP_H_Creat está en latest_messages
            Check_EP_H (EP_H_Creat, Seq_N, Found, Higher_Seq_N);
            
            -- Si el nick es igual al nuestro y no hemos recibido ya el mensaje...
            if Nick = My_Nick and not Found then
        
                -- Enviamos mensaje Reject al EP_R del nodo que creó el mensaje
                CM.Message_Type'Output (P_Buffer, CM.Reject);
                LLU.End_Point_Type'Output (P_Buffer, My_EP_H);
                ASU.Unbounded_String'Output (P_Buffer, My_Nick);
                
                LLU.Send (EP_R_Creat, P_Buffer);
                LLU.Reset (P_Buffer.All);
        
                DB.Put ("    SEND Reject " ,Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & " " & ASU.To_String(Nick));
                
                -- Añadimos a latest_messages
                Latest_Msgs.Put (Ltst_Msgs_Map, EP_H_Creat, Seq_N, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                DB.Put_Line ("    Añadimos a latest_messages " & Cut_EP(EP_H_Creat) 
                             & Seq_N_T'Image(Seq_N));
                T_IO.New_Line;

                -- Finalizamos el procedimiento
                return;
            end if;
            
            -- Si es vecino lo añadimos a neighbors
            if EP_H_Creat = EP_H_Rsnd then
                Neighbors.Put (Neighbors_Map, EP_H_Creat, A_C.Clock, Success);
                NB_Key_Array := Neighbors.Get_Keys (Neighbors_Map);
                NB_Val_Array := Neighbors.Get_Values (Neighbors_Map);
                DB.Put_Line ("    Añadimos a neighbors " & Cut_EP(EP_H_Creat));
            end if;
            
            -- Si EP_H_Creat no está en latest_messages o Seq_N es mayor...
            if not Found or Higher_Seq_N then
            
                -- Añadimos a latest_messages (actualizamos en caso de Seq_N mayor)
                Latest_Msgs.Put (Ltst_Msgs_Map, EP_H_Creat, Seq_N, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                DB.Put_Line ("    Añadimos a latest_messages " & Cut_EP(EP_H_Creat) 
                             & Seq_N_T'Image(Seq_N));
        
                -- Renviamos el mensaje Init por inundación a nuestros vecinos 
                -- (excepto al que lo ha generado), modificando el campo EP_H_Rsnd.
                CM.Message_Type'Output      (P_Buffer, CM.Init);    -- Init
                LLU.End_Point_Type'Output   (P_Buffer, EP_H_Creat); -- EP_H_Creat
                Seq_N_T'Output              (P_Buffer, Seq_N);      -- Seq_N
                LLU.End_Point_Type'Output   (P_Buffer, My_EP_H);    -- EP_H_Rsnd
                LLU.End_Point_Type'Output   (P_Buffer, EP_R_Creat); -- EP_R_Creat
                ASU.Unbounded_String'Output (P_Buffer, Nick);       -- Nick
                                                                                                            
                Flood_Rsnd_Msg (EP_H_Rsnd, P_Buffer);
                
                DB.Put ("    FLOOD Init ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(My_EP_H) & " ... " & ASU.To_String(Nick));
                
            -- Si EP_H_Creat está en latest_messages y Seq_N es menor o igual
            -- que el del mapa, el mensaje es ignorado.
            else
                DB.Put ("    NOFLOOD Init ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick));
                return;    
            end if;
            
            T_IO.New_Line;


        ------------------------------------------------------------------------        
        --                    Recepción de mensaje CONFIRM                    --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Confirm then

            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N      := Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd  := LLU.End_Point_Type'Input (P_Buffer);
            Nick       := ASU.Unbounded_String'Input (P_Buffer);
            
            LLU.Reset (P_Buffer.All);
            
            DB.Put ("RCV Confirm ", Pantalla.Amarillo);
            DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                         Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick));
                                 
            -- Comprobamos si EP_H_Creat está en latest_messages
            Check_EP_H (EP_H_Creat, Seq_N, Found, Higher_Seq_N);

            -- Si Seq_N es mayor (sólo queda esta posibilidad, ya que al ser un 
            -- CONFIRM, ya hemos tenido que recibir el INIT previamente)...
            if Higher_Seq_N then
                                   
                T_IO.Put_Line (ASU.To_String(Nick) & " ha entrado en el chat");
        
                -- Añadimos a latest_msgs
                Latest_Msgs.Put (Ltst_Msgs_Map, EP_H_Creat, Seq_N, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                DB.Put_Line ("    Añadimos a latest_messages " & Cut_EP(EP_H_Creat) 
                             & Seq_N_T'Image(Seq_N));
        
                -- Renviamos el mensaje Confirm por inundación a nuestros vecinos 
                -- (excepto al que lo ha generado), modificando el campo EP_H_Rsnd.    
                CM.Message_Type'Output      (P_Buffer, CM.Confirm); -- Confirm
                LLU.End_Point_Type'Output   (P_Buffer, EP_H_Creat); -- EP_H_Creat
                Seq_N_T'Output              (P_Buffer, Seq_N);      -- Seq_N
                LLU.End_Point_Type'Output   (P_Buffer, My_EP_H);    -- EP_H_Rsnd
                ASU.Unbounded_String'Output (P_Buffer, Nick);       -- Nick
        
                Flood_Rsnd_Msg (EP_H_Rsnd, P_Buffer);
                
                DB.Put ("    FLOOD Confirm ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(My_EP_H) & " ... " & ASU.To_String(Nick));
                
            -- Si EP_H_Creat está en latest_messages y Seq_N es menor o igual
            -- que el del mapa, el mensaje es ignorado.    
            else
                DB.Put ("    NOFLOOD Confirm ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick));
                return;
            end if;
            
            T_IO.New_Line;
            
            
        ------------------------------------------------------------------------        
        --                     Recepción de mensaje LOGOUT                    --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Logout then

            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat   := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N        := Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd    := LLU.End_Point_Type'Input (P_Buffer);
            Nick         := ASU.Unbounded_String'Input (P_Buffer);
            Confirm_Sent := Boolean'Input (P_Buffer);
            
            LLU.Reset (P_Buffer.All);
            
            DB.Put ("RCV Logout ", Pantalla.Amarillo);
            DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                         Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick) & " " &
                         Boolean'Image (Confirm_Sent));

            -- Comprobamos si EP_H_Creat está en latest_messages
            Check_EP_H (EP_H_Creat, Seq_N, Found, Higher_Seq_N);
            
            -- Si EP_H_Creat no está en latest_messages...
            if not Found then
            
                -- Ignoramos el mensaje para evitar bucle infinito 
                -- (fin del procedimiento)
                return;
                
            -- Si EP_H_Creat está en latest_messsages y su Seq_N es mayor...
            elsif Higher_Seq_N then
            
                -- Borramos de latest_msgs
                Latest_Msgs.Delete (Ltst_Msgs_Map, EP_H_Creat, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                DB.Put_Line ("    Borramos de latest_messages a " & 
                             Cut_EP(EP_H_Creat));
                                 
                -- Si es vecino lo borramos de neighbors
                if EP_H_Creat = EP_H_Rsnd then
                    Neighbors.Delete (Neighbors_Map, EP_H_Creat, Success);
                    NB_Key_Array := Neighbors.Get_Keys (Neighbors_Map);
                    NB_Val_Array := Neighbors.Get_Values (Neighbors_Map);
                    DB.Put_Line ("    Borramos de neighbors a " & 
                                 Cut_EP(EP_H_Creat));
                end if;
                
                -- Si Confirm_Sent está a TRUE, mostramos mensaje
                if Confirm_Sent then
                    T_IO.Put_Line (ASU.To_String(Nick) & " ha abandonado el chat");
                end if;
            
                -- Renviamos el mensaje LOGOUT por inundación a nuestros vecinos 
                -- (excepto al que lo ha generado), modificando el campo EP_H_Rsnd.    
                CM.Message_Type'Output      (P_Buffer, CM.Logout);    -- Logout
                LLU.End_Point_Type'Output   (P_Buffer, EP_H_Creat);   -- EP_H_Creat
                Seq_N_T'Output              (P_Buffer, Seq_N);        -- Seq_N
                LLU.End_Point_Type'Output   (P_Buffer, My_EP_H);      -- EP_H_Rsnd
                ASU.Unbounded_String'Output (P_Buffer, Nick);         -- Nick
                Boolean'Output              (P_Buffer, Confirm_Sent); -- Confirm_Sent
        
                Flood_Rsnd_Msg (EP_H_Rsnd, P_Buffer);
                
                DB.Put ("    FLOOD Logout ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick) & " " 
                             & Boolean'Image (Confirm_Sent));                
                T_IO.New_Line;
            
            -- Si EP_H_Creat está en latest_messages y Seq_N es menor o igual
            -- que el del mapa, el mensaje es ignorado.    
            else
                DB.Put ("    NOFLOOD Logout ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick) & " " 
                             & Boolean'Image (Confirm_Sent));
                return;
            end if;
            
            T_IO.New_Line;
            
            
        ------------------------------------------------------------------------        
        --                     Recepción de mensaje WRITER                    --
        ------------------------------------------------------------------------
        elsif Msg_Mode = CM.Writer then
            
            -- Extraemos del buffer los campos restantes del mensaje
            EP_H_Creat := LLU.End_Point_Type'Input (P_Buffer);
            Seq_N := Seq_N_T'Input (P_Buffer);
            EP_H_Rsnd := LLU.End_Point_Type'Input (P_Buffer);
            Nick := ASU.Unbounded_String'Input (P_Buffer);
            Text := ASU.Unbounded_String'Input (P_Buffer);

            LLU.Reset (P_Buffer.All);
            
            DB.Put ("RCV Writer ", Pantalla.Amarillo);
            DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                         Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick) & " " &
                         ASU.To_String(Text));

            -- Comprobamos si EP_H_Creat está en latest_messages
            Check_EP_H (EP_H_Creat, Seq_N, Found, Higher_Seq_N);
            
            -- Si EP_H_Creat no está en latest_messages o Seq_N es mayor...
            if not Found or Higher_Seq_N then
            
                T_IO.Put_Line(ASU.To_String(Nick) & ": " & ASU.To_String(Text));
            
                -- Añadimos a latest_messages (actualizamos en caso de Seq_N mayor)
                Latest_Msgs.Put (Ltst_Msgs_Map, EP_H_Creat, Seq_N, Success);
                LM_Key_Array := Latest_Msgs.Get_Keys (Ltst_Msgs_Map);
                LM_Val_Array := Latest_Msgs.Get_Values (Ltst_Msgs_Map);
                DB.Put_Line ("    Añadimos a latest_messages " & Cut_EP(EP_H_Creat) 
                             & Seq_N_T'Image(Seq_N));
        
                -- Renviamos el mensaje Writer por inundación a nuestros vecinos 
                -- (excepto al que lo ha generado), modificando el campo EP_H_Rsnd.                                   
                CM.Message_Type'Output      (P_Buffer, CM.Writer);  -- Writer
                LLU.End_Point_Type'Output   (P_Buffer, EP_H_Creat); -- EP_H_Creat
                Seq_N_T'Output              (P_Buffer, Seq_N);      -- Seq_N
                LLU.End_Point_Type'Output   (P_Buffer, My_EP_H);    -- EP_H_Rsnd
                ASU.Unbounded_String'Output (P_Buffer, Nick);       -- Nick
                ASU.Unbounded_String'Output (P_Buffer, Text);       -- Text
        
                Flood_Rsnd_Msg (EP_H_Rsnd, P_Buffer);
                
                DB.Put ("    FLOOD Writer ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(My_EP_H) & " ... " & ASU.To_String(Nick));
                
            -- Si EP_H_Creat está en latest_messages y Seq_N es menor o igual
            -- que el del mapa, el mensaje es ignorado.    
            else
                DB.Put ("    NOFLOOD Writer ", Pantalla.Amarillo);
                DB.Put_Line (Cut_EP(EP_H_Creat) & Seq_N_T'Image(Seq_N) & " " &
                             Cut_EP(EP_H_Rsnd) & " ... " & ASU.To_String(Nick));
                return;    
            end if;
            
            T_IO.New_Line;
            
        end if;
        
    end Handler;
    
end Chat_Handler;
