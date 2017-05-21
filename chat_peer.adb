--------------------------------------------------------------------------------
-------------------------------------------------------------------------------- --                                                                            --
--                             C h a t - P e e r                              --    
--                                   v 1.0                                    --    
--                                                                            --
--      Ofrece un servicio de chat entre usuarios utilizando el modelo P2P    --
--      descentralizado                                                       --                   --                                                                            --
--                                                                            --
-- Autor: Álvaro Moles Vinader                                                --
--                                                                            --          
-- Asignatura "Informática II", Grado en Ingeniería en Sistemas Audiovisuales --
-- y Multimedia. Universidad Rey Juan Carlos. Madrid. Curso 2012-2013.        --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Calendar;
with Ada.Command_Line;
with Ada.Exceptions;
with Gnat.Calendar.Time_IO;
with Lower_Layer_UDP;
with Chat_Handler;
with Maps_G;
with Maps_Protector_G;
with Debug;
with Pantalla;
with Chat_Messages;

procedure Chat_Peer is

    package T_IO   renames Ada.Text_IO;
    package ASU    renames Ada.Strings.Unbounded;
    package ASU_IO renames Ada.Strings.Unbounded.Text_IO;
    package A_C    renames Ada.Calendar;
    package C_L    renames Ada.Command_Line;
    package C_IO   renames Gnat.Calendar.Time_IO;
    package LLU    renames Lower_Layer_UDP;
    package CH     renames Chat_Handler;
    package DB     renames Debug;
    package PNT    renames Pantalla;
    package CM     renames Chat_Messages;
    
    use type LLU.End_Point_Type;
    use type ASU.Unbounded_String;
    use type CM.Message_Type;
    
    Usage_Error: exception;
    
    -- Procedimiento para el envío de mensajes por inundación
    procedure Flood_Send_Msg (P_Buffer: access LLU.Buffer_Type) is
        I: Positive := 1;
    begin
        while I <= CH.Neighbors.Map_Length(CH.Neighbors_Map) and 
              CH.NB_Key_Array(I) /= null loop
                                             
            LLU.Send (CH.NB_Key_Array(I), P_Buffer);
            DB.Put_Line("        send to " & CH.Cut_EP(CH.NB_Key_Array(I)));
            I := I + 1;
        end loop;
        
        LLU.Reset(P_Buffer.All);

    end Flood_Send_Msg;
    
    -- Declaración de variables
    My_EP_R : LLU.End_Point_Type;
    NB_1_EP : LLU.End_Point_Type := null;
    NB_2_EP : LLU.End_Point_Type := null;
    
    Port      : Integer;
    NB_1_Host : ASU.Unbounded_String := ASU.Null_Unbounded_String;
    NB_1_Port : Integer              := 0;
    NB_2_Host : ASU.Unbounded_String := ASU.Null_Unbounded_String;
    NB_2_Port : Integer              := 0;
    
    Msg_Mode : CM.Message_Type;
    EP_H     : LLU.End_Point_Type;
    Nick     : ASU.Unbounded_String;
    Buffer   : aliased LLU.Buffer_Type(1024);
    
    Text    : ASU.Unbounded_String;
    Success : Boolean;
   
    Expired : Boolean;
    Finish  : Boolean := False;
    
begin

    -- Mensajes debug activados por defecto
    CM.Status := True;
    DB.Set_Status(CM.Status);

    ----------------------------------------------------------------------------        
    --                               Fase de Inicialización                   --
    ----------------------------------------------------------------------------

    -- Evaluamos los argumentos:
    -- Si no hay vecinos iniciales...
    if C_L.Argument_Count = 2 then
        Port       := Integer'Value           (C_L.Argument(1));
        CH.My_Nick := ASU.To_Unbounded_String (C_L.Argument(2));
        DB.Put_Line ("NO hacemos protocolo de admisión porque no tenemos " & 
                     "contactos iniciales...");
    
    -- Si hay un vecino inicial...                     
    elsif C_L.Argument_Count = 4 then
        Port       := Integer'Value           (C_L.Argument(1));
        CH.My_Nick := ASU.To_Unbounded_String (C_L.Argument(2));
        NB_1_Host  := ASU.To_Unbounded_String (C_L.Argument(3));
        NB_1_Port  := Integer'Value           (C_L.Argument(4));

        -- Construímos el EP del vecino
        NB_1_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_1_Host)), NB_1_Port);
        
        -- Añadimos el vecino a la tabla neighbors
        CH.Neighbors.Put (CH.Neighbors_Map, NB_1_EP, A_C.Clock, Success);
        DB.Put_Line ("Añadimos a neighbors " & CH.Cut_EP(NB_1_EP));
        T_IO.New_Line;

    -- Si hay dos vecinos iniciales...
    elsif C_L.Argument_Count = 6 then
        Port       := Integer'Value           (C_L.Argument(1));
        CH.My_Nick := ASU.To_Unbounded_String (C_L.Argument(2));
        NB_1_Host  := ASU.To_Unbounded_String (C_L.Argument(3));
        NB_1_Port  := Integer'Value           (C_L.Argument(4));
        NB_2_Host  := ASU.To_Unbounded_String (C_L.Argument(5));
        NB_2_Port  := Integer'Value           (C_L.Argument(6));

        -- Construímos los EP de los vecinos
        NB_1_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_1_Host)), NB_1_Port);
        NB_2_EP := LLU.Build (LLU.To_IP(ASU.To_String(NB_2_Host)), NB_2_Port);
        
        -- Añadimos los vecinos a la tabla neighbors
           CH.Neighbors.Put (CH.Neighbors_Map, NB_1_EP, A_C.Clock, Success);
           DB.Put_Line ("Añadimos a neighbors " & CH.Cut_EP(NB_1_EP));
           
       CH.Neighbors.Put (CH.Neighbors_Map, NB_2_EP, A_C.Clock, Success);
           DB.Put_Line ("Añadimos a neighbors " & CH.Cut_EP(NB_2_EP));
        T_IO.New_Line;
    else
        raise Usage_Error;
    end if;

    -- Actualizamos los arrays de vecinos
    CH.NB_Key_Array := CH.Neighbors.Get_Keys (CH.Neighbors_Map);
    CH.NB_Val_Array := CH.Neighbors.Get_Values (CH.Neighbors_Map);
    
    -- Construimos nuestros EP_R y EP_H y nos atamos
    LLU.Bind_Any (My_EP_R);
    CH.My_EP_H := LLU.Build (LLU.To_IP(LLU.Get_Host_Name), Port);
    LLU.Bind (CH.My_EP_H, CH.Handler'Access); -- Recibimos mensajes en el handler 
                                              -- (el programa continúa)
    LLU.reset (Buffer);
    

    ----------------------------------------------------------------------------        
    --                           Protocolo de Admisión                        --
    ----------------------------------------------------------------------------
    
    -- Si tenemos vecinos iniciales...
    if C_L.Argument_Count /= 2 then
    
        DB.Put_Line ("Iniciando protocolo de admisión...");
        
        -- Enviamos el mensaje INIT por inundación a nuestros vecinos
        
        CH.Seq_N := CH.Seq_N_Increased(CH.Seq_N);
        
          CH.Latest_Msgs.Put (CH.Ltst_Msgs_Map, CH.My_EP_H, CH.Seq_N, Success);     
        CH.LM_Key_Array := CH.Latest_Msgs.Get_Keys (CH.Ltst_Msgs_Map);
        CH.LM_Val_Array := CH.Latest_Msgs.Get_Values (CH.Ltst_Msgs_Map);
        DB.Put_Line ("Añadimos a latest_messages " & CH.Cut_EP(CH.My_EP_H) & 
                     CH.Seq_N_T'Image(CH.Seq_N));

        CM.Message_Type'Output      (Buffer'Access, CM.Init);      -- Init
        LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H);   -- EP_H_Creat
        CH.Seq_N_T'Output           (Buffer'Access, CH.Seq_N);     -- Seq_N
        LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H);   -- EP_H_Rsnd
        LLU.End_Point_Type'Output   (Buffer'Access, My_EP_R);      -- EP_R
        ASU.Unbounded_String'Output (Buffer'Access, CH.My_Nick);   -- Nick

        Flood_Send_Msg (Buffer'Access);
        
        DB.Put ("FLOOD Init ", Pantalla.Amarillo);
        DB.Put_Line (CH.Cut_EP(CH.My_EP_H) & CH.Seq_N_T'Image(CH.Seq_N) & " " &
                     CH.Cut_EP(CH.My_EP_H) & " ... " & ASU.To_String(CH.My_Nick));
        
        -- Esperamos la respuesta en My_EP_R
        LLU.Receive (My_EP_R, Buffer'Access, 2.0, Expired);
        
        -- Si ha expirado el tiempo (2s)...
        if Expired then

            -- Enviamos mensaje CONFIRM por inundación
            CH.Seq_N := CH.Seq_N_Increased(CH.Seq_N);
            
            CH.Latest_Msgs.Put (CH.Ltst_Msgs_Map, CH.My_EP_H, CH.Seq_N, Success);
            CH.LM_Key_Array := CH.Latest_Msgs.Get_Keys (CH.Ltst_Msgs_Map);
            CH.LM_Val_Array := CH.Latest_Msgs.Get_Values (CH.Ltst_Msgs_Map);
            DB.Put_Line ("Añadimos a latest_messages " & CH.Cut_EP(CH.My_EP_H) & 
                         CH.Seq_N_T'Image(CH.Seq_N));    
                             
            CM.Message_Type'Output      (Buffer'Access, CM.Confirm);  -- Confirm
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H);  -- EP_H_Creat
            CH.Seq_N_T'Output           (Buffer'Access, CH.Seq_N);    -- Seq_N
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H);  -- EP_H_Rsnd
            ASU.Unbounded_String'Output (Buffer'Access, CH.My_Nick);  -- Nick
                             
            Flood_Send_Msg (Buffer'Access);
            
            DB.Put ("FLOOD Confirm ", Pantalla.Amarillo);
            DB.Put_Line (CH.Cut_EP(CH.My_EP_H) & CH.Seq_N_T'Image(CH.Seq_N) & " " 
                         & CH.Cut_EP(CH.My_EP_H) & " ... " & 
                         ASU.To_String(CH.My_Nick));
        
        -- Si se recibe respuesta antes de (2s)...    
        else
        
            Msg_Mode := CM.Message_Type'Input (Buffer'Access);
            EP_H     := LLU.End_Point_Type'Input (Buffer'Access);
            Nick     := ASU.Unbounded_String'Input (Buffer'Access);
            
            LLU.Reset(Buffer);
            
            -- Si se recibe mensaje REJECT
            if Msg_Mode = CM.Reject then
                DB.Put ("RCV Reject ", Pantalla.Amarillo);
                DB.Put_Line (CH.Cut_EP(EP_H) & " " & ASU.To_String(Nick));
                T_IO.Put_Line("Usuario rechazado porque " & CH.Cut_EP(EP_H) & 
                              " está usando el mismo nick");
                
                -- Enviamos mensaje LOGOUT por inundación
                CH.Seq_N := CH.Seq_N_Increased(CH.Seq_N);
                
                CM.Message_Type'Output      (Buffer'Access, CM.Logout);  -- Logout
                LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Creat
                CH.Seq_N_T'Output           (Buffer'Access, CH.Seq_N);   -- Seq_N
                LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Rsnd
                ASU.Unbounded_String'Output (Buffer'Access, CH.My_Nick); -- Nick
                Boolean'Output              (Buffer'Access, False);      -- Confirm_Sent

                Flood_Send_Msg (Buffer'Access);
                
                DB.Put ("FLOOD Logout ", Pantalla.Amarillo);
                DB.Put_Line (CH.Cut_EP(CH.My_EP_H) & CH.Seq_N_T'Image(CH.Seq_N) & 
                             " " & CH.Cut_EP(CH.My_EP_H) & " ... " & 
                             ASU.To_String(CH.My_Nick) & " FALSE");
                        
                DB.Put_Line ("Fin del Protocolo de Admisión");
                
                -- Finalizamos el programa
                LLU.Finalize;
                return;
            end if;
            
        end if;
        
        DB.Put_Line ("Fin del Protocolo de Admisión");
        T_IO.New_Line;
        
    end if;


    ----------------------------------------------------------------------------        
    --            Protocolo de Envío y Recepción de mensajes Writer           --
    ----------------------------------------------------------------------------    

    -- Entramos en el chat
    T_IO.Put_Line ("Peer-Chat v1.0");
    T_IO.Put_Line ("==============");
    T_IO.New_Line;
    
    T_IO.Put_Line("Entramos en el chat con el nick: " & 
                  ASU.To_String(CH.My_Nick));
    T_IO.Put_Line (".h para help");    
    
    while not Finish loop
        
        -- Leemos mensajes de la entrada
        Text := ASU_IO.Get_Line;
        
        -- Evaluamos los comandos
        
        -- Mostrar ayuda
        if Text = ".h" or Text = ".help" then
            T_IO.Put_Line("         Comandos                 Efectos");
            T_IO.Put_Line("         =================        =================");
            T_IO.Put_Line("         .nb      .neighbors      lista de vecinos");
            T_IO.Put_Line("         .lm      .latest_msgs    lista de últimos mensajes recibidos");
            T_IO.Put_Line("         .db      .debug          toggle para info de debug");
            T_IO.Put_Line("         .h       .help           muestra esta información de ayuda");
            T_IO.Put_Line("         .salir                   termina el programa");
        
        -- Activar / desactivar información de debug    
        elsif Text = ".db" or Text = ".debug" then
            CM.Status := not CM.Status;
            DB.Set_Status(CM.Status);
            
            if CM.Status then
                T_IO.Put_Line("Activada información de debug");
            else
                T_IO.Put_Line("Desactivada información de debug");
            end if;
        
        -- Mostrar la tabla neighbor    
        elsif Text = ".nb" or Text = ".neighbors" then
            T_IO.New_Line;
           CH.Neighbors.Print_Map (CH.Neighbors_Map);
       
       -- Mostrar la tabla latest_messages        
        elsif Text = ".lm" or Text = ".latest_messages" then
            T_IO.New_Line;
            CH.Latest_Msgs.Print_Map (CH.Ltst_Msgs_Map);
        
        -- Salir del chat
        elsif Text = ".salir" then
        
            -- Enviamos mensaje LOGOUT por inundación
            CH.Seq_N := CH.Seq_N_Increased(CH.Seq_N);
            
            CM.Message_Type'Output      (Buffer'Access, CM.Logout);  -- Logout
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Creat
            CH.Seq_N_T'Output           (Buffer'Access, CH.Seq_N);   -- Seq_N
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Rsnd
            ASU.Unbounded_String'Output (Buffer'Access, CH.My_Nick); -- Nick
            Boolean'Output              (Buffer'Access, True);       -- Confirm_Sent
            
            Flood_Send_Msg (Buffer'Access);
            
            T_IO.New_Line;
                
            DB.Put ("FLOOD Logout ", Pantalla.Amarillo);
            DB.Put_Line (CH.Cut_EP(CH.My_EP_H) & CH.Seq_N_T'Image(CH.Seq_N) & 
                         " " & CH.Cut_EP(CH.My_EP_H) & " ... " & 
                         ASU.To_String(CH.My_Nick) & " TRUE");
        
            -- Finalizamos el programa
            LLU.Finalize;
            Finish := True;
        
        -- Si escribimos cualquier otra cadena de texto...    
        else
        
            -- Enviamos mensaje WRITER por inundación
            CH.Seq_N := CH.Seq_N_Increased(CH.Seq_N);
            
            CH.Latest_Msgs.Put (CH.Ltst_Msgs_Map, CH.My_EP_H, CH.Seq_N, Success);
            CH.LM_Key_Array := CH.Latest_Msgs.Get_Keys (CH.Ltst_Msgs_Map);
            CH.LM_Val_Array := CH.Latest_Msgs.Get_Values (CH.Ltst_Msgs_Map);
            DB.Put_Line ("Añadimos a latest_messages " & CH.Cut_EP(CH.My_EP_H) & 
                         CH.Seq_N_T'Image(CH.Seq_N));    
                             
            CM.Message_Type'Output      (Buffer'Access, CM.Writer);  -- Writer
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Creat
            CH.Seq_N_T'Output           (Buffer'Access, CH.Seq_N);   -- Seq_N
            LLU.End_Point_Type'Output   (Buffer'Access, CH.My_EP_H); -- EP_H_Rsnd
            ASU.Unbounded_String'Output (Buffer'Access, CH.My_Nick); -- Nick
            ASU.Unbounded_String'Output (Buffer'Access, Text);       -- Text
                             
            Flood_Send_Msg (Buffer'Access);
            
            DB.Put ("FLOOD Writer ", Pantalla.Amarillo);
            DB.Put_Line (CH.Cut_EP(CH.My_EP_H) & CH.Seq_N_T'Image(CH.Seq_N) & " " 
                         & CH.Cut_EP(CH.My_EP_H) & " ... " & 
                         ASU.To_String(CH.My_Nick));
            
        end if;
        T_IO.New_Line;
    end loop;
    
--------------------------------------------------------------------------------        
--                               Excepciones                                  --
--------------------------------------------------------------------------------
exception
    when Usage_Error =>
        T_IO.Put_Line ("Usage: ./chat_peer port nickname " &
                       "[[NB_host NB_port] " & 
                       "[NB_host NB_port]]");
        LLU.Finalize;
    when Except:others =>
        T_IO.Put_Line ("Excepción imprevista: " &
                       Ada.Exceptions.Exception_Name(Except) & " en: " &
                       Ada.Exceptions.Exception_Message (Except));
        LLU.Finalize;
        
end Chat_Peer;
