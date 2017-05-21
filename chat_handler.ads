--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--    Contiene las instanciaciones de los mapas genéricos necesarios en       --
--    Chat-Peer y el procedimiento para el envío y recepción de mensajes      --
--    mediante Handler                                                        --
--                                                                            --
--    Autor: Álvaro Moles Vinader                                             --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Lower_Layer_UDP;
with Maps_G;
with Maps_Protector_G;
with Chat_Messages;
with Debug;
with Pantalla;

package Chat_Handler is

    package T_IO renames Ada.Text_IO;
    package A_C  renames Ada.Calendar;
    package ASU  renames Ada.Strings.Unbounded;
    package C_L  renames Ada.Command_Line;
    package LLU  renames Lower_Layer_UDP;
    package CM   renames Chat_Messages;
    package DB   renames Debug;
    package PNT  renames Pantalla;
    
    use type LLU.End_Point_Type;
    use type CM.Message_Type;
    use type ASU.Unbounded_String;
    
    function Image_Time (T: Ada.Calendar.Time) return string;
    function Cut_EP (EP: LLU.End_Point_Type) return string;
    
    type Seq_N_T is mod Integer'Last;
    
    package NP_Neighbors is new Maps_G (Key_Type        => LLU.End_Point_Type,
                                        Value_Type      => A_C.Time,
                                        "="             => LLU."=",
                                        Key_To_String   => Cut_EP,
                                        Value_To_String => Image_Time,
                                        Null_Key        => null,
                                        Null_Value      => A_C.Clock,
                                        Max_Length      => 10);
                                
    package NP_Latest_Msgs is new Maps_G (Key_Type        => LLU.End_Point_Type,
                                          Value_Type      => Seq_N_T,
                                          "="             => LLU."=",
                                          Key_To_String   => Cut_EP,
                                          Value_To_String => Seq_N_T'Image,
                                          Null_Key        => null,
                                          Null_Value      => 0,
                                          Max_Length      => 50);
    
    package Neighbors is new Maps_Protector_G (NP_Neighbors);
    package Latest_Msgs is new Maps_Protector_G (NP_Latest_Msgs);
    
    Neighbors_Map : Neighbors.Prot_Map;
    Ltst_Msgs_Map : Latest_Msgs.Prot_Map;
   
    NB_Key_Array : Neighbors.Keys_Array_Type;
    NB_Val_Array : Neighbors.Values_Array_Type;
    LM_Key_Array : Latest_Msgs.Keys_Array_Type;
    LM_Val_Array : Latest_Msgs.Values_Array_Type;
    
    My_EP_H : LLU.End_Point_Type;
    My_Nick : ASU.Unbounded_String;
    
    Seq_N : Seq_N_T := 0;
    
    Success : Boolean;
    
    procedure Handler (From     : in LLU.End_Point_Type; 
                       To       : in LLU.End_Point_Type; 
                       P_Buffer : access LLU.Buffer_Type);
    
    function Seq_N_Increased (N: Seq_N_T) return Seq_N_T;

end Chat_Handler;
