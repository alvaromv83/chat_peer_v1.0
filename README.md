# chat_peer_v1.0

Chat_Peer v1.0 es un programa en lenguaje ADA, que ofrece un servicio de chat
entre usuarios utilizando el modelo P2P descentralizado. Se ejecuta desde un
terminal en GNU/Linux bajo la biblioteca Lower_Layer.

Cada usuario dentro de la red, debe compilar el programa desde un terminal y
ejecutarlo indicando su número de puerto y nombre de usuario, así como el nombre
de host y número de puerto de su o sus vecinos iniciales (máximo 2), en
caso de tenerlos. Se puede ejecutar a modo de prueba en un solo ordenador,
utilizando un terminal diferente por usuario y la dirección IP de loopback.
En tal caso se indicará el propio nombre de host para todos los vecinos
iniciales, asignando un puerto diferente a cada uno.

La información de debug está activada por defecto.

Para instalar la biblioteca Lower_Layer en Linux ver "lower_layer_LEEME.txt"

Para compilar el programa (desde Lower_Layer):
gnatmake -I/usr/local/ll/lib chat_peer.adb

Para ejecutar el programa (con 0, 1 o 2 vecinos iniciales):
./chat_peer my_port my_nickname [[neighbor1_host neighbor1_port] [neighbor2_host neighbor2_port]]



EJEMPLOS DE CONFIGURACIONES

    (1)---(4)
     |     |
    (2)   (5)
     |     |
    (3)   (6)

HOST 1 --> ./chat_peer 1111 User1

HOST 2 --> ./chat_peer 2222 User2 <host1_name> 1111

HOST 3 --> ./chat_peer 3333 User3 <host2_name> 2222

HOST 4 --> ./chat_peer 4444 User4 <host1_name> 1111

HOST 5 --> ./chat_peer 5555 User5 <host4_name> 4444

HOST 6 --> ./chat_peer 6666 User6 <host5_name> 5555




        (7)
     /       \
    (1)     (4)
     |       |
    (2)     (5)
     |       |
    (3)     (6)

HOST 1 --> ./chat_peer 1111 User1

HOST 2 --> ./chat_peer 2222 User2 <host1_name> 1111

HOST 3 --> ./chat_peer 3333 User3 <host2_name> 2222

HOST 4 --> ./chat_peer 4444 User4 

HOST 5 --> ./chat_peer 5555 User5 <host4_name> 4444

HOST 6 --> ./chat_peer 6666 User6 <host5_name> 5555

HOST 7 --> ./chat_peer 7777 User7 <host1_name> 1111 <host4_name> 4444





    (1)---(4)
     |     |
    (2)---(3)
           |
          (5)
           |
          (6)
            

HOST 1 --> ./chat_peer 1111 User1

HOST 2 --> ./chat_peer 2222 User2 <host1_name> 1111

HOST 3 --> ./chat_peer 3333 User3 <host2_name> 2222

HOST 4 --> ./chat_peer 4444 User4 <host1_name> 1111 <host3_name> 3333

HOST 5 --> ./chat_peer 5555 User5 <host3_name> 3333

HOST 6 --> ./chat_peer 6666 User6 <host5_name> 5555
