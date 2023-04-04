# Chat-Application-Using-Erlang
## Chat Application

##### Version 1: Consists Chat Application Implemented using Erlang with In-Memory Data Structures to store Messages, History, etc.

##### Version 2: Added Mnesia DBMS for storing Messages, History, Offline messages, etc.

##### Version 3: Upcomming(OTP Gen_Server)


## App Start Commands:

### Server Terminal:
```
   erl -sname server                => Server_Name.   [ Start Server Terminal with name as "server" ]
   c(server). 
   c(client). 
   server:start(Server_Name,0,MaxUsersAllowedCount).  [ Starts Server ]
```

### Client Terminal:
 ``` 
    erl -sname client              => Client_Name.    [ Start Client Terminal with name as "client" ]
    net_kernel:connect_node(Server_Name).             [ Connect Client Terminal to Server Terminal ]
    c(server). 
    c(client).
    client:start(Server_Name, UserName).              [ Starts Client with UserName ]
```  

## App Functionality Commands:

### Server Terminal:
``` 
   server:get_history(Server_Name).                                         [ Get Chat History ]
   server:get_connected_clients(Server_Name).                               [ Get List of Clients Connected ]
   server:send_message_to_all_clients(Server_Name,"Hello Everyone").        [ Send Message to All Clients Connected ]
   server:make_admin(Server_Name, UserName).                                [ Make a Client as Admin ]
```

### Client Terminal:
```
  client:send_message(Server_Name,UserName,"hi").                                     [ Send Message to Chat Room (Server) ]
  client:exit(Server_Name,UserName).                                                  [ Exit Chat Room ]
  client:send_private_message(Server_Name,Receiver UserName ,Sender UserName,"hi").   [ Send Private Message from Sender to Receiver ]
  client:get_connected_clients(Server_Name,UserName).                                 [ Get List of Clients Connected ]
  client:update_chat_topic(Server_Name,UserName,"New Topic").                         [ Update Chat Topic of Chat Room ]
  client:get_current_chat_topic(Server_Name,UserName).                                [ Get Chat Topic of Chat Room ]
  client:perform_admin_action(Server_Name,AdminUserName,UserName,mute,5).             [ Perform Admin Actions on User (mute/unmute/kick/makeAdmin) ]
  client:get_admins(Server_Name,UserName).                                            [ Get List of Admin Clients Connected ]
  client:set_status(Server_Name,UserName,offline).                                    [ Update Status of Clients (online/offline) ]
```
