-module(client).

-export([start/2, send_message/3, exit/2, send_private_message/4,
  get_connected_clients/2, update_chat_topic/3, get_current_chat_topic/2, perform_admin_action/5, get_admins/2, set_status/3]).

%Main Function
%Also for connection with the Server
start(ServerName,UserName) ->
  ServerPid = global:whereis_name(ServerName),
  UserPid = spawn(fun() -> receives(UserName) end),
  io:format("UserPid ~p ServerPid ~p~n",[UserPid,ServerPid]),
  global:register_name(UserName,UserPid),
  ServerPid!{UserName,UserPid,connect_client_to_server}.

receives(UserName) ->
  receive
    {ServerName, Reason,Msg,connection_failed} ->
      io:format("*Cannot ~p to ~p due to : ~p~n",[Reason, ServerName,Msg]),
      receives(UserName);
    {UserName, _ReceiverName,Msg,private_message} ->
      io:format("*[Private Message][~p]: ~p~n",[UserName,Msg]),
      receives(UserName);
    {ServerName,Clients,connected_clients} ->
      io:format("*All Connected Clients to Server ~p : ~p~n",[ServerName,Clients]),
      receives(UserName);

    {ServerName, MsgHistory,latest_n_history} ->
      N = 2,  % history to return on new joining client
      io:format("*** Welcome to Chat Room ***~n"),
      io:format("*Following is the Chat Message History of Last ~p Messages from Server ~p :~n",[N,ServerName]),

      Len = lenList(MsgHistory),
      if Len < N ->
        DisplayList = lists:reverse(MsgHistory),
        lists:foreach(fun({_TableName,Time,From,Message}) ->
          {{Year,Month,Day},{Hour,Min,Sec}} = Time,
          MsgTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
          io:format("~p ~p: ~p~n",[MsgTime,From,Message]) end, DisplayList);
        true ->
          LastNMsgs = lists:nthtail(Len-N,MsgHistory),
          DisplayList = lists:reverse(LastNMsgs),
          lists:foreach(fun({_TableName,Time,From,Message}) ->
            {{Year,Month,Day},{Hour,Min,Sec}} = Time,
            MsgTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
            io:format("~p ~p: ~p~n",[MsgTime,From,Message]) end, DisplayList)
      end,
      receives(UserName);

    {ServerName,Topic,current_topic} ->
      io:format("*Current Chat Room Topic : ~p on Server : ~p~n",[Topic,ServerName]),
      receives(UserName);

    {ServerName,Msg,announcement} ->
      io:format("*Announcement For Everyone From Server : ~p  : ~p~n",[ServerName,Msg]),
      receives(UserName);

    {ServerName, AdminKeys, list_of_admins} ->
      io:format("*All Admin Clients on Server ~p : ~p~n",[ServerName,AdminKeys]),
      receives(UserName);

    {UserName, ReceiverName, offline_message} ->
      io:format("* ~p is offline and will receive messages once back.~n",[ReceiverName]),
      receives(UserName);

    {ServerName, MsgHistory,offline_message_history} ->
      io:format("*Following is the Private Chat Message History you Received When you were offline from Server ~p :~n",[ServerName]),
      DisplayList = lists:reverse(MsgHistory),
      lists:foreach(fun({_TableName,Time,_To,Message,From}) ->
        {{Year,Month,Day},{Hour,Min,Sec}} = Time,
        MsgTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
        io:format("~p ~p: ~p~n",[MsgTime,From,Message]) end, DisplayList),
      receives(UserName)


  end.


send_message(ServerName,UserName,Msg) ->
  global:send(ServerName,{UserName,Msg,send_message}).

exit(ServerName,UserName) ->
  global:send(ServerName,{UserName,exit}).

send_private_message(ServerName, UserName, ReceiverName, Msg) ->
  global:send(ServerName,{UserName,ReceiverName,Msg,send_private_message}).

get_connected_clients(ServerName,UserName) ->
  global:send(ServerName,{UserName,get_connected_clients}).

update_chat_topic(ServerName, UserName, NewTopic) ->
  global:send(ServerName,{NewTopic,UserName,update_topic}).

get_current_chat_topic(ServerName, UserName) ->
  global:send(ServerName,{UserName,get_current_topic}).

perform_admin_action(ServerName,AdminName,UserName, Action,Time) ->  % mute/unmute/kick/makeAdmin
  global:send(ServerName,{AdminName,UserName,Action,Time, perform_admin_action}).

get_admins(ServerName,UserName) ->
  global:send(ServerName,{UserName,get_admins}).

set_status(ServerName, UserName, Status) ->  % online / offline
  global:send(ServerName,{UserName, Status, update_status}).

lenList([]) -> 0;
lenList([_|T]) -> 1 + lenList(T).



