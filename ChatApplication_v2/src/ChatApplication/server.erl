-module(server).
-export([start/3, get_connected_clients/1, send_message_to_all_clients/2, make_admin/2, get_history/1]).

%% IMPORTANT: The next line must be included if we want to call qlc:q(...)
-include_lib("stdlib/include/qlc.hrl").

-record(message_history, {time, username, msg}).
-record(offline_message_history, {time, sendername, msg, username}).

do_this_once() ->
  mnesia:create_schema([node()]),  % create schema on current node.
  mnesia:start(),
  mnesia:create_table(message_history, [{attributes, record_info(fields, message_history)}]),
  mnesia:create_table(offline_message_history, [{attributes, record_info(fields, offline_message_history)}]),
  mnesia:stop().

start(ServerName,CurrentUsers,MaxUsers) ->
  do_this_once(),
  mnesia:start(),
  mnesia:wait_for_tables([message_history,offline_message_history], 20000),
  reset_tables(),

  UsersMap = #{},
  Topic = "",
  ServerPid = spawn(fun() -> listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic) end),
  io:format("Server Started with PID ~p~n",[ServerPid]),
  global:register_name(ServerName,ServerPid).

listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic) ->
  receive
    {UserName,UserPid,connect_client_to_server} ->
      CurrentUsers = map_size(UsersMap),
      Check1 = maps:is_key(UserName,UsersMap),

      if Check1 =:= true ->
        global:send(UserName,{ServerName, "Establish Connection","UserName already been used",connection_failed}),
        listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
        true -> ok
        end,

      if
        CurrentUsers < MaxUsers ->
          User = #{userPid => UserPid, isAdmin => false, isMuted => false, mutedTill => 0, status => online},
          TempMap = maps:put(UserName,User,UsersMap),
          io:format("\n[" ++ get_time() ++ "]"),
          io:format("*New Client Added To Group:~p~n",[UserName]),
          History = do(qlc:q([X || X <- mnesia:table(message_history)])),
          MsgHistory = lists:sort(History),
          global:send(UserName,{ServerName, MsgHistory,latest_n_history}),
          listening(ServerName,TempMap,CurrentUsers+1,MaxUsers,Topic);
        true -> global:send(UserName,{ServerName, "Establish Connection","Server User Limit Exceeded",connection_failed}),
          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
      end;

    {UserName,Msg,send_message} ->
      UserExists = maps:is_key(UserName,UsersMap),
      if
        UserExists =:= true ->
          User = maps:get(UserName,UsersMap),
          CheckMute = maps:get(isMuted,User),
          if
            CheckMute =:= false ->
              io:format("\n[" ++ get_time() ++ "]"),
              io:format("~p: ~p~n",[UserName,Msg]),
              insert_into_message_history(UserName, Msg),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
            true ->
              MuteEndTime = maps:get(mutedTill,User),
              CheckMuteTime = is_expired_time(MuteEndTime),
              if
                CheckMuteTime =:= true ->
                  TempUser = maps:put(isMuted,false,User),
                  TempMap = maps:put(UserName,TempUser,UsersMap),
                  io:format("\n[" ++ get_time() ++ "]"),
                  io:format("~p: ~p~n",[UserName,Msg]),
                  insert_into_message_history(UserName, Msg),
                  listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);
                true ->
                  {{Year,Month,Day},{Hour,Min,Sec}} = MuteEndTime,
                  MuteTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
                  global:send(UserName,{ServerName,"Send Message","User is Muted "++ MuteTime,connection_failed}),
                  listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
              end

          end;
      true -> ok
      end;
    {UserName,exit} ->
      UserExists = maps:is_key(UserName,UsersMap),
      if
        UserExists =:= true ->
          io:format("\n[" ++ get_time() ++ "]"),
          TempMap = maps:remove(UserName,UsersMap),
          io:format("*~p Left the Group: ~n",[UserName]),
          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);
      true -> ok
      end;

    {UserName,ReceiverName,Msg,send_private_message} ->
      UserExists = maps:is_key(UserName,UsersMap),
      ReceiverExists = maps:is_key(UserName,UsersMap),
      Flag1 = UserExists == true,
      Flag = Flag1 and ReceiverExists,
      if
        Flag =:= true ->
          User = maps:get(UserName,UsersMap),
          ReceiverStatus = maps:get(status,User),
          if
            ReceiverStatus =:= offline ->
              insert_into_offline_message_history(ReceiverName,UserName,Msg),
              global:send(ReceiverName,{ReceiverName,UserName,offline_message}),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
            true ->
              global:send(UserName,{UserName,ReceiverName,Msg,private_message}),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
          end;
        true -> global:send(UserName,{ServerName,"Send Message","UserName Not Found",connection_failed}),
          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
      end;

    {get_connected_clients} ->
      Clients = maps:keys(UsersMap),
      io:format("\n[" ++ get_time() ++ "]"),
      io:format("*Clients Connected to Server :~p~n",[Clients]),
      listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);

    {UserName,get_connected_clients} ->
      Clients = maps:keys(UsersMap),
      global:send(UserName,{ServerName,Clients,connected_clients}),
      listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);

    {NewTopic,UserName,update_topic} ->
      UserExists = maps:is_key(UserName,UsersMap),
      if
        UserExists =:= true ->
          User = maps:get(UserName,UsersMap),
          CheckAdmin = maps:get(isAdmin,User),
          if
            CheckAdmin =:= true ->
              io:format("\n[" ++ get_time() ++ "]"),
              io:format("*~p Updated the Chat Room Topic ~p~n",[UserName,NewTopic]),
              Msg = "*Chat Room Topic to " ++ NewTopic,
              global:send(ServerName,{Msg, send_message_to_all_clients}),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,NewTopic);
            true ->
              global:send(UserName,{ServerName, "Change the Topic ","You are Not Admin",connection_failed}),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
          end;
        true -> ok
      end;

    {UserName,get_current_topic} ->
      global:send(UserName,{ServerName,Topic,current_topic}),
      listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);

    {Msg, send_message_to_all_clients} ->
      Clients = maps:keys(UsersMap),
      lists:foreach(fun(ClientName) ->
        global:send(ClientName,{ServerName,Msg,announcement}) end, Clients),
        listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);

    {UserName, make_admin} ->
      UserExists = maps:is_key(UserName,UsersMap),
      if
        UserExists =:= true ->
          User = maps:get(UserName,UsersMap),
          TempUser = maps:put(isAdmin,true,User),
          TempMap = maps:put(UserName,TempUser,UsersMap),
          io:format("\n[" ++ get_time() ++ "]"),
          io:format("*~p made as Admin~n",[UserName]),
          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);
        true -> ok
      end;

    {AdminName,UserName,Action,Time,perform_admin_action} ->
      AdminExists = maps:is_key(AdminName,UsersMap),
      if
        AdminExists =:= true ->
          AdminUser = maps:get(AdminName,UsersMap),
          CheckAdmin = maps:get(isAdmin,AdminUser),
          if
            CheckAdmin =:= true ->
              UserExists = maps:is_key(UserName,UsersMap),
              if
                UserExists =:= true ->
                  case Action of
                    kick ->
                      TempMap = maps:remove(UserName,UsersMap),
                      io:format("\n[" ++ get_time() ++ "]"),
                      io:format("*~p Kicked ~p from the Group ~n",[AdminName,UserName]),
                      listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);

                    mute  ->
                      User = maps:get(UserName,UsersMap),
                      AlreadyMuted = maps:get(isMuted,User),
                      if
                        AlreadyMuted =:= true ->
                          ok,
                          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
                        true ->
                          TempUser1 = maps:put(isMuted,true,User),
                          MutedTill = add_time(Time),
                          TempUser2 = maps:put(mutedTill,MutedTill,TempUser1),
                          TempMap = maps:put(UserName,TempUser2,UsersMap),
                          {{Year,Month,Day},{Hour,Min,Sec}} = MutedTill,
                          MuteTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
                          io:format("\n[" ++ get_time() ++ "]"),
                          io:format("*~p Muted ~p in the Group Till ~p ~n",[AdminName,UserName,MuteTime]),
                          timer:send_after(Time*60*1000, self(), {AdminName,UserName,unmute,Time,perform_admin_action}),
                          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic)
                      end;

                    unmute ->
                      User = maps:get(UserName,UsersMap),
                      AlreadyUnMuted = maps:get(isMuted,User),
                      if
                        AlreadyUnMuted =:= false ->
                          ok,
                          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
                        true ->
                          TempUser = maps:put(isMuted,false,User),
                          TempMap = maps:put(UserName,TempUser,UsersMap),
                          io:format("\n[" ++ get_time() ++ "]"),
                          io:format("*~p UnMuted ~p in the Group ~n",[AdminName,UserName]),
                          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic)
                      end;

                    makeAdmin ->
                      User = maps:get(UserName,UsersMap),
                      AlreadyAdmin = maps:get(isAdmin,User),
                      if
                        AlreadyAdmin =:= true ->
                          ok,
                          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);
                        true ->
                          TempUser = maps:put(isAdmin,true,User),
                          TempMap = maps:put(UserName,TempUser,UsersMap),
                          io:format("\n[" ++ get_time() ++ "]"),
                          io:format("*~p Promoted ~p to Admin in the Group ~n",[AdminName,UserName]),
                          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic)
                      end

                  end;
                true -> ok
              end;
            true ->
              global:send(AdminName,{ServerName, "Permform action on The User","You are Not Admin",connection_failed}),
              listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
          end;
        true ->
          global:send(AdminName,{ServerName, "Permform action on The User","Admin Name Does not Exists",connection_failed}),
          listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)
      end;

    {UserName,get_admins} ->
      AdminKeys = [Key || {Key, Value} <- maps:to_list(UsersMap), maps:get(isAdmin, Value) =:= true],
      global:send(UserName,{ServerName, AdminKeys, list_of_admins}),
      listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic);

    {UserName,Status,update_status} ->
      UserExists = maps:is_key(UserName,UsersMap),
      if
        UserExists =:= true ->
          User = maps:get(UserName,UsersMap),
          TempUser1 = maps:put(status,Status,User),
          TempMap = maps:put(UserName,TempUser1,UsersMap),
          io:format("\n[" ++ get_time() ++ "]"),
          io:format("*~p is ~p now ~n",[UserName,Status]),

          if
            Status =:= online ->
              PersonalHistory = get_offline_message_history(UserName),
              delete_offline_message_history(UserName),
              SortedHistory = lists:sort(PersonalHistory),
              global:send(UserName,{ServerName, SortedHistory,offline_message_history}),
              listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);
            true -> ok
          end,
          listening(ServerName,TempMap,CurrentUsers,MaxUsers,Topic);
        true -> ok
      end;

    {complete_history} ->
      History = do(qlc:q([X || X <- mnesia:table(message_history)])),
      SortedHistory = lists:sort(History),
      DisplayHistory = lists:reverse(SortedHistory),
      io:format("\n[" ++ get_time() ++ "]"),
      io:format("*Following is the Chat Message History :~n"),
      lists:foreach(fun({_TableName,Time,From,Message}) ->
        {{Year,Month,Day},{Hour,Min,Sec}} = Time,
        MsgTime = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
        io:format("~p ~p: ~p~n",[MsgTime,From,Message]) end, DisplayHistory),
      listening(ServerName,UsersMap,CurrentUsers,MaxUsers,Topic)


  end.


is_expired_time(EndTime) ->
  CurrentTime = calendar:local_time(),
  if
    CurrentTime > EndTime -> true;
    true -> false
  end.

add_time(Time) ->
  calendar:gregorian_seconds_to_datetime(
    calendar:datetime_to_gregorian_seconds(calendar:local_time()) + Time*60
  ).

get_time() ->
  {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
  Time = lists:concat([Year,':',Month,':',Day,' ',Hour,':',Min,':',Sec]),
  Time.

get_offline_message_history(UserName) ->
  do(qlc:q([T || T <- mnesia:table(offline_message_history), T#offline_message_history.username =:= UserName])).

delete_offline_message_history(UserName) ->
  do(qlc:q([mnesia:delete({offline_message_history,T#offline_message_history.time}) || T <- mnesia:table(offline_message_history), T#offline_message_history.username =:= UserName])).


get_connected_clients(ServerName) ->
  global:send(ServerName,{get_connected_clients}).

send_message_to_all_clients(ServerName,Msg) ->
  global:send(ServerName,{Msg,send_message_to_all_clients}).

make_admin(ServerName, UserName) ->
  global:send(ServerName,{UserName,make_admin}).

% Mnesia Methods:
do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

reset_tables() ->
  mnesia:clear_table(message_history),
  mnesia:clear_table(offline_message_history).


insert_into_message_history(UserName, Message) ->
  Time = erlang:localtime(),
  Row = #message_history{username = UserName, time = Time, msg = Message},
  F = fun() ->
    mnesia:write(Row)
      end,
  mnesia:transaction(F).

insert_into_offline_message_history(SenderName,UserName,Message ) ->
  Time = erlang:localtime(),
  Row = #offline_message_history{sendername = SenderName, time = Time, msg = Message, username = UserName},
  F = fun() ->
    mnesia:write(Row)
      end,
  mnesia:transaction(F).

get_history(ServerName) ->
  global:send(ServerName,{complete_history}).




%% server terminal:
%  erl -sname server  => server@GGN002317
%  c(server). c(client). server:start(server@GGN002317,0,3).
%  server:get_connected_clients(server@GGN002317).
%  server:send_message_to_all_clients(server@GGN002317,"Hello Everyone").
%  server:make_admin(server@GGN002317,ansh).
%  server:get_history(server@GGN002317).


%% client terminal:
%  erl -sname client  => client@GGN002317)
% net_kernel:connect_node(server@GGN002317). c(server). c(client).
% client:start(server@GGN002317,ansh).    => ?? if another client adds with same name, so it sends fail response to ansh terminal not on senders terminal.
% client:send_message(server@GGN002317,ansh,"hi").
% client:exit(server@GGN002317,raj).
% client:send_private_message(server@GGN002317,raj,ansh,"hi Raj").
% client:get_connected_clients(server@GGN002317,ansh).
% client:update_chat_topic(server@GGN002317,ansh,"New Topic").1
% client:get_current_chat_topic(server@GGN002317,ansh).
% client:perform_admin_action(server@GGN002317,ansh,raj,mute,5).    =>  % mute/unmute/kick/makeAdmin
% client:get_admins(server@GGN002317,ansh).
% client:set_status(server@GGN002317,raj,offline).







