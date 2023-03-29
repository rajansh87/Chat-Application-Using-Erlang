# Chat-Application-Using-Erlang
Chat Application

Commands:

%% server terminal:
%  erl -sname server  => server@GGN002317
%  c(server). c(client). server:start(server@GGN002317,0,2).
%  server:get_history(server@GGN002317).
%  server:get_connected_clients(server@GGN002317).
%  server:send_message_to_all_clients(server@GGN002317,"Hello Everyone").
%  server:make_admin(server@GGN002317,ansh).


%% client terminal:
%  erl -sname client  => client@GGN002317)
% net_kernel:connect_node(server@GGN002317). c(server). c(client).
% client:start(server@GGN002317,ansh).    => ?? if another client adds with same name, so it sends fail response to ansh terminal not on senders terminal.
% client:send_message(server@GGN002317,ansh,"hi").
% client:exit(server@GGN002317,raj).
% client:send_private_message(server@GGN002317,raj,ansh,"hi Raj").
% client:get_connected_clients(server@GGN002317,ansh).
% client:update_chat_topic(server@GGN002317,ansh,"New Topic").
% client:get_current_chat_topic(server@GGN002317,ansh).
% client:perform_admin_action(server@GGN002317,ansh,raj,mute,5).    =>  % mute/unmute/kick/makeAdmin
% client:get_admins(server@GGN002317,ansh).
% client:set_status(server@GGN002317,raj,offline).
