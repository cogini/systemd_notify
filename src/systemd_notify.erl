%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019, Cogini
%%% @doc Send message to notify systemd service manager about service status.
%%%
%%% @author Jake Morrison
%%% @end
%%%-----------------------------------------------------------------------------
-module(systemd_notify).

%% API exports
-export([send/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Send message to notify socket.
%% @end
%%------------------------------------------------------------------------------
-spec send(Message :: binary()) -> ok | {error, not_configured | not_owner | inet:posix()}. 
send(Message) ->
  case os:getenv("NOTIFY_SOCKET") of
    false ->
      {error, not_configured};
    Path ->
      case gen_udp:open(0, [local]) of
        {error, SocketError} ->
          {error, SocketError};
        {ok, Socket} ->
          Result = gen_udp:send(Socket, {local, Path}, 0, Message),
          gen_udp:close(Socket),
          Result
      end
  end.
