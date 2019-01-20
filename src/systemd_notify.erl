-module(systemd_notify).

%% API exports
-export([send/1]).

%%====================================================================
%% API functions
%%====================================================================

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
