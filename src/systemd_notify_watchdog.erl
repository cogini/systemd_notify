-module(systemd_notify_watchdog).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], [Args]).

init([Interval]) ->
  erlang:send_after(Interval, self(), watchdog),
  {ok, Interval}.

handle_info(watchdog, Interval) ->
  systemd_notify:send(<<"WATCHDOG=1">>),
  erlang:send_after(Interval, self(), watchdog),
  {noreply, Interval};

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
