# systemd_notify

This Erlang library lets your app notify the systemd service manager about start-up
completion and other service status changes.

It uses the native support for Unix domain sockets in recent Erlang releases, writing
directly to the socket specified by the `$NOTIFY_SOCKET` environment variable.

See the [sd_notify](https://www.freedesktop.org/software/systemd/man/sd_notify.html)
documentation for more details on notification messages.

## Usage

```erlang
systemd_notify:send(<<"READY=1">>),
systemd_notify:send(<<"RELOADING=1">>),
systemd_notify:send(<<"STOPPING=1">>),
systemd_notify:send(<<"WATCHDOG=1">>),
```

### Watchdog

Add the `gen_server` to your app's supervision tree, giving an argument with the
refresh interval.

Erlang

```erlang
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => systemd_notify_watchdog,
                    start => {systemd_notify_watchdog, start_link, [60000]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [systemd_notify_watchdog]}],
    {ok, {SupFlags, ChildSpecs}}.
```

Elixir

```elixir
def init(arg) do
    children = [
      %{id: :systemd_notify_watchdog, start: {:systemd_notify_watchdog, :start_link, [60000]}},
    ]
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 1, max_seconds: 5)
end
```

## Building

```shell
rebar3 compile
```

## Authors

The core of this code is based on Max Lapshin's [systemd.erl](https://gist.github.com/maxlapshin/01773f0fca706acdcb4acb77d91d78bb),
reorganized and packaged by Jake Morrison.
