# systemd_notify

This Erlang library lets your app notify the systemd service manager about start-up
completion and other service status changes.

It uses the native support for Unix domain sockets in recent Erlang releases, writing
directly to the socket specified by the `$NOTIFY_SOCKET` environment variable.

See the [sd_notify](https://www.freedesktop.org/software/systemd/man/sd_notify.html)
documentation for more details on notification messages.

## Usage

    [Unit]
    Description=Foo service
    After=local-fs.target network.target

    [Service]
    Type=notify
    User=foo
    Group=foo
    WorkingDirectory=/srv/foo/current
    ExecStart=/srv/foo/current/bin/foo foreground
    ExecStop=/srv/foo/current/bin/foo stop
    Environment=LANG=en_US.UTF-8
    Environment=MIX_ENV=prod
    Environment=RELEASE_MUTABLE_DIR=/run/foo
    Environment=PORT=4000
    # systemd ignores /etc/security/limits
    LimitNOFILE=65535
    UMask=0027
    SyslogIdentifier=foo
    Restart=always
    RestartSec=5
    # PermissionsStartOnly=true
    RuntimeDirectory=foo
    RuntimeDirectoryMode=750
    RuntimeDirectoryPreserve=no
    ConfigurationDirectory=foo
    ConfigurationDirectoryMode=750
    LogsDirectory=foo
    LogsDirectoryMode=750
    StateDirectory=foo
    StateDirectoryMode=750
    CacheDirectory=foo
    CacheDirectoryMode=750
    # KillMode=process # default control-group
    # TimeoutSec=10
    # RemainAfterExit=no
    # https://elixirforum.com/t/distillery-node-is-not-running-and-non-zero-exit-code/3834
    # SuccessExitStatus=143

    [Install]
    WantedBy=multi-user.target

### Service type

https://www.freedesktop.org/software/systemd/man/systemd.service.html#Type=

#### `Type=simple`, `Type=exec`

It is generally recommended to use `Type=simple` for long-running services
whenever possible, as it is the simplest and fastest option.

`Type=simple` allows systemd to continue starting other units as soon
as the `fork()` command completes. With `Type=exec`, systemd waits until
the `exec()` command completes successfully, i.e. checking that your service
actually started up.

    [Service]
    Type=simple
    User=foo
    Group=foo
    WorkingDirectory=/srv/foo
    ExecStart=/srv/foo/bin/foo foreground
    Restart=on-failure
    RestartSec=5
    Environment=PORT=8080
    Environment=LANG=en_US.UTF-8
    SyslogIdentifier=foo
    RemainAfterExit=no

#### `Type=notify`

Using systemd dependency tracking:

Behavior of `notify` is similar to `exec`; however, it is expected that the service
sends a notification message when it has finished starting up.

If this option is used, NotifyAccess=
(see below) should be set to open access to the notification socket provided by
systemd. If `NotifyAccess=` is missing or set to none, it will be forcibly set to
main. Note that currently `Type=notify` will not work if used in combination with
PrivateNetwork=yes.

    [Service]
    Type=notify
    NotifyAccess=main
    User=foo
    Group=foo
    WorkingDirectory=/srv/foo
    ExecStart=/srv/foo/bin/foo foreground
    Restart=on-failure
    RestartSec=5
    Environment=PORT=8080
    Environment=LANG=en_US.UTF-8
    SyslogIdentifier=foo
    RemainAfterExit=no

`READY=1` tells the service manager that service startup is finished, or the
service finished loading its configuration. Send it at the end of your
application after everyting has started. systemd will proceed with starting
follow-up units after this notification message has been sent.

```erlang
systemd_notify:send(<<"STOPPING=1">>),
```

#### Type=forking

If set to forking, it is expected that the process configured with `ExecStart=`
will call `fork()` as part of its start-up. The parent process is expected to
exit when start-up is complete and all communication channels are set up. The
child continues to run as the main service process, and the service manager
will consider the unit started when the parent process exits. This is the
behavior of traditional UNIX services. If this setting is used, it is
recommended to also use the `PIDFile=` option, so that systemd can reliably
identify the main process of the service. systemd will proceed with starting
follow-up units as soon as the parent process exits.

Elixir Distillery 2.0 supports a `PIDFILE` environment var, which tells the
startup scripts to write the pid to to the specified file.

    [Service]
    Type=forking
    User=foo
    Group=foo
    WorkingDirectory=/srv/foo
    ExecStart=/srv/foo/bin/foo start
    ExecStop=/srv/foo/bin/foo stop
    PIDFile=/run/foo.pid
    Restart=on-failure
    RestartSec=5
    Environment=PORT=8080
    Environment=LANG=en_US.UTF-8
    Environment=PIDFILE=/run/foo.pid
    SyslogIdentifier=foo
    RemainAfterExit=no

Erlang

https://www.rebar3.org/docs/releases#section-hooks

```erlang
{extended_start_script_hooks, [
  ...
  {post_start, [
    {pid, "/run/foo.pid"},
  ]},
  ...
]}.
```

### STOPPING=1

Tells the service manager that the service is beginning its shutdown. This is
useful to allow the service manager to track the service's internal state, and
present it to the user.

Put it in your application `stop/1` callback.

```erlang
-spec stop(State) -> no_return() when
      State :: term().
stop(_State) ->
    systemd_notify:send(<<"STOPPING=1">>),
    ok.
```

### STATUS=...

Passes a single-line UTF-8 status string back to the service manager that
describes the service state. This is free-form and can be used for various
purposes: general state feedback, fsck-like programs could pass completion
percentages and failing programs could pass a human-readable error message.

Example:

```erlang
systemd_notify:send(<<"STATUS=Completed 66% of file system check`">>),
```

### MAINPID=...

TODO: not ncorrect, use pid handling in startup scripts instead

The main process ID (PID) of the service, in case the service manager did not
fork off the process itself. Example: "`MAINPID=4711`".

Put this at the beginning of your application startup.

```erlang
systemd_notify:send(list_to_binary(io_lib:format("MAINPID=~d", [os:getpid()]))),
```

```elixir
:systemd_notify.send("MAINPID=#{:os.getpid()}")
```

### Watchdog

Add the `gen_server` to your app's supervision tree.

Erlang

```erlang
init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => systemd_notify_watchdog,
                    start => {systemd_notify_watchdog, start_link, []},
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
      %{id: :systemd_notify_watchdog, start: {:systemd_notify_watchdog, :start_link, []}},
    ]
    Supervisor.init(children, strategy: :one_for_one, max_restarts: 1, max_seconds: 5)
end
```

    [Service]
    Type=notify
    NotifyAccess=main
    User=foo
    Group=foo
    WorkingDirectory=/srv/foo
    ExecStart=/srv/foo/bin/foo foreground
    Restart=on-failure
    RestartSec=5
    Environment=PORT=8080
    Environment=LANG=en_US.UTF-8
    SyslogIdentifier=foo
    RemainAfterExit=no
    WatchdogSec=120

`WatchdogSec=`

https://www.freedesktop.org/software/systemd/man/systemd.service.html#WatchdogSec=

Configures the watchdog timeout for a service. The watchdog is activated when
the start-up is completed. The service must call `systemd_notify:send/1` regularly with
"`WATCHDOG=1`" (i.e. the "keep-alive ping").

If the time between two such calls is larger than the configured time, then the
service is placed in a failed state and it will be terminated with `SIGABRT`
(or the signal specified by `WatchdogSignal=`). By setting `Restart=` to
`on-failure`, `on-watchdog`, `on-abnormal` or `always`, the service will be
automatically restarted.

The time configured here will be passed to the executed service process in the
`WATCHDOG_USEC=` environment variable. This allows daemons to automatically enable the
keep-alive pinging logic if watchdog support is enabled for the service. If
this option is used, `NotifyAccess=` should be set to open access to
the notification socket provided by systemd.

If `NotifyAccess=` is not set, it will be implicitly set to `main`. Defaults to 0,
which disables this feature. The service can check whether the service manager
expects watchdog keep-alive
notifications. See sd_watchdog_enabled(3) for details. sd_event_set_watchdog(3)
may be used to enable automatic watchdog notification support.

## Building

```shell
rebar3 compile
```

## Authors

The core of this code is based on Max Lapshin's [systemd.erl](https://gist.github.com/maxlapshin/01773f0fca706acdcb4acb77d91d78bb),
reorganized and packaged by Jake Morrison.
