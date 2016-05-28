-module(ehcu_reset).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("ehcu.hrl").

-define(PROVIDER, reset).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 reset"}, % How to use the plugin
        {hooks, {[], []}}, % execute rebar command afterwards
        {opts, []},
        {short_desc, "reset repository from github"},
        {desc, "Reset repository from github"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("===> Start reset...~n"),
    Output = os:cmd("git fetch --all && git reset --hard origin/master"),
    io:format(ehcu:format_os_ouput(Output)),
    io:format("===> Reset done.~n"),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%==================================================================