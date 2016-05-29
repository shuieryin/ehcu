-module(ehcu_stop).

-export([
    init/1,
    do/1,
    format_error/1
]).

-include("ehcu.hrl").

-define(PROVIDER, stop).
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
        {example, "rebar3 stop"}, % How to use the plugin
        {hooks, {[], []}}, % execute rebar command afterwards
        {opts, []},
        {short_desc, "stop main app server"},
        {desc, "Stop main app server"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    #ehcu_state{
        app_name = AppName,
        plugin_path = PluginPath
    } = ehcu:ehcu_state(State),

    PluginOutDir = filename:join([PluginPath, "ebin"]),
    os:cmd("erl -noshell +pc unicode -name stop_server@127.0.0.1 -setcookie " ++ AppName ++ " -s remote_control stop_server " ++ AppName ++ "@127.0.0.1 -s init stop -pa " ++ PluginOutDir),
    io:format("===> Application server should has been stopped.~n"),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%==================================================================