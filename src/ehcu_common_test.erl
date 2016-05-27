-module(ehcu_common_test).

-export([init/1, do/1, format_error/1]).

-include("ehcu.hrl").

-define(PROVIDER, cmt).
-define(DEPS, [app_discovery, ct]).

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
        {example, "rebar3 cmt"}, % How to use the plugin
        {hooks, {[cover], []}}, % execute rebar command afterwards
        {opts, []},
        {short_desc, "run common test and clean up"},
        {desc, "run common test and clean up"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    #ehcu_state{
        rebar_config = RebarConfig
    } = ehcu:ehcu_state(State),

    case lists:keyfind(cover_enabled, 1, RebarConfig) of
        {cover_enabled, true} ->
            do_nothing;
        _CoverDisabled ->
            io:format("===> Please define \"{cover_enabled, true}\" in rebar.config~n")
    end,


    io:format("===> Common test done.~n"),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%==================================================================