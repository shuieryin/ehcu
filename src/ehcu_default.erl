-module(ehcu_default).

-export([init/1, do/1, format_error/1]).

-include("ehcu.hrl").

-define(PROVIDER, ehcu).
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
        {example, "rebar3 ehcu"}, % How to use the plugin
        {hooks, {[], []}}, % execute rebar command afterwards
        {opts, []},
        {short_desc, "Ehcu plugin introduction"},
        {desc, "Ehcu plugin introduction"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("This is Hot code upgrade plugin for convinience~n~nFollowing are commands available~n~n"),

    lists:foreach(
        fun(Provider) ->
            ModuleName = providers:module(Provider),
            if
                ModuleName == ?MODULE ->
                    do_nothing;
                true ->
                    case re:run(atom_to_list(ModuleName), <<"^ehcu_.*">>) of
                        {match, _Match} ->
                            io:format("============================================>~n"),
                            providers:help(Provider),
                            io:format("~n");
                        nomatch ->
                            do_nothing
                    end
            end
        end, rebar_state:providers(State)),
    file:write_file("rebar_state", io_lib:format("~tp.", [State])),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%==================================================================