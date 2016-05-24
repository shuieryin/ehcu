-module(ehcu_prv).

-export([init/1, do/1, format_error/1]).

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
        {opts, [                 % list of options understood by the plugin
            {deps, $d, "deps", undefined, "also run against dependencies"}
        ]},
        {short_desc, "Hot code upgrade plugin for convinience"},
        {desc, "Hot code upgrade plugin for convinience"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case discovery_type(State) of
               project -> rebar_state:project_apps(State);
               deps -> rebar_state:project_apps(State) ++ lists:usort(rebar_state:all_deps(State))
           end,
    lists:foreach(fun check_todo_app/1, Apps),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


%==================================================================
check_todo_app(App) ->
    Path = filename:join(rebar_app_info:dir(App), "src"),
    Mods = find_source_files(Path),
    case lists:foldl(fun check_todo_mod/2, [], Mods) of
        [] -> ok;
        Instances -> display_todos(rebar_app_info:name(App), Instances)
    end.

find_source_files(Path) ->
    [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.erl", Path)].

check_todo_mod(ModPath, Matches) ->
    {ok, Bin} = file:read_file(ModPath),
    case find_todo_lines(Bin) of
        [] -> Matches;
        Lines -> [{ModPath, Lines} | Matches]
    end.

find_todo_lines(File) ->
    case re:run(File, "%+.*(TODO:.*)", [{capture, all_but_first, binary}, global, caseless]) of
        {match, DeepBins} -> lists:flatten(DeepBins);
        nomatch -> []
    end.

display_todos(_, []) -> ok;
display_todos(App, FileMatches) ->
    io:format("Application ~s~n", [App]),
    [begin
         io:format("\t~s~n", [Mod]),
         [io:format("\t  ~s~n", [TODO]) || TODO <- TODOs]
     end || {Mod, TODOs} <- FileMatches],
    ok.

discovery_type(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(deps, Args) of
        undefined -> project;
        _ -> deps
    end.