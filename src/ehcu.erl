-module(ehcu).

-export([
    init/1,
    ehcu_state/1,
    retrieve_n_break/2,
    retrieve_n_break_r/2,
    format_os_ouput/1,
    cmd/1
]).

-include("ehcu.hrl").

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    PluginInfo = lists:keyfind(atom_to_binary(?PLUGIN_NAME, utf8), 2, rebar_state:all_plugin_deps(State)),
    PluginPath = rebar_app_info:dir(PluginInfo),
    {ok, FileList} = file:list_dir(filename:append(PluginPath, "ebin")),

    FinalState = lists:foldl(
        fun(FileName, AccState) ->
            case re:run(FileName, <<"^(ehcu_.*)\.beam">>, [global, {capture, all_but_first, list}]) of
                {match, [[ModuleNameStr]]} ->
                    ModuleName = list_to_atom(ModuleNameStr),
                    {ok, UpdatedAccState} = ModuleName:init(AccState),
                    UpdatedAccState;
                nomatch ->
                    AccState
            end
        end, State, FileList),

    {ok, FinalState}.

%%--------------------------------------------------------------------
%% @doc
%% Generate ehcu state.
%%
%% @end
%%--------------------------------------------------------------------
-spec ehcu_state(rebar_state:t()) -> #ehcu_state{}.
ehcu_state(State) ->
    ProjectPath = rebar_state:dir(State),
    ProjectSrcPath = filename:append(ProjectPath, "src"),

    {ok, FileList} = file:list_dir(ProjectSrcPath),
    SrcFileName = retrieve_n_break(
        fun(FileName) ->
            case re:run(FileName, <<".*\.app\.src">>) of
                {match, _Match} ->
                    true;
                nomatch ->
                    false
            end
        end, FileList),

    SrcFilePath = filename:append(ProjectSrcPath, SrcFileName),

    {ok, [{application, RawAppName, ConfigList} = SrcConfig | _Rest]} = file:consult(SrcFilePath),
    {vsn, Vsn} = lists:keyfind(vsn, 1, ConfigList),
    PluginInfo = lists:keyfind(atom_to_binary(?PLUGIN_NAME, utf8), 2, rebar_state:all_plugin_deps(State)),

    RebarConfigPath = filename:append(ProjectPath, "rebar.config"),
    {ok, RebarConfig} = file:consult(RebarConfigPath),

    AppInfo = lists:keyfind(atom_to_binary(RawAppName, utf8), 2, rebar_state:project_apps(State)),

    #ehcu_state{
        app_name = atom_to_list(RawAppName),
        app_vsn = Vsn,
        plugin_path = rebar_app_info:dir(PluginInfo),
        project_path = ProjectPath,
        src_config = SrcConfig,
        src_file_path = SrcFilePath,
        rebar_config_path = RebarConfigPath,
        rebar_config = RebarConfig,
        project_out_dir = rebar_app_info:out_dir(AppInfo)
    }.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve value from List and return the match value once found by
%% omitting rest elements from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec retrieve_n_break(Func, List) -> Elem | undefined when
    Elem :: term(),
    List :: list(),
    Func :: fun((Elem) -> boolean()).
retrieve_n_break(Func, [H | T]) ->
    case Func(H) of
        true ->
            H;
        false ->
            retrieve_n_break(Func, T)
    end;
retrieve_n_break(Func, []) when is_function(Func, 1) -> undefined.

%%--------------------------------------------------------------------
%% @doc
%% Retrieve value from List and return the match value once found by
%% omitting rest elements from list.
%%
%% @end
%%--------------------------------------------------------------------
-spec retrieve_n_break_r(Func, List) -> Elem | undefined when
    Elem :: term(),
    List :: list(),
    Func :: fun((Elem) -> boolean()).
retrieve_n_break_r(Func, List) ->
    retrieve_n_break(Func, lists:reverse(List)).


%%--------------------------------------------------------------------
%% @doc
%% Format output from os:cmd/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_os_ouput(string()) -> string().
format_os_ouput(RawOutput) ->
    re:replace(RawOutput, "\n", "~n", [global, {return, list}]).

%%--------------------------------------------------------------------
%% @doc
%% Execute command and print output in realtime.
%%
%% @end
%%--------------------------------------------------------------------
-spec cmd(string()) -> ok.
cmd(CmdStr) ->
    OutputNode = erlang:open_port({spawn, CmdStr},
        [stderr_to_stdout, in, exit_status,
            binary, stream, {line, 255}]),

    cmd_receive(OutputNode).

%%--------------------------------------------------------------------
%% @doc
%% Receive func for cmd/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec cmd_receive(node()) -> ok.
cmd_receive(OutputNode) ->
    receive
        {OutputNode, {data, {_LineType, OutputBin}}} ->
            io:format(<<"~n", OutputBin/binary>>),
            cmd_receive(OutputNode);
        {OutputNode, {exit_status, ExitStatus}} ->
            case ExitStatus of
                0 ->
                    io:format("\e[0m~n");
                _Else ->
                    throw(cmd_error)
            end
    end.