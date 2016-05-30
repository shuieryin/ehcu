-module(ehcu_upgrade).

-export([
    init/1,
    do/1,
    format_error/1,
    test/0
]).

-include("ehcu.hrl").

-define(VERSION_DEPTH, -1).
-define(PROVIDER, hcu).
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
        {example, "rebar3 hcu"}, % How to use the plugin
        {hooks, {[], []}}, % execute rebar command afterwards
        {opts, []},
        {short_desc, "hot code upgrade for my convenience"},
        {desc, "Hot code upgrade for my convenience"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    #ehcu_state{
        app_name = AppName,
        app_vsn = OldVsn
    } = EhcuState = ehcu:ehcu_state(State),

    case appup(EhcuState) of
        no_change ->
            io:format("===> No added/deleted/modified files~n");
        NewVsn ->
            %% copy, remove, and move the src folder is because the modifications which got synchronized
            %% from intellij does not work for release upgrade and the cause remains unknown, this is to
            %% workaround the issue since no other solution is found.
            os:cmd("cp -rf src src_bak ;
                    rm -rf src ;
                    mv src_bak src"),

            %% execute release upgrade command, version number after "-u" is the upgrade-from version.
            ehcu:cmd("./config/rebar3 release relup -u " ++ OldVsn ++ " tar"),

            %% copy [APP_NAME].rel from the latest release folder to parent releases folder because rebar3
            %% only perform this action once which will cause the auto release upgrade only works for two
            %% time, there we do this manually here.
            %% rebar3 does not put the release upgrade gzip package under the latest release version folder so do it manually.
            os:cmd("rm -f _build/default/rel/" ++ AppName ++ "/releases/" ++ AppName ++ ".rel ;
                        cp _build/default/rel/" ++ AppName ++ "/releases/" ++ NewVsn ++ "/" ++ AppName ++ ".rel _build/default/rel/" ++ AppName ++ "/releases/ ;
                        mv _build/default/rel/" ++ AppName ++ "/" ++ AppName ++ "-" ++ NewVsn ++ ".tar.gz _build/default/rel/" ++ AppName ++ "/releases/" ++ NewVsn ++ "/" ++ AppName ++ ".tar.gz"),

            ehcu:cmd("./_build/default/rel/" ++ AppName ++ "/bin/" ++ AppName ++ " install " ++ NewVsn)
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%==================================================================
appup(#ehcu_state{
    app_name = AppName,
    app_vsn = OldVsn,
    project_path = ProjectPath,
    project_out_dir = ProjectOutDir
} = EhcuState) ->
    NewVsn = increase_vsn(OldVsn, ?VERSION_DEPTH, 1), %% will not modify version number in rebar.config and [app_name].app.src

    %% -------------------------get existing instructions - start-------------------------
    OldAppupPath = filename:join([ProjectPath, "ebin/", AppName ++ ".appup"]),
    ExistingInstructions =
        case file:consult(OldAppupPath) of
            {ok, [{OldVsn, [{_, SrcInstructions}], [{_, []}]}]} ->
                update_existing_instruction_version(SrcInstructions, OldVsn, NewVsn, []);
            _ ->
                []
        end,
    %% -------------------------get existing instructions - end---------------------------

    %% -------------------------get changed csv files - start---------------------------
    ListModifiedPrivFilesCommand = binary_to_list(<<"git diff --name-only HEAD~0 --diff-filter=M | grep -E 'priv/.*\.csv'">>),
    ListAddedPrivFilesCommand = binary_to_list(<<"git ls-files --others --exclude-standard | grep -E 'priv/.*\.cv'; git diff --name-only HEAD~0 --diff-filter=A | grep -E 'priv/.*\.csv'">>),
    ListDeletedPrivFilesCommand = binary_to_list(<<"git diff --name-only HEAD~0 --diff-filter=D | grep -E 'priv/.*\.csv'">>),

    ModifiedPrivFiles = string:tokens(os:cmd(ListModifiedPrivFilesCommand), "\n"),
    AddedPrivFiles = string:tokens(os:cmd(ListAddedPrivFilesCommand), "\n"),
    DeletedPrivFiles = string:tokens(os:cmd(ListDeletedPrivFilesCommand), "\n"),
    %% -------------------------get changed csv files - end-----------------------------

    %% -------------------------generate new instructions - start-------------------------
    BeamFolder = filename:join([ProjectOutDir, "ebin"]),
    ModifiedFiles = string:tokens(os:cmd("git diff --name-only HEAD~0 --diff-filter=M | grep -E 'src.*\.erl'"), "\n"),
    {ModifiedInstructions, {UpdatedModifiedPrivFiles, UpdatedAddedPrivFiles, UpdatedDeletedPrivFiles}} = generate_modified_instruction(modified, ModifiedFiles, OldVsn, NewVsn, BeamFolder, [], {ModifiedPrivFiles, AddedPrivFiles, DeletedPrivFiles}),

    ModifiedInstructionsWithModPriv = generate_instruction_for_priv(OldVsn, NewVsn, modified, BeamFolder, ModifiedInstructions, UpdatedModifiedPrivFiles),
    ModifiedInstructionsWithAddPriv = generate_instruction_for_priv(OldVsn, NewVsn, added, BeamFolder, ModifiedInstructionsWithModPriv, UpdatedAddedPrivFiles),
    UpdatedModifiedInstructions = generate_instruction_for_priv(OldVsn, NewVsn, deleted, BeamFolder, ModifiedInstructionsWithAddPriv, UpdatedDeletedPrivFiles),

    DeletedFiles = string:tokens(os:cmd("git diff --name-only HEAD~0 --diff-filter=D | grep -E 'src.*\.erl'"), "\n"),
    DeletedModifiedInstructions = generate_added_deleted_instruction(delete_module, DeletedFiles, UpdatedModifiedInstructions),

    AddedFiles = string:tokens(os:cmd("git ls-files --others --exclude-standard | grep -E 'src.*\.erl'; git diff --name-only HEAD~0 --diff-filter=A | grep -E 'src.*\.erl'"), "\n"),
    AddedDeletedModifiedInstructions = generate_added_deleted_instruction(add_module, AddedFiles, DeletedModifiedInstructions),
    %% -------------------------generate new instructions - end---------------------------

    CodeChangesInstructions = ukeymerge(2, AddedDeletedModifiedInstructions, ExistingInstructions),

    case CodeChangesInstructions of
        [] ->
            no_change;
        _ ->
            gen_appup(EhcuState, NewVsn, OldAppupPath, CodeChangesInstructions)
    end.

gen_appup(#ehcu_state{
    app_name = AppName,
    app_vsn = OldVsn,
    plugin_path = PluginPath,
    project_path = ProjectPath
} = EhcuState, NewVsn, OldAppupPath, Instructions) ->
    HcuConfigName = "hcu.config",
    ProjectConfigPath = filename:join([ProjectPath, "config"]),
    HcuConfigPath = filename:join([ProjectConfigPath, HcuConfigName]),
    case filelib:is_regular(HcuConfigPath) of
        true ->
            do_nothing;
        false ->
            io:format("===> config/hcu.config not found, create config from sample~n"),
            os:cmd("mkdir -p " ++ ProjectConfigPath ++ ";
                    cp " ++ filename:join([PluginPath, "priv", HcuConfigName]) ++ " " ++ HcuConfigPath)
    end,
    {ok, HcuConfigs} = file:consult(HcuConfigPath),
    {server_priv_dependency, ServerPrivDependencies} = lists:keyfind(server_priv_dependency, 1, HcuConfigs),
    UpdatedInstructions =
        lists:foldl(
            fun({TargetServerModName, PrivDependencyServerModNames}, AccUpdatedInstructions) ->
                case lists:keyfind(TargetServerModName, 2, Instructions) of
                    {update, TargetServerModName, {advanced, {OldVsn, NewVsn, {ModifiedFilesExtra, AddedFilesExtra, DeletedFilesExtra}}}} ->
                        if
                            ModifiedFilesExtra == [] andalso AddedFilesExtra == [] andalso DeletedFilesExtra == [] ->
                                AccUpdatedInstructions;
                            true ->
                                lists:foldl(
                                    fun(DependencyServerModName, AccAccUpdatedInstructions) ->
                                        case lists:keymember(DependencyServerModName, 2, Instructions) of
                                            false ->
                                                [{update, DependencyServerModName, {advanced, {OldVsn, NewVsn, {[], [], []}}}} | AccAccUpdatedInstructions];
                                            true ->
                                                AccAccUpdatedInstructions
                                        end
                                    end, AccUpdatedInstructions, PrivDependencyServerModNames)
                        end;
                    false ->
                        AccUpdatedInstructions
                end
            end, Instructions, ServerPrivDependencies),

    PluginOutDir = filename:join([PluginPath, "ebin"]),
    ModuleSequenceCommand = "erl -noshell +pc unicode -name module_sequence@127.0.0.1 -setcookie " ++ AppName ++ " -s remote_control module_sequence " ++ AppName ++ "@" ++ AppName ++ ".local -s init stop -pa " ++ PluginOutDir,

    ModuleSequence = str_to_term(os:cmd(ModuleSequenceCommand)),
    if
        ModuleSequence == connection_failed ->
            ErrMsg = "===> Cannot get module sequences, " ++ atom_to_list(ModuleSequence) ++ "~n",
            io:format(ErrMsg),
            throw(ErrMsg);
        true ->
            do_nothing
    end,

    {ModuleSequenceMap, _} = lists:foldl(
        fun(ModuleName, {AccModuleSequenceMap, Counter}) ->
            {
                AccModuleSequenceMap#{
                    ModuleName => Counter
                },
                Counter + 1
            }
        end, {#{}, 1}, ModuleSequence),

    FinalInstructions = lists:sort(
        fun(A, B) ->
            {ASeq, AModuleName} = module_sequence(A, ModuleSequenceMap),
            {BSeq, BModuleName} = module_sequence(B, ModuleSequenceMap),
            case ASeq =:= BSeq of
                true ->
                    AModuleName < BModuleName;
                false ->
                    ASeq < BSeq
            end
        end, UpdatedInstructions),

    update_version(EhcuState, NewVsn),
    AppupContent = {NewVsn,
        [{OldVsn, FinalInstructions}],
        [{OldVsn, []}]},
    os:cmd("mkdir -p ebin"),
    AppupContentBin = io_lib:format("~tp.", [AppupContent]),
    file:write_file(OldAppupPath, AppupContentBin),
    file:write_file("config/" ++ AppName ++ ".appup", AppupContentBin),
    NewVsn.

increase_vsn(SourceVersion, VersionDepth, Increment) ->
    VsnList = string:tokens(SourceVersion, "."),
    ReverseDepth = case VersionDepth of
                       -1 ->
                           VersionDepth;
                       _Else ->
                           length(VsnList) - VersionDepth + 1
                   end,
    string:join(increase_vsn(lists:reverse(VsnList), ReverseDepth, Increment, 1, []), ".").

increase_vsn([CurDepthVersionNumStr | Tail], VersionDepth, Increment, CurDepth, AccVersion) ->
    IsFound = case VersionDepth of
                  -1 ->
                      case re:run(CurDepthVersionNumStr, "^[0-9]*$") of
                          {match, _Match} ->
                              true;
                          nomatch ->
                              false
                      end;
                  _Else ->
                      CurDepth =:= VersionDepth
              end,

    case IsFound of
        true ->
            UpdatedVersionNum = integer_to_list(list_to_integer(CurDepthVersionNumStr) + Increment),
            increase_vsn([], VersionDepth, Increment, CurDepth, lists:reverse(AccVersion) ++ [UpdatedVersionNum | Tail]);
        false ->
            increase_vsn(Tail, VersionDepth, Increment, CurDepth + 1, [CurDepthVersionNumStr | AccVersion])
    end;
increase_vsn([], _, _, _, AccVersion) ->
    lists:reverse(AccVersion).

update_version(#ehcu_state{
    app_name = AppName,
    src_file_path = SrcFilePath,
    src_config = {application, RawAppName, ConfigList},
    rebar_config_path = RebarConfigPath,
    rebar_config = RebarConfigs
}, TargetVsn) ->
    UpdatedConfigList = lists:keyreplace(vsn, 1, ConfigList, {vsn, TargetVsn}),

    NewSrcConfig = {application, RawAppName, UpdatedConfigList},
    file:write_file(SrcFilePath, io_lib:format("~tp.", [NewSrcConfig])),

    {relx, RelxConfigs} = lists:keyfind(relx, 1, RebarConfigs),
    AppNameAtom = list_to_atom(AppName),
    {release, {AppNameAtom, _OldVsn}, ReleaseConfigs} = lists:keyfind(release, 1, RelxConfigs),
    NewReleaseConfig = {release, {AppNameAtom, TargetVsn}, ReleaseConfigs},
    NewRelxConfig = {relx, lists:keyreplace(release, 1, RelxConfigs, NewReleaseConfig)},

    file:delete(RebarConfigPath),
    lists:foreach(
        fun({ConfigName, _ConfigContents} = Config) ->
            ConfigBin =
                case ConfigName of
                    relx ->
                        io_lib:format("~tp.~n~n", [NewRelxConfig]);
                    _Other ->
                        io_lib:format("~tp.~n~n", [Config])
                end,
            file:write_file(RebarConfigPath, ConfigBin, [append])
        end, RebarConfigs).

update_existing_instruction_version([{update, ModName, {advanced, {_, _, []}}} | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [{update, ModName, {advanced, {OldVsn, NewVsn, []}}} | AccResult]);
update_existing_instruction_version([Other | Tail], OldVsn, NewVsn, AccResult) ->
    update_existing_instruction_version(Tail, OldVsn, NewVsn, [Other | AccResult]);
update_existing_instruction_version([], _, _, AccResult) ->
    AccResult.

generate_instruction_for_priv(OldVsn, NewVsn, FileType, BeamFolder, AccInstructions, PrivFilePahts) ->
    lists:foldl(
        fun(FilePath, AccModifiedInstructions) ->
            [_, CurModNameStr | _] = filename:split(FilePath),
            CurModName = list_to_atom(CurModNameStr),
            case lists:keyfind(CurModName, 2, AccModifiedInstructions) of
                false ->
                    BeamFilePath = filename:join(BeamFolder, CurModNameStr ++ ".beam"),
                    case filelib:is_regular(BeamFilePath) of
                        true ->
                            NewExtra =
                                case FileType of
                                    modified ->
                                        {[FilePath], [], []};
                                    added ->
                                        {[], [FilePath], []};
                                    deleted ->
                                        {[], [], [list_to_atom(filename:basename(filename:rootname(FilePath)))]}
                                end,
                            NewInstruction = {update, CurModName, {advanced, {OldVsn, NewVsn, NewExtra}}},
                            [NewInstruction | AccModifiedInstructions];
                        false ->
                            throw("[" ++ FilePath ++ "] cannot be processed because module [" ++ CurModNameStr ++ "] does not exist.")
                    end;
                {update, CurModName, {advanced, {OldVsn, NewVsn, CurExtra}}} ->
                    {AccModPaths, AccAddPaths, AccDelPaths} = CurExtra,
                    UpdatedCurExtra
                        = case FileType of
                              modified ->
                                  {[FilePath | AccModPaths], AccAddPaths, AccDelPaths};
                              added ->
                                  {AccModPaths, [FilePath | AccAddPaths], AccDelPaths};
                              deleted ->
                                  {AccModPaths, AccAddPaths, [list_to_atom(filename:basename(filename:rootname(FilePath))) | AccDelPaths]}
                          end,
                    UpdatedInstruction = {update, CurModName, {advanced, {OldVsn, NewVsn, UpdatedCurExtra}}},
                    lists:keyreplace(CurModName, 2, AccModifiedInstructions, UpdatedInstruction)
            end
        end, AccInstructions, PrivFilePahts).

generate_added_deleted_instruction(Status, [SrcFilePath | Tail], AccInstructions) when add_module == Status orelse delete_module == Status ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    Instruction = {Status, ModName},
    generate_added_deleted_instruction(Status, Tail, [Instruction | AccInstructions]);
generate_added_deleted_instruction(_, [], InstructionList) ->
    InstructionList.

generate_modified_instruction(modified, [SrcFilePath | Tail], OldVsn, NewVsn, BeamFolder, AccInstructions, PrivChangedFiles) ->
    ModNameStr = filename:rootname(filename:basename(SrcFilePath)),
    ModName = list_to_atom(ModNameStr),
    ModFileName = ModNameStr ++ ".beam",
    BeamFilePath = filename:join(BeamFolder, ModFileName),
    {Instruction, UpdatedPrivChangedFiles} =
        case file:read_file(BeamFilePath) of
            {ok, Beam} ->
                {ok, {_, [{exports, Exports}, {attributes, Attributes}]}} = beam_lib:chunks(Beam, [exports, attributes]),
                Behaviour = proplists:get_value(behaviour, Attributes, []),
                case lists:member(supervisor, Behaviour) of
                    true ->
                        {
                            {update, ModName, supervisor},
                            PrivChangedFiles
                        };
                    _ ->
                        case lists:member({code_change, 3}, Exports) orelse lists:member({code_change, 4}, Exports) of
                            true ->
                                {AccModifiedFiles, AccAddedFiles, AccDeletedFiles} = PrivChangedFiles,

                                {ModifiedFilesExtra, RestModifiedFiles} = filter_priv_files(AccModifiedFiles, ModNameStr),
                                {AddedFilesExtra, RestAddedFiles} = filter_priv_files(AccAddedFiles, ModNameStr),
                                {DeletedFilesExtra, RestDeletedFiles} = filter_priv_files(AccDeletedFiles, ModNameStr),

                                {
                                    {update, ModName, {advanced, {OldVsn, NewVsn, {ModifiedFilesExtra, AddedFilesExtra, DeletedFilesExtra}}}},
                                    {RestModifiedFiles, RestAddedFiles, RestDeletedFiles}
                                };
                            _ ->
                                {
                                    {load_module, ModName},
                                    PrivChangedFiles
                                }
                        end
                end;
            _ ->
                io:format("Could not read ~s\n", [BeamFilePath])
        end,
    generate_modified_instruction(modified, Tail, OldVsn, NewVsn, BeamFolder, [Instruction | AccInstructions], UpdatedPrivChangedFiles);
generate_modified_instruction(_, [], _, _, _, InstructionList, PrivChangedFiles) ->
    {InstructionList, PrivChangedFiles}.

filter_priv_files(FilePaths, ModNameStr) ->
    lists:foldl(
        fun(FilePath, {AccFilePaths, AccOriFilePaths}) ->
            case re:run(FilePath, "^priv/" ++ ModNameStr ++ ".*") of
                {match, _Captured} ->
                    {
                        [FilePath | AccFilePaths],
                        lists:delete(FilePath, AccOriFilePaths)
                    };
                nomatch ->
                    {AccFilePaths, AccOriFilePaths}
            end
        end, {[], FilePaths}, FilePaths).

ukeymerge(ElemPos, SrcList, MergeList) ->
    MergeMap = proplist_to_map(ElemPos, MergeList, #{}),
    FinalMap = proplist_to_map(ElemPos, SrcList, MergeMap),
    maps:values(FinalMap).

str_to_term(SrcStr) ->
    {ok, Tokens, _EndLocation} = erl_scan:string(SrcStr),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

module_sequence(Instruction, ModuleSequenceMap) ->
    case Instruction of
        {add_module, ModuleName} -> {-1, ModuleName};
        {load_module, ModuleName} -> {0, ModuleName};
        {update, ModuleName, _} ->
            {
                maps:get(ModuleName, ModuleSequenceMap, 10000),
                ModuleName
            };
        {delete_module, ModuleName} -> {10001, ModuleName}
    end.

proplist_to_map(ElemPos, [Value | Tail], AccMap) ->
    Key = erlang:element(ElemPos, Value),
    proplist_to_map(ElemPos, Tail, AccMap#{Key => Value});
proplist_to_map(_, [], AccMap) ->
    AccMap.

test() ->
    increase_vsn("0.1", 2, 1).