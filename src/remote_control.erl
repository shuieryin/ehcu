%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% Remote controlling app server trigger build
%%%
%%% @end
%%% Created : 19. Feb 2016 3:56 PM
%%%-------------------------------------------------------------------
-module(remote_control).
-author("shuieryin").

%% API
-export([
    module_sequence/1,
    stop_server/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Retrieve module sequence
%%
%% @end
%%--------------------------------------------------------------------
-spec module_sequence(Args :: list()) -> ok.
module_sequence([NodeAddr]) ->
    exec(NodeAddr, module_sequence, call).

%%--------------------------------------------------------------------
%% @doc
%% Stop app server
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_server(Args :: list()) -> ok.
stop_server([NodeAddr]) ->
    exec(NodeAddr, stop_server, cast).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Implementation method
%%
%% @end
%%--------------------------------------------------------------------
-spec exec(node(), module(), atom()) -> ok.
exec(NodeAddr, ModuleName, CallMethod) ->
    case net_kernel:connect_node(NodeAddr) of
        true ->
            NodeAddStr = atom_to_list(NodeAddr),
            [AppNameStr | _Rest] = string:tokens(NodeAddStr, "@"),
            InfoServerName = list_to_atom(AppNameStr ++ "_information_server"),
            timer:sleep(250),
            ChildrenSpecs = gen_server:CallMethod({global, InfoServerName}, ModuleName),
            io:format("~p.", [ChildrenSpecs]);
        _ ->
            io:format("connection_failed.")
    end.