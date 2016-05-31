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
    case connect_node(NodeAddr) of
        true ->
            timer:sleep(250),
            ChildrenSpecs = gen_server:CallMethod({global, information_server}, ModuleName),
            io:format("~p.", [ChildrenSpecs]);
        _ ->
            io:format("connection_failed.")
    end.

%%--------------------------------------------------------------------
%% @doc
%% Connect node
%%
%% @end
%%--------------------------------------------------------------------
-spec connect_node(node()) -> boolean().
connect_node(NodeAddr) ->
    case lists:member(NodeAddr, erlang:nodes()) of
        true ->
            true;
        false ->
            Result = net_kernel:connect_node(NodeAddr),
            timer:sleep(250),
            Result
    end.