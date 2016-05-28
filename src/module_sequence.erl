%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 19. Feb 2016 3:56 PM
%%%-------------------------------------------------------------------
-module(module_sequence).
-author("shuieryin").

%% API
-export([
    exec/1
]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Comment starts here
%%
%% @end
%%--------------------------------------------------------------------
exec([NodeAddr]) ->
    case net_kernel:connect_node(NodeAddr) of
        true ->
            timer:sleep(250),
            ChildrenSpecs = gen_server:call({global, information_server}, ?MODULE),
            io:format("~p.", [ChildrenSpecs]);
        _ ->
            io:format("connection_failed.")
    end.

%%%===================================================================
%%% Internal functions (N/A)
%%%===================================================================