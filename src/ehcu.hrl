%%%-------------------------------------------------------------------
%%% @author shuieryin
%%% @copyright (C) 2016, Shuieryin
%%% @doc
%%%
%%% @end
%%% Created : 25. May 2016 12:55 PM
%%%-------------------------------------------------------------------
-author("shuieryin").

-define(PLUGIN_NAME, ehcu).
-define(STATE_NAME, ehcu_state).

-record(ehcu_state, {
    app_name :: string(),
    app_vsn :: string(),
    plugin_path :: file:filename(),
    project_path :: file:filename(),
    src_config :: tuple(),
    src_file_path :: file:filename(),
    rebar_config_path :: file:filename(),
    rebar_config :: [tuple()]
}).