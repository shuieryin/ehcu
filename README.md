ehcu
=====

[![Build Status](https://travis-ci.org/shuieryin/ehcu.svg?branch=master)](https://travis-ci.org/shuieryin/ehcu)
[![Code Climate](http://img.shields.io/badge/code_climate-Erlang_21.0-brightgreen.svg)](http://www.erlang.org/downloads/21.0)

Hot code upgrade plugin for my convenience

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { ehcu, ".*", {git, "git@host:user/ehcu.git", {tag, "master"}}}
    ]}.

Then just call below to get all command usages:

    $ rebar3 ehcu
    ===> Fetching ehcu
    ===> Compiling ehcu
    <Command Usage>


Module sequence method on your app server
-----------------------------------------

Implement below method on your app server globally for ehcu plugin to generate app sequence for you.

```erlang
-spec gen_sequence(module()) -> [module()].
gen_sequence(RootSupName) ->
    RootSequences = supervisor:which_children(RootSupName),
    lists:foldl(
        fun(Spec, AccSequence) ->
            case Spec of
                {_ModuleId, _Pid, worker, [ModuleName]} ->
                    LastModuleName = case AccSequence of
                                         [] ->
                                             undefined;
                                         [LModuleName | _RestAccModuleNames] ->
                                             LModuleName
                                     end,
                    case LastModuleName =/= ModuleName of
                        true ->
                            [ModuleName | AccSequence];
                        false ->
                            AccSequence
                    end;
                {ModuleName, _Pid, supervisor, [ModuleName]} ->
                    [ModuleName, gen_sequence(ModuleName) | AccSequence]
            end
        end, [], RootSequences).
```