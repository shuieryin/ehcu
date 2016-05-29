ehcu
=====

[![Build Status](https://travis-ci.org/shuieryin/ehcu.svg?branch=master)](https://travis-ci.org/shuieryin/ehcu)
[![Code Climate](http://img.shields.io/badge/code_climate-Erlang_18.3-brightgreen.svg)](http://www.erlang.org/downloads/18.3)

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
