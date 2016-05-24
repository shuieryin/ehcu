ehcu
=====

Hot code upgrade plugin for convinience

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { ehcu, ".*", {git, "git@host:user/ehcu.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 ehcu
    ===> Fetching ehcu
    ===> Compiling ehcu
    <Plugin Output>
