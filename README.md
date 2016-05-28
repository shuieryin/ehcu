ehcu
=====

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
