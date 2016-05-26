#!/bin/bash

mkdir -p doc
cp ./config/overview.edoc ./doc/

sed -i.bak "s/{app_name}/$1/1" ./doc/overview.edoc
sed -i.bak "s/{version}/$2/1" ./doc/overview.edoc
rm -f ./doc/overview.edoc.bak

./config/rebar3 edoc