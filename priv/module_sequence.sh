#!/bin/bash

MOD_SEQ_BEAM=$0
APP_NAME=$1

cp MOD_SEQ_BEAM ./
MOD_SEQ=$(erl -noshell +pc unicode -name module_sequnce@127.0.0.1 -setcookie ${APP_NAME} -s module_sequence exec -s init stop)
rm -f module_sequence.beam

# The last dot is needed for parsing result to term in erlang!!
echo "$MOD_SEQ".