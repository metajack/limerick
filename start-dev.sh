#!/bin/sh

cd `dirname $0`
erl -pa $PWD/ebin $PWD/deps/*/ebin -name limerick@`hostname` -boot start_sasl -s limerick
