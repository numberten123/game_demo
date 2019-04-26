#!/bin/sh
cd tool/xls
name=${1%%.*}
python table2erl.py $1
cp -rf ./output/*$name*.erl ../../apps/game/src/config/
