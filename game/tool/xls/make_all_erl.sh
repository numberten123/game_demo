#!/bin/sh
cd tool/xls
for i in `ls *.xlsx`
do
    python table2erl.py $i
done

cp -rf output/*.erl ../../apps/game/src/config/