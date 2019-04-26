#!/bin/sh
cd tool/single_c
hname=$1."erl"

sdir=$(find ../../apps/ -name $hname)
ddir=$(find ../../_build/default/rel/game/lib/ -name $hname)
echo $sdir
echo $ddir

cp -rf $sdir $ddir

./hot_swap.sh $1

