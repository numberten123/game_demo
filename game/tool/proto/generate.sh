#!/bin/sh
cd tool/proto
filelist=`ls -1 proto/*.proto`
>output/all_pb.proto
for file in $filelist
do 
	cat $file >> output/all_pb.proto
done

./gpb/bin/protoc-erl -I. "output/all_pb.proto";

cp -rf output/*.erl ../../apps/game/src/
cp -rf output/*.hrl ../../apps/game/include/