#!/bin/bash

rm -rf ppss_dir
rm -f /tmp/tg$1_*
rm -f /tmp/tgtasks_$1

count=0
for (( j=0; j<=$2-1; j++ ))
do
	echo "$1_$2_$j" >> /tmp/tgtasks_$1
done

ppss -f /tmp/tgtasks_$1 -c './src/newSortingAlgParSimple "$ITEM" > /tmp/tg"$ITEM"'

cat /tmp/tg$1_* | gsort --parallel $2 -S 12G > /tmp/tg$1_sorted
./src/getSolutionFromSortedFile < /tmp/tg$1_sorted +RTS -K1000M -RTS

rm -rf ppss_dir