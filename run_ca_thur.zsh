#!/bin/zsh

ws=t
for j in {1..15}; do
	(( job = $j + $1 * 15 ))
	echo $ws $job
	Rscript thur/2_catchment_area.r "$job"
done
