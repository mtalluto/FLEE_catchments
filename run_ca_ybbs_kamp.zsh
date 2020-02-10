#!/bin/zsh

ws=$1
for j in {1..15}; do
	(( job = $j + $2 * 15 ))
	echo $ws $job
	Rscript ybbs_kamp/2_catchment_area.r "$ws" "$job"
done
