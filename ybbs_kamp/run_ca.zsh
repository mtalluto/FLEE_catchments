#!/bin/zsh

for ws in (y k); do
	for job in {1..12}; do
		Rscript ybbs_kamp/2_catchment_area.r "$ws" "$job" &
		# we sleep here because the script startup takes a LOT of memory, but declines
		# once it gets going
		sleep 300
	done
	wait
	Rscript ybbs_kamp/2_catchment_area.r "$ws" -1
done
