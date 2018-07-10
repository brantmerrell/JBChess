#!/bin/bash

aws s3 ls s3://jbchess/data --recursive > temp.txt

for n in $(seq 200 1000)
do
	first=$(($n*1000+1))
	last=$(($n*1000+1000))
	echo start $first $last $(date)
	Rscript R/scrape.chess.com.R $first $last
	aws s3 mv data s3://jbchess/data --recursive
done

