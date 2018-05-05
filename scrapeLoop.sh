#!/bin/bash

aws s3 ls s3://jbchess/data --recursive > temp.txt

for n in $(seq 100 199)
do
	first=$(($n*1000+1))
	last=$(($n*1000+1000))
	echo $first,$last
	Rscript R/scrape.chess.com.R $first $last
	aws s3 mv data s3://jbchess/data --recursive
done

