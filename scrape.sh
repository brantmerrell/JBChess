#!/bin/bash
# obtain list of previously stored data to avoid duplication
aws s3 ls s3://jbchess/data --recursive > temp.txt

# iterate through a range of numbers
for n in $(seq 1000 1001)
do

  # adjust both numbers to the proper order of magnitude
	first=$(($n*1000+1))
	last=$(($n*1000+1000))
	# record the beginning & ending range of numbers; include a timestamp
	echo start $first $last $(date)
	
	# pass the range of numbers to the scrape.chess.com function
	Rscript R/scrape.chess.com.R $first $last
	
	# move the acquired data to the bucket
	aws s3 mv data s3://jbchess/data --recursive
done
