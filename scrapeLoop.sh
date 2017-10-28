#!/bin/bash

while getopts f:l: o
do	case "$o" in 
	f)	first="$OPTARG";;
	l)	last="$OPTARG" 
	esac
done

Rscript scrapeLoop.R $((first)),$((last))

#for n in $(seq 100 199)
#do
#	first=$(($n*1000+1))
#	last=$(($n*1000+1000))
#	Rscript scrapeLoop.R $first,$last
#	aws s3 mv data s3://jbchess/data --recursive
#done

#aws s3 ls s3://jbchess/data --recursive
