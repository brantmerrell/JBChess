#!/bin/bash

while getopts f:l: o
do	case "$o" in 
	f)	first="$OPTARG";;
	l)	last="$OPTARG" 
	esac
done

Rscript scrapeLoop.R $((first)),$((last))

#aws s3 sync data s3://jbchess/data

aws s3 mv data s3://jbchess/data --recursive

aws s3 ls s3://jbchess/data --recursive
#for n in $(seq $((first)) $((last)));
#	do Rscript test.R $((n));
#done


