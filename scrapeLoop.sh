#!/bin/bash

while getopts f:l: o
do	case "$o" in 
	f)	first="$OPTARG";;
	l)	last="$OPTARG" 
	esac
done

Rscript scrapeLoop.R $((first)),$((last))

#for n in $(seq $((first)) $((last)));
#	do Rscript test.R $((n));
#done


