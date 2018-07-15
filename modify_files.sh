#!/bin/bash
while IFS='' read -r line || [[ -n "$line" ]]; do
    echo "$line"
    aws s3 cp "s3://jbchess/data/$line" "data/$line"
    Rscript modify_files.R "data/$line"
    aws s3 mv "data/$line" "s3://jbchess/data/$line"

done < modify_files.txt
