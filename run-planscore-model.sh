#!/bin/sh -ex
cd `dirname "$1"`
FILE=`basename "$1"`
Rscript --vanilla /usr/local/lib/R/run_planscore_model.R "$FILE" "$2" "$3"
