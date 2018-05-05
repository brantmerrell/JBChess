#!/bin/bash

# sudo apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

# R -e "install.packages("devtools")"

R -e "install.packages('jsonlite', dependencies=TRUE, repos='http://cran.rstudio.com/')"
