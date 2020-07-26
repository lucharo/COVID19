#!/usr/bin/bash

path="/Users/luischavesrodriguez/Documents/ExtratimeWork/COVID19/"
cd $path
bash getData.sh 

Rscript EDA_workhorse.R T

## cool function, it finds all file dependencies automatically
Rscript -e 'rsconnect::deployDoc("COVID_EDA.Rmd", forceUpdate = getOption("rsconnect.force.update.apps", TRUE))'

url=https://lucha6.shinyapps.io/COVID19EDA/

#open $url

