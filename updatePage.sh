#!/bin/bash

path="/Users/luischavesrodriguez/Documents/ExtratimeWork/COVIDEDA/"
cd $path
bash getData.sh 

# Workhorses - order matters
for script in datacleaning tables_for_all treemap curves events_per_day barplots raceplots maps
do
	echo 'Running '${script}'...'
	Rscript workHorse/$script.R T #2> /dev/null
done
#Rscript EDA_workhorse.R T

## cool function, it finds all file dependencies automatically
Rscript -e 'rsconnect::deployDoc("COVIDEDA.Rmd", forceUpdate = getOption("rsconnect.force.update.apps", TRUE))'

url=https://lucha6.shinyapps.io/covideda/

open $url

