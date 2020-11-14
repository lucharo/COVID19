#!/bin/bash

path=$(git rev-parse --show-toplevel)
cd $path

## Run data source + cleaning Rmd
Rscript -e "rmarkdown::render('scripts/datacleaning.Rmd')"

## Run plot scripts
Rscript -e "
	source(scripts/treemap.R)
	source(scripts/line_plot_cumulative.R
	source(scripts/line_plot_daily.R
	source(scripts/maps.R "


## Deploy to shinyapps.io, account need to have been set up prior to this.
Rscript -e 'rsconnect::deployDoc("COVIDEDA.Rmd", forceUpdate = getOption("rsconnect.force.update.apps", TRUE))'

url=https://lucha6.shinyapps.io/covideda/

open $url

