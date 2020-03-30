bash getData.sh

Rscript EDA_workhorse.R T

## cool function, it finds all file dependencies automatically
Rscript -e 'rsconnect::deployDoc("COVID_EDA.Rmd", forceUpdate = getOption("rsconnect.force.update.apps", TRUE))'

