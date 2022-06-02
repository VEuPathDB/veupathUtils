#### Getting started with VEuPathDB package development

### Setting up the environment

## Check out where we are (assuming running the docker container and that plot.data and microbiomeComputations also checked out)
list.dirs(".", recursive=F)
# [1] "./.devcontainer"          "./microbiomeComputations"
# [3] "./plot.data"           "./veupathUtils"

## Set directory
# Often it's easiest to set the directory to the repo in which we're actively developing
setwd('veupathUtils') # If developing veupathUtils

## For developing veupathUtils
# TODO

## For developing plot.data
# First have plot.data checked out in a directory adjacent to veupathUtils
# Handle plot.data environment (time zone, for example)
library(dotenv)
load_dot_env(file=".dev/.env")


## For developing microbiomeComputations
# First have microbiomeComputations checked out in a directory adjacent to veupathUtils
# TODO


## Load libraries generally helpful for development
library(devtools)
library(sloop)

## Load package under development. Assuming we've set the directory to be the package we're developing, we can use
devtools::load_all()


### Running tests
devtools::test() # Runs all tests in the directory
devtools::test_file() # Runs all tests within the open file

### Debugging with real data
# Assuming there is a file containing real data (for example, from the EDA merging service) in a directory called real_data
# Read in with data.table::fread, just like we do in the EDA data service
# Assume column names are 'x' and 'y'.
df <- data.table::fread('../real_data/my_eda_error.tab',select=c('x'='numeric','y'='character'), na.strings=c(''))