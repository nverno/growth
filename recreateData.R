## Keep track of data manipulations for growth analysis
## Run this script to recreate all datasets used in growth analysis
source("~/work/functions/functions-datatrans.R")

## read master data
source("~/work/data/data-prep/read-moose.R")

## add/remove columns, annualize growth, rename columns if they changed
## creates "~/work/data/data/moose-wide.csv"
source("~/work/data/data-prep/clean-moose.R")

## make long version of data
## creates "~/work/data/data/moose-long.csv"
source("~/work/data/data-trans/make-long-moose.R")
