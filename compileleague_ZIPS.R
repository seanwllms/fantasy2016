setwd("/home/sean/Documents/rstuff/fantasy")
library(dplyr)

#load in coefficients file
if (!file.exists("coefs.rda")) {
  source("historyanalysis.R")
}

#load hitter and pitcher projections
if (!file.exists("projections_ZIPS.rda")) {
  source("calculatevalue_ZIPS.R")
} else{
  load("projections_ZIPS.rda")
}

#Build league
source("leaguesetup.R")

#run draft
source("draftpicks.R")

#merge in projections
source("mergeinprojections.R")

#calculate standings
source("calculatestandings.R")

#write to .csv
source("csvwriter.R")

standings.output