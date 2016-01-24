#set up file
setwd("/home/sean/Documents/rstuff/fantasy")
library(dplyr)
library(reshape2)

#read in results from pre-2014
results <- read.csv("./history/historicalresults.csv") %>%
      select(1:4)

#fill in results for years that it is missing
results[1:540, 4] <- c(rep(2010, 18), rep(2011, 18), rep(2012, 18))

#convert to lowercase
results$Category <- tolower(results$Category)

######################################
#####read in the 2014 results#########
######################################
standings.2014 <- read.csv("./history/results2014.csv",stringsAsFactors = FALSE) %>% 
      select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) 

standings.2014[,c("R","K")] <- sapply(standings.2014[,c("R","K")], gsub,pattern=",",replacement="")

standings.2014 <- sapply(standings.2014, as.numeric)

standings.2014 <- melt(standings.2014) %>% mutate(Category = tolower(Var2), Value = value, Year=2014) %>% select(4:6) %>%
      group_by(Category) %>%
      arrange(desc(Value)) %>%
      mutate(Points=min_rank(Value)) 

standings.2014[standings.2014$Category %in% c("era", "whip"),"Points"] <- 19-standings.2014[standings.2014$Category %in% c("era", "whip"),"Points"]

######################################
#####read in the 2015 results#########
######################################
standings.2015 <- read.csv("./history/results2015.csv",stringsAsFactors = FALSE) %>% 
      select(R, HR, RBI, SB, Avg, W, K, Sv, ERA, WHIP) 

standings.2015 <- sapply(standings.2015, as.numeric)

standings.2015 <- melt(standings.2015) %>% mutate(Category = tolower(Var2), Value = value, Year=2015) %>% select(4:6) %>%
      group_by(Category) %>%
      arrange(desc(Value)) %>%
      mutate(Points=min_rank(Value)) 


standings.2015[standings.2015$Category %in% c("era", "whip"),"Points"] <- 19-standings.2015[standings.2015$Category %in% c("era", "whip"),"Points"]

######################################
##### Merge All Results Together######
######################################
results <- rbind(results, standings.2014, standings.2015)



######################################
#####Graphs and Analysis go Here######
######################################
library(ggplot2)
library(broom)

catplot <- ggplot(results, aes(x=Value, y=Points)) +
      geom_point() +
      facet_wrap(~ Category, ncol=2, scales="free_x")
catplot

###Regression Time####

#filter out 1 point and 18 point recipients (skew results)
regress <- results %>% filter(Points > 1 & Points < 18)

#run the regression for each category
regress <- regress %>% group_by(Category) %>%
      do(regresults = lm(Points ~ Value, data=.))

#organize regression results in to tidy df for calculating value 
coefs <- tidy(regress, regresults) %>%
      filter(term == "Value") %>%
      select(Category, estimate)

save(coefs, file="coefs.rda")

#organize regression results in to tidy df for calculating standings
coefs.standings <- tidy(regress, regresults) %>% select(Category, term, estimate) %>%
  dcast(Category ~ term) 

names(coefs.standings)[2:3] <- c("yint", "coef")

save(coefs.standings, file="standingscoefs.rda")
