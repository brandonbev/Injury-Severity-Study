# Brandon Vermeer, Carson Mohr, Corey Coole, James Altman, Danny Breyfogle
# Spring 2017
# _________________________________________________________________

rm(list=ls()) # Clear environment
setwd("/Users/BrandonVermeer/Desktop")
df <- read.table('drunkdriving.txt',header=T) # Import data
df <- subset(df,select=-c(Obs.)) # Obs. not needed

# Collapsing atmospheric conditions into single dummy variable
bad <- c(2:7,10:12) # Numbers which correspond to bad weather
df$badweather <- df$atmcond%in%bad|df$atmcond2%in%bad

# Collapsing light condition into a dummy variable
light <- c(1,4,5) # Numbers which correspond to light conditions
df$light <- df$lightcond%in%light

# Aggregrating by case number
# manncol,intersectiontype, badweather, and light are the same for each
#     person in a case number
# Injury, speeding, age, and alcres are totaled for each person involved
#     in the case (since crash severity is dependent on the attributes
#     of each person).
# totalseverity is a fraction of the worst case scenario (everybody dies)
# totalspeeding is the number of vehicles which were speeding in the crash
# totalage is the combined age of all people involved in the crash
# totalalcres is the combined BAV of all people involved
aggdf <- as.data.frame(matrix(nrow=length(unique(df$casenum)),ncol=0))
i.df <- 0
i.aggdf <- 0
for (i in unique(df$casenum)) {
  matches <- which(df$casenum==i)
  i.df <- matches[1]
  i.aggdf <- i.aggdf+1
  totalseverity <- 0
  totalspeeding <- 0
  totalage <- 0
  totalalcres <- 0
  for (j in matches) {
    totalseverity <- totalseverity + df$injury[j]
    totalspeeding <- totalspeeding + df$speeding[j]
    totalage <- totalage + df$age[j]
    totalalcres <- totalalcres + df$alcres[j]
  }
  aggdf$casenum[i.aggdf] <- df$casenum[i.df]
  aggdf$manncol[i.aggdf] <- df$manncol[i.df]
  aggdf$totalspeeding[i.aggdf] <- totalspeeding
  aggdf$intersectiontype[i.aggdf] <- df$intersectiontype[i.df]
  aggdf$totalage[i.aggdf] <- totalage
  aggdf$totalalcres[i.aggdf] <- totalalcres
  aggdf$totalseverity[i.aggdf] <- totalseverity/(length(matches)*4)
  aggdf$badweather[i.aggdf] <- df$badweather[i.df]
  aggdf$light[i.aggdf] <- df$light[i.df]
}

# There seems to be some cases with no fatalities. This is a bug in
# the FARS dataset. In this dataset, the maximum people involved in a
# case is 4. Total severity should not be below 0.25.
aggdf <- aggdf[aggdf$totalseverity>=0.25,] # Remove cases with no fatalities

# Factor necessary variables
aggdf$manncol <- factor(aggdf$manncol)
aggdf$intersectiontype <- factor(aggdf$intersectiontype)
aggdf$badweather <- factor(aggdf$badweather)
aggdf$light <- factor(aggdf$light)
aggdf$totalspeeding <- factor(aggdf$totalspeeding)

# Fit a linear model with the variables
mod <- lm(totalseverity~manncol+totalspeeding+intersectiontype+totalage+
            totalalcres+badweather+light,aggdf)
summary(mod)
library(car)
residualPlots(mod,~1)

max.mod <- lm(totalseverity ~ .,aggdf)
summary(max.mod)

step.mod <- step(max.mod)
summary(step.mod)


