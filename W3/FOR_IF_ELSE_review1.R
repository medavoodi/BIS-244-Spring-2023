# This script is designed to provide a refresher/introduction
# of the topic of IF..ELSE statements and FOR loops

# The following assume you already have the covid-19-data repo
# set up as a submodule of the BIS-244-Fall-2022 repo, and that
# you have that data set up as a subfolder of this project's
# folder.

# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")

# Let's read in the us-counties file from covid-19-data

# We'll use package "readr", which is part of the tidyverse
library(tidyverse)

# Storing the path of the current working directory
Temp <- getwd()

# Switching the working directory to the covid-19-data subfolder
setwd("./covid-19-data/")

# Reading the us.counties.csv in as a data frame
COUNTIES <- read_csv("us-counties.csv")

# Switching the working directory back to the project folder
setwd(Temp)

## Alternative way to access subfolders
library(here)
COUNTIES <- read_csv(here("covid-19-data","us-counties.csv"))

# Examining the data
View(COUNTIES)

# Using filter()to get just Snohomish county in Washington
SNOHOMISH <- filter(COUNTIES, state=="Washington" &
                      county=="Snohomish")
View(SNOHOMISH)

# Set n to legth of data set
n <- length(SNOHOMISH$date)

# Initialize new variable in data frame
SNOHOMISH$incr_cases <- 1

View(SNOHOMISH)

# Calculate values for other than first row using FOR loop

for (i in 2:n) {
  SNOHOMISH$incr_cases[i] <- (SNOHOMISH$cases[i]-SNOHOMISH$cases[i-1]) 
}

View(SNOHOMISH)

# Plot what we've got

p <- ggplot(data = SNOHOMISH,
            mapping = aes(x = date, y = incr_cases))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Cases",
       title = "COVID-19 Cases in Snohomish County, WA",
       subtitle = "Data points are incremental new confirmed cases",
       caption = "Source: NY Times")

# Let's replace 0 values with NA using IF..ELSE statement

for (i in 1:n) {
  if (SNOHOMISH$incr_cases[i]==0) {
    SNOHOMISH$incr_cases[i] <- NA
  } else {}
}

View(SNOHOMISH)

# Replot

p <- ggplot(data = SNOHOMISH,
            mapping = aes(x = date, y = incr_cases))

p + geom_point() +
  labs(x = "Dates", y = "Incremental Cases",
       title = "COVID-19 Cases in Snohomish County, WA",
       subtitle = "Data points are incremental new confirmed cases",
       caption = "Source: NY Times")

# Remember, we have replaced some value of incr_cases with NA, so...

mean(SNOHOMISH$incr_cases)

# There IS a workaround for some commands, such as mean()
mean(SNOHOMISH$incr_cases, na.rm=TRUE)
meancases <- mean(SNOHOMISH$incr_cases, na.rm=TRUE)

# Initialize seperate vectors for cases above and below average
SNOHOMISH$above_cases <- 0
SNOHOMISH$below_cases <- 0

# But there is no easy workaround for logical tests, such below where
# we have "NA" in a variable in a logical test inside an IF() statement

for (i in 1:n) {
  if(SNOHOMISH$incr_cases[i]>=meancases) {
    SNOHOMISH$above_cases[i] <- SNOHOMISH$incr_cases[i]
  } else {
    SNOHOMISH$below_cases[i] <- SNOHOMISH$incr_cases[i]
  }
}

# Return incr_cases to what it was when we first computed it

SNOHOMISH$incr_cases <- 1
for (i in 2:n) {
  SNOHOMISH$incr_cases[i] <- (SNOHOMISH$cases[i]-SNOHOMISH$cases[i-1]) 
}

View(SNOHOMISH)

# Assign values to above_cases and below_cases based on whether incr_cases
# is greater than or less than average

for (i in 1:n) {
  if(SNOHOMISH$incr_cases[i]>=meancases) {
    SNOHOMISH$above_cases[i] <- SNOHOMISH$incr_cases[i]
  } else {
    SNOHOMISH$below_cases[i] <- SNOHOMISH$incr_cases[i]
  }
}

View(SNOHOMISH)

# Plot what we've got

p = ggplot() + 
  geom_point(data = SNOHOMISH, aes(x = date, y = above_cases), color = "red") +
  geom_point(data = SNOHOMISH, aes(x = date, y = below_cases), color = "green") +
  labs(x = "Dates", y = "Incremental Cases",
       title = "Incremental COVID-19 Cases in Snohomish County, WA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p

# Now, the values on the x-axis are really ugly, so, replace 0 values with "NA"

for (i in 1:n) {
  if(SNOHOMISH$above_cases[i]==0) {
    SNOHOMISH$above_cases[i] <- NA
  } else {}
  if(SNOHOMISH$below_cases[i]==0) {
    SNOHOMISH$below_cases[i] <- NA
  } else {}
}

# Now, our plot will look much better

p = ggplot() + 
  geom_point(data = SNOHOMISH, aes(x = date, y = above_cases), color = "red") +
  geom_point(data = SNOHOMISH, aes(x = date, y = below_cases), color = "green") +
  labs(x = "Dates", y = "Incremental Cases",
       title = "Incremental COVID-19 Cases in Snohomish County, WA",
       subtitle = "Red = Above Average, Green = Below Average",
       caption = "Source: NY Times")
p

