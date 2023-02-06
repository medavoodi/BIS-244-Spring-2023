############################################################################
# Recommend you DO NOT run lines below here unless you want to download
# "Yellow_Sample.csv" data file from GitHub and are prepared to wait.
############################################################################

# Clear out Console and Environment
rm(list=ls(all=TRUE))
cat("\014")

# Download the original data file; 
# https://intro-to-r.com/data-files
# A timed example of readr::read_csv()
### Run as a block of text to time #########
ptm <- proc.time()
DF <- read_csv("Yellow_Sample.csv", col_names=TRUE)
READR_READ_TIME <- (proc.time() - ptm)
READR_READ_TIME

# Reminder of what's in the data
str(DF)

# Tired of typing name of data.frame all the time, so...
attach(DF)

# Store the max value for our FOR loops
n <- length(VendorID)
n

# Was going to take WAY too long to process the whole file, so am only going to 
# process .1 percent

n <- n/1000

# Loop to calculate trip_length (i.e., time of trip)
ptm <- proc.time()
pcter <- 0
for (i in 1:n) {
  ### This if() command is just to provide the "dots" for visual feedback
  if(pcter >= 2*n/100) {
    cat(".")
    pcter <- 0
  } else {
    pcter <- pcter + 1
  }
  ###
  
  ### The single line below is the main point of the loop, calculating trip_length  
  DF$trip_length[i] <- difftime(tpep_dropoff_datetime[i],tpep_pickup_datetime[i],
                                units="mins")
}
LOOP_TIME <- (proc.time() - ptm)
LOOP_TIME

# Let's see what we got
View(DF)

cat("If we had processed the whole file, would have taken",
    as.numeric(LOOP_TIME[3], units = "hours"),"hours.")

# Now, will use dplyr::mutate() command, which uses the fact that columns in 
# data frame are vectors, allowing it to "vectorize" calculations

ptm <- proc.time()
DF <- mutate(DF,trip_length2=(tpep_dropoff_datetime - tpep_pickup_datetime)/60)
MUTATE_TIME <- (proc.time() - ptm)
MUTATE_TIME

View(DF)

cat("Took",as.numeric(MUTATE_TIME[3]), units = "seconds")

# Moral of the story: use FOR() loops very cautiously on "big" data