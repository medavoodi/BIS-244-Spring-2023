# BIS 244 Fall 2022
# ========================================================================
#                           Chpater 4 - Review Part 1
# ========================================================================

library(gapminder)
library(tidyverse)
library(socviz)

# Reminder of what is in gss_sm data

View(gss_sm)
str(gss_sm)

# Quick review: have previously used both geom_smooth() and geom_point()

p <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs))
p + geom_point(alpha = 0.2) + 
  geom_smooth() +
  facet_grid(sex ~ race)

p01 <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs, color=sex))
p01 + geom_point(alpha = 0.2) + geom_smooth()

p02 <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs, color=race))
p02 + geom_point(alpha = 0.2) + geom_smooth()


install.packages("gridExtra")
library(gridExtra)

p03 <- ggplot(data = gss_sm[gss_sm$sex=="Male" & gss_sm$race=="White",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth() + labs(subtitle = "Male ~ White")

p04 <- ggplot(data =  gss_sm[gss_sm$sex=="Male" & gss_sm$race=="Black",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth() + labs(subtitle = "Male ~ Black")

p05 <- ggplot(data =  gss_sm[gss_sm$sex=="Male" & gss_sm$race=="Other",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth() + labs(subtitle = "Male ~ Other")

p06 <- ggplot(data = gss_sm[gss_sm$sex=="Female" & gss_sm$race=="White",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth()  + labs(subtitle = "Female ~ White")

p07 <- ggplot(data =  gss_sm[gss_sm$sex=="Female" & gss_sm$race=="Black",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth()  + labs(subtitle = "Female ~ Black")

p08 <- ggplot(data =  gss_sm[gss_sm$sex=="Female" & gss_sm$race=="Other",], 
             mapping = aes(x = age, y = childs)) +
  geom_point(alpha = 0.2) + geom_smooth()  + labs(subtitle = "Female ~ Other")

grid.arrange(p03,p04,p05,p06,p07,p08,nrow=2,ncol=3)

# ========================================================================
#                           Chpater 4 - Part 2
# ========================================================================
## 4.4 Geoms Can Transform Data

# Using geom_bar() for categorical variables (i.e., factors)

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar()

# Default y-dimension is count, but can specify others:
p + geom_bar(mapping = aes(y = ..count..))

# You can change the default to other functions, like proportion 

p + geom_bar(mapping = aes(y = ..prop..))

# ..prop.. is a temporary variable, and defaults to the proportion of each x "group"

# if we specify group = 1, tells geom_bar() to use total number of observations instead 

p + geom_bar(mapping = aes(y = ..prop.., group = 1)) 

# table() will list the frequencies for all values of a factor

table(gss_sm$religion)

# Use geom_bar() again, but specify x & color = religion in ggplot()

p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, color = religion))
p + geom_bar()

# Fill is "livelier" than color:

p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, fill = religion))
p + geom_bar() 

# But do we really need a legend in this case?

p + geom_bar() + guides(fill = FALSE)

# ========================================================================
## 4.5 Frequency Plots the Slightly Awkward Way

# Let's specify x = bigregion, but fill=religion

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar()

# changing x axis and fill argument produces something different,

p0 <- ggplot(data = gss_sm,
            mapping = aes(x = religion, 
                          fill = bigregion)) 
p0 + geom_bar()

# Note that, here, we DO need a legend

# But colors here are showing counts, not proportions. If we want proportions:

p + geom_bar(position = "fill")   

# If we want bars by religion in each bigregion, use "dodge"

p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop..))      

# Just as we did with "1-way" bar charts before, can use "group = "...

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = religion)) 

p1 + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = bigregion)) 

# At some point, we have to ask ourselves if there isn't maybe a less-cluttered way to do this:

p <- ggplot(data = gss_sm,
            mapping = aes(x = religion))

p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = bigregion)) +
  facet_wrap(~bigregion, ncol =1)

p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = bigregion)) +
  facet_wrap(~bigregion, nrow =1)

p2 <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))

p2 + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = religion)) +
  facet_wrap(~religion, nrow =1)

# ========================================================================
## 4.6 Histograms and Density Plots

# Healey uses midwesat data set now

View(midwest)

# Similar to geom_bar(), but geom_histogram() is for continuous variables

p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_histogram()

# Note that it defaults to counts, and chooses "bin" sizes for us

# But we can specify bin size:

p + geom_histogram(bins = 10)
p + geom_histogram(bins = 15)
p + geom_histogram(bins = 22)
p + geom_histogram(bins = 40)

# One way to make a histogram for a subset of the data:

# 1. Subset the data

data_subset <- filter(midwest,(state=="OH" | state=="WI"))

# 2a. Graph the subset

p <- ggplot(data = data_subset,
            mapping = aes(x = percollege))

p + geom_histogram(bins = 20) +
  facet_wrap(~state)

# 2b. Or graph the subset on one graph, differentiating states by fill colors

p <- ggplot(data = data_subset,
            mapping = aes(x = percollege, fill = state))

p + geom_histogram(alpha = 0.4, bins = 20)

# Reminder: alpha sets opacity, handy for multiple lines/fills on one graph

# Question: are these "stacked" or behind one another?

# Subsetting in the ggplot() command

oh_wi <- c("OH", "WI")

# The %in% operator is a convenient way to filter on more than one 
# term in a variable when using subset().

p <- ggplot(data = subset(midwest, subset = state %in% oh_wi),
            mapping = aes(x = percollege, fill = state))
p + geom_histogram(alpha = 0.4, bins = 20)

# Similar to a histogram with very small bin size is density plot:
p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_density()

### Healy could have illustrated their similarity by:
p + geom_histogram() + geom_density()
p + geom_histogram(bins = 100) + geom_density()


# Can use fill and color in geom_density plots, as well
p <- ggplot(data = midwest,
            mapping = aes(x = area, color = state))
p + geom_density(alpha = 0.1)

p + geom_line(stat = "density")

p <- ggplot(data = midwest,
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.1)

p + geom_line(stat = "density")

# ..scaled..: proportional density estimate. 
p <- ggplot(data = subset(midwest, subset = state %in% oh_wi), 
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3, mapping = (aes(y = ..scaled..)))


# ========================================================================
## 4.7 Avoid Transformations When Necessary
# (i.e., how to tell ggplot() not to summarize observations for us)

# Will use titanic data

titanic

# Suppose we want to plot bar chart showing sex by fill for each fate 

p <- ggplot(data = titanic,
            mapping = aes(x = fate, fill = sex))
p + geom_bar() 

# What is going on? It's counting occurrence of factors by number of rows, not using "n"

# Need to tell is to use "percent" column as y, and not to try to count rows 
# Move the legend to the top of the chart: theme(legend.position = "top")

p <- ggplot(data = titanic,
            mapping = aes(x = fate, y = percent, fill = sex))
p + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")
p + geom_bar(position = "dodge") + theme(legend.position = "top") # NOT WORKING

# geom_col() is like geom_bar(), but with an assumed stat = "identity"

oecd_sum

# Example:

p <- ggplot(data = oecd_sum,
            mapping = aes(x = year, y = diff, fill = hi_lo))
p + geom_col() + guides(fill = FALSE) +
  labs(x = NULL, y = "Difference in Years",
       title = "The US Life Expectancy Gap",
       subtitle = "Difference between US and OECD
                   average life expectancies, 1960-2015",
       caption = "Data: OECD. After a chart by Christopher Ingraham,
                  Washington Post, December 27th 2017.")

p + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top") + 
  labs(x = NULL, y = "Difference in Years",
       title = "The US Life Expectancy Gap",
       subtitle = "Difference between US and OECD
                   average life expectancies, 1960-2015",
       caption = "Data: OECD. After a chart by Christopher Ingraham,
                  Washington Post, December 27th 2017.")
