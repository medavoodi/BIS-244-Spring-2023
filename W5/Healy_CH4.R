## Intended to accompany BIS 244 Video Set 05

library(gapminder)
library(tidyverse)
library(socviz)

# ========================================================================
#                             Chpater 3 review
# ========================================================================

# Reminder of what gapminder looks like
view(gapminder)

# Brief reminder of what we looked at in chapter 3

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p

# We plotted a scatterplot of y against x
p + geom_point() 

# Then we added a smoothed line of y against x

p + geom_smooth()

# Then we combined both the scatterplot and the smoother line

p + geom_point() + geom_smooth() 

# Convert the x axis to log10 and use the linear model to smooth 
# and add some explanations

p + geom_point(alpha=0.2) + geom_smooth(method = "lm") + scale_x_log10() + 
labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
     title = "Economic Growth and Life Expectancy",
     subtitle = "Data points are country-years",
     caption = "Source: Gapminder.")

# Coloring/Groupinh based on the continent  
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()

# Coloring based on the continent but the smooth line based on all data 

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10()

#Mapping a continuous variable to color.

p + geom_point(mapping = aes(color = log(pop))) +
  scale_x_log10()    
# ========================================================================
#                             Chpater 4  
# ========================================================================

## 4.2 Grouped Data and the "Group" Aesthetic

# In section 4.2, let's try the same thing, but with different variables

p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line()       

# To see what's going on, take a look at structure of data

str(gapminder)

# We are asking it to plot a Factor variable on a continuous axis

# Let's plot continuous variable against continuous variable, but grouped by Factors

p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line(mapping = 
                    aes(group = country))


## 4.3 Facet to Make Small Multiples
p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))

p + geom_line(mapping = aes(group = country)) + 
    facet_wrap(~ continent)      

# What facet_wrap function does is to shrink the dataset first 
# and then plot different plots for continents 

africa.df <- gapminder[gapminder$continent == "Africa",]
p.africa <- ggplot(data = africa.df,
            mapping = aes(x = year,
                          y = gdpPercap))

p.africa + geom_line(mapping = aes(group = country)) 

# Note how the layout of the picture allows facets to share axes titles

# Also note that "~ continent" is a powerful statement. For now, DON'T use it
# on continuous variables, and be careful about using it Factors with a lot of levels.

p + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ country)   

p.africa + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ country)   

# FIne-tuning facet_wrap() to specify 5 columns

p + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ continent, ncol = 5)   

p.africa + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ country, ncol = 13)      

# Let's add a smoothing line, set color for each country to grey, 
# use log10 y-axis, and add labels

p + geom_line(color="gray70", mapping=aes(group = country)) +
    geom_smooth(size = 1.1, method = "loess", se = FALSE) +
    scale_y_log10(labels=scales::dollar) +
    facet_wrap(~ continent, ncol = 5) +
    labs(x = "Year",
         y = "log GDP per capita",
         title = "GDP per capita on Five Continents")      

# Why did we specify "se = FALSE"?

# Healy switches to GSS data. Let's look at it:

View(gss_sm)
head(gss_sm)
str(gss_sm)
# Using the gss_sm data set to show faceting by two Factors at the same time

p <- ggplot(data = gss_sm, mapping = aes(x = age, y = childs))
p + geom_point()

# facet this relationship by sex and race of the respondent.
p + geom_point() + facet_grid(sex ~ race)

# Add smooth line for each facet
p + geom_point(alpha = 0.2) + 
    geom_smooth() +
    facet_grid(sex ~ race)

# Let's look at another example of the usefulness of faceting

# Following data is in your repo, and is from 
# https://ourworldindata.org/policy-responses-covid#government-stringency-index

owid_covid_data <- read_csv("owid-covid-data.csv")

# The above will generate some warnings, which we will ignore for now 

# Let's look at the data

View(owid_covid_data)

# structure of the dataset
str(owid_covid_data)

# The number of variables 
length(owid_covid_data)

# The number of observations 
length(owid_covid_data$iso_code)


t(t(names(owid_covid_data)))

# Handy built-in function for categorizing variables is cut()

categories <- c(-10,0,10,20,30,40,50,60,70,80,90,100)
owid_covid_data$s_bucket <- cut(owid_covid_data$stringency_index, categories)

# How many observations in each bucket?
summary(owid_covid_data$s_bucket)

str(owid_covid_data$s_bucket)

#Faceting the data based on the stringency
p <- ggplot(data = owid_covid_data, mapping = aes(x = date, y = new_cases))
p + geom_point(alpha = 0.2) + 
  facet_wrap(owid_covid_data$s_bucket)

# Drop Rows with NA’s
owid_covid_data.NA1 <- owid_covid_data[complete.cases(owid_covid_data$s_bucket),]
owid_covid_data.NA2 <- filter(owid_covid_data,!is.na(s_bucket))

p.NA <- ggplot(data = owid_covid_data.NA1, mapping = aes(x = date, y = new_cases))
p.NA + geom_point(alpha = 0.2) + 
  facet_wrap( ~ s_bucket)

sum(is.na(owid_covid_data.NA2$continent))
