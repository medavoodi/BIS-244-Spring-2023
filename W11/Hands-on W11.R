## Hands-on 1
library(gapminder)
library(tidyverse)
library(ggrepel)
library(socviz)

# Considering American Sociological Association Section Membership dataset (asasec), For 2013 show the relatioship between members and revenues and also cosider different colors for having journal. Use the linear prediction line.



## Hands-on 2
# Lebel those outliers with revenue bigger than $10,000




## Hands-on 3
# Revise the code below to use *Accent* for color
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p + geom_point(size = 1.5) +
  theme(legend.position = "top")


## Hands-on 4
# Find Lehigh university and Rutgers University color codes and dispaly it here 