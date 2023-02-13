## ----setup----

library(gapminder)
library(here)
library(tidyverse)
library(socviz)

## 3.3 Mappings Link Data to Things You See

# Reminder of what gapminder looks like
gapminder

p <- # Defining the landscape of our graph 
p <- ggplot(data = gapminder)
p

# Giving our landscape more structure
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p

## 3.4 Build Your Plots Layer by Layer
p + geom_point() 

gapdata <- as.data.frame(gapminder)

# Life expectancy vs GDP, using a smoother.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth()

# Add back individual dots as well

p   + geom_point()+ geom_smooth()


# ================= Solve part a of Hands-on Week 4

# Fitting a linear regression to data:
# You can add a straight “linear model” line.

p + geom_point() + geom_smooth(method = "lm") 

# Default method:
# By default, the trend line that’s added is a gam smooth line.
# gam: "generalized additive mode" smoothing
# Smoothing methods: lm, glm, gam, loess

p + geom_point() + geom_smooth(method = "gam") 
p + geom_point() + geom_smooth() 

# Converting X scale to logarithmic base 10 scale

p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

# Using Loess mode smoothing
# In ggplot2 this should be done when you have less than 1000 points, 
# otherwise it can be time consuming.

p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()


# Setting labels on scale_() functions
# Load whole package: EX: library(scales)
# or Load a function from a package: EX: scales::dollar

## ----Load whole package----
# library(package)
## ----Load a function from a package----
# package::function

# dollar: Format numbers as currency, rounding values to dollars or cents
p + geom_point() +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar)


## 3.5 Mapping Aesthetics vs Setting Them

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = "purple"))
# Note: have mapped a dimension ("color") into a character constant ("purple")

p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()


# If you want to use a color for all points

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(color = "purple") +
  geom_smooth(method = "loess") +
  scale_x_log10()


# The various geom_ functions can take many other arguments 
# That will affect how the plot looks.
# But that do not involve mapping variables to aesthetic elements.

# Example: Setting opacity for points and color for line
# “alpha” is an aesthetic property that points
# Alpha refers to the opacity of a geom. Values of alpha range from 0 to 1.
# With lower values corresponding to more transparent colors. 

p + geom_point(alpha = 0.3) +
  geom_smooth(color = "orange", se = FALSE, size = 2, method = "lm") +
  scale_x_log10()


# Tying all this together:

p + geom_point(alpha = 0.3) + geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")

# ================= Solve parts b and c of Hands-on Week 4

# Using multiple colors as a dimension
# EX: "Mapping" the "continent" variable to the color "aesthetic".
#  In this case we get both our points and smoothers colored by continent.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()

# ================= Solve part d of Hands-on Week 4

## We might also consider shading the standard error ribbon of each line 
# to match its dominant color: fill = continent
## Making sure that color and fill aesthetics match up consistently in this way 
# improves the overall look of the plot.
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10()


## 3.6 Aesthetics Can Be Mapped per Geom

# Here color is mapped to continent for the points
# but not the smoother.

p <- ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10()


#Mapping a continuous variable to color.

p + geom_point(mapping = aes(color = log(pop))) +
  scale_x_log10()    
# ================= Solve part e of Hands-on Week 4


## 3.7 Save Your Work

# Save your entire plot (including aesthetics) to an object
p_out <- p + geom_point(mapping = aes(color = log(pop))) +
  scale_x_log10()
p_out

# Save a figure individually
ggsave("ifexp_vs_gdp_gradient.pdf",plot = p_out)

# ggsave() currently recognizes the following extensions:
#eps/ps, tex (pictex), df, jpeg, tiff, png, bmp, svg and wmf (windows only).

# Save as jpeg:
ggsave("ifexp_vs_gdp_gradient.jpg",plot = p_out)




