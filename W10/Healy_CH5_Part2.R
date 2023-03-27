# Fall 2022-- BIS 244

library(gapminder)
library(tidyverse)
library(socviz)

# Reloading by_country from organdata

by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize(donors_mean= mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(gdp, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE),
            cerebvas_mean = mean(cerebvas, na.rm = TRUE))

by_country
# donors. Organ Donation rate per million population.
# gdp. Gross Domestic Product in thousands of PPP dollars.
# health. Health spending, thousands of PPP dollars per capita.
# roads. Road accident fatalities per 100,000 population.
# cerebvas. Cerebrovascular deaths per 100,000 population (rounded).

## 5.3 Plot Text Directly
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))
p + geom_point() + geom_text(mapping = aes(label = country))
      

# can use hjust to left- (0) or right-justify (1) 
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean))

p + geom_point() + geom_text(mapping = aes(label = country), hjust = -.1)
      
# ======================= hands-on 1 vjust ================

# ggrepel provides geoms for ggplot2 to repel overlapping text labels:

# geom_text_repel()
# geom_label_repel()

# Text labels repel away from each other, away from data points, 
# and away from edges of the plotting area (panel).

library(ggrepel)      
p <- ggplot(data = by_country,
            mapping = aes(x = roads_mean, y = donors_mean, label = country))
p + geom_point()  + geom_text_repel()

# A look at date in elections_historic
View(elections_historic)

elections_historic %>% select(2:7)       


# Using geom_text_repel
p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct,
                                    label = winner_label))

p + geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
    geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = x_label, y = y_label, title = p_title, subtitle = p_subtitle,
         caption = p_caption)      
# ======================= hands-on 2 geom_text_repel ================
## 5.4 Label Outliers
# Working with organdata dataset

# Can use subset() to only provide text lables for certin points
# Use 
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country, gdp_mean > 25000),
                    mapping = aes(label = country))

# Subset() can be fairly complex
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() +
    geom_text_repel(data = subset(by_country,
                                  gdp_mean > 25000 | health_mean < 1500 |
                                  country %in% "Belgium"),
                    mapping = aes(label = country))      

# Instead of geom_text_repel, you can use geom_text
p <- ggplot(data = by_country,
            mapping = aes(x = gdp_mean, y = health_mean))

p + geom_point() + geom_text(data=subset(by_country, gdp_mean > 25000), 
                             mapping = aes(label = country), hjust = -.1)

p + geom_point() +
  geom_text(data = subset(by_country,
                                gdp_mean > 25000 | health_mean < 1500 |
                                  country %in% "Belgium"),
                  mapping = aes(label = country), hjust = -.1)

# Can also use "dummy variable" to pick out specific points before we graph
# ccode. Abbreviated country code.
organdata$ind <- organdata$ccode %in% c("Ita", "Spa") &
                    organdata$year > 1998

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors, color = ind))
p + geom_point() +
    geom_text_repel(data = subset(organdata, ind),
                    mapping = aes(label = ccode)) +
    guides(color = "none")   # Remove some guides

# =================== hands-on 3 - conditions =================
## 5.5 Write and Draw in the Plot Area

# Using annotate() to insert text
p <- ggplot(data = organdata, mapping = aes(x = roads, y = donors))
p + geom_point() + annotate(geom = "text", x = 91, y = 33,
                            label = "A surprisingly high \n recovery rate.",
                            hjust = 0)
      


# Using annotate() to highlight a region
p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors))
p + geom_point() +
    annotate(geom = "rect", xmin = 125, xmax = 155,
             ymin = 30, ymax = 35, fill = "red", alpha = 0.2) + 
    annotate(geom = "text", x = 157, y = 33,
             label = "A surprisingly high \n recovery rate.", hjust = 0)      


## 5.6 Understanding Scales, Guides, and Themes


# Example using 3 aesthetic mappings
p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point()


# scale_() functions change the dimensioning of an aesthetic

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    scale_x_log10() +
    scale_y_continuous(breaks = c(5, 15, 25),
                       labels = c("Five", "Fifteen", "Twenty Five"))


# When working with a scale that produces a legend, can specify the labels in the key

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    scale_color_discrete(labels =
                             c("Corporatist", "Liberal",
                               "Social Democratic", "Unclassified")) +
    labs(x = "Road Deaths",
         y = "Donor Procurement",
        color = "Welfare State")



# Can also tell ggplot we don't want a guide for that scale

p <- ggplot(data = organdata,
            mapping = aes(x = roads,
                          y = donors,
                          color = world))
p + geom_point() +
    labs(x = "Road Deaths",
         y = "Donor Procurement") +
    guides(color = "none")


