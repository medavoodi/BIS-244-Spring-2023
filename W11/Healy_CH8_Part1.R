## BIS 244 Chapter 8

library(gapminder)
library(tidyverse)
library(ggrepel)
library(socviz)


# Let's take a look at the asasec data
head(asasec)

asasec <- asasec

# Reminder of scatterplot with smoothed line

p <- ggplot(data = subset(asasec, Year == 2014),
            mapping = aes(x = Members, y = Revenues, label = Sname))

p + geom_point() + geom_smooth()


# Let's use color to indicate whether each section has a Journal
# and switch smoothing to OLS

p + geom_point(mapping = aes(color = Journal)) +
    geom_smooth(method = "lm")

# ================== hands-on 1 ==================
# Let's slim down the smoothed line

p0 <- ggplot(data = subset(asasec, Year == 2014),
             mapping = aes(x = Members, y = Revenues, label = Sname))

p1 <- p0 + geom_smooth(method = "lm", se = FALSE, color = "gray80") +
    geom_point(mapping = aes(color = Journal)) 

p1

# To show that default geom_smooth() and nethod="lm" are different
p1 + geom_smooth()

#  ... and add some text labels...
p2 <- p1 + 
  geom_text_repel(data=subset(asasec, Year == 2014 & Revenues > 10000), size = 2)

p2

#  ... and graph labels...
p3 <- p2 + labs(x="Membership",
                y="Revenues",
                color = "Section has own Journal",
                title = "ASA Sections",
                subtitle = "2014 Calendar year.",
                caption = "Source: ASA annual report.")

p3

#  ... reposition the legend.
p4 <- p3 + scale_y_continuous(labels = scales::dollar) +
  theme(legend.position = "bottom")
p4
# ================== hands-on 2 ==================

## 8.1 Use Color to Your Advantage

library(RColorBrewer)
display.brewer.all(type = "seq")
display.brewer.all(type = "div")
display.brewer.all(type = "qual")
display.brewer.all(type = "all")


# Examples of using colorbrewer

p <- ggplot(data = organdata,
            mapping = aes(x = roads, y = donors, color = world))
p + geom_point()

p + geom_point(size = 1.5) + scale_color_brewer(palette = "Set2") +
    theme(legend.position = "top")

p + geom_point(size = 1.5) + scale_color_brewer(palette = "Pastel2") +
        theme(legend.position = "top")

p + geom_point(size = 1.5) + scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "top")

# ================== hands-on 3 ==================

# Example of a custom color palette
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_comp(cb_palette)

p + geom_point() + scale_color_manual(values = cb_palette) 

p4 + scale_color_manual(values = cb_palette) 


# Extract 5 colors from ggplot()'s default palette
display.brewer.all(type = "qual")
# Clear plots
if(!is.null(dev.list())) dev.off()

display.brewer.all(type = "qual")

Default <- brewer.pal(5, "Set2")

Default

color_comp(Default)

p + geom_point() + scale_color_manual(values = cb_palette) 

p4 + scale_color_manual(values = Default) 

# Simulating different types of color blindness
if (!require("dichromat")) install.packages("dichromat")
library(dichromat)

types <- c("deutan", "protan", "tritan")
names(types) <- c("Deuteronopia", "Protanopia", "Tritanopia")

color_table <- types %>%
    purrr::map(~ dichromat(Default, .x)) %>%
    as_tibble() %>%
    add_column(Default, .before = TRUE)

color_table

color_comp(color_table)
p + geom_point() + scale_color_manual(values = color_table$Default) 
p + geom_point() + scale_color_manual(values = color_table$Deuteronopia) 
p + geom_point() + scale_color_manual(values = color_table$Protanopia) 
p + geom_point() + scale_color_manual(values = color_table$Tritanopia) 

## 8.2 Layer Color and Text Together

# Democrat Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")
color_comp(party_colors)
party_colors <- c("#0000ff","#ff0803")

color_comp(party_colors)

# ================== hands-on 4 ==================

county_data <- county_data
p0 <- ggplot(data = subset(county_data,
                           flipped == "No"),
             mapping = aes(x = pop,
                           y = black/100))

p1 <- p0 + geom_point(alpha = 0.15, color = "gray50") +
    scale_x_log10(labels=scales::comma) 

p1


# Using party_colors

p2 <- p1 + geom_point(data = subset(county_data,
                                    flipped == "Yes"),
                      mapping = aes(x = pop, y = black/100,
                                    color = partywinner16)) +
    scale_color_manual(values = party_colors)

p2

# Adding the "bells and whistles"

p3 <- p2 + scale_y_continuous(labels=scales::percent) +
    labs(color = "County flipped to ... ",
         x = "County Population (log scale)",
         y = "Percent Black Population",
         title = "Flipped counties, 2016",
         caption = "Counties in gray did not flip.")

p3


# Using geom_text_repel() on subset of data

p4 <- p3 + geom_text_repel(data =
                               subset(county_data,
                                      flipped == "Yes" &
                                      black  > 25),
                           mapping =
                               aes(x = pop,
                                   y = black/100,
                                   label = state), size = 2)

p4 + theme_minimal() + theme(legend.position="top")

