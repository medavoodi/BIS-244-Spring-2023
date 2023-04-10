## BIS 244 Chapter 8 part 2 Week 12

library(gapminder)
library(tidyverse)
library(ggrepel)
library(socviz)

# Get us back up through section 8.2 material

# Democrat Blue and Republican Red color
party_colors <- c("#2E74C0", "#CB454A")
color_comp(party_colors)
party_colors <- c("#0000ff","#ff0000")
color_comp(party_colors)

#Election results flipped counties in the US 2006 election

county_data <- county_data

# Define the environment which is x= population and y the percent black
p0 <- ggplot(data = subset(county_data,
                           flipped == "No"),
             mapping = aes(x = pop,
                           y = black/100))
# Set color gray50 for those counties without flipping to the other party
# Change the transparency alpha=0.15 
# Change the scale of X axis to log10 and use comma 
p1 <- p0 + geom_point(alpha = 0.15, color = "gray50") +
  scale_x_log10(labels=scales::comma) 
p1 
# Add flipped counties to the plot and coloring them based on the party winner
p2 <- p1 + geom_point(data = subset(county_data,
                                    flipped == "Yes"),
                      mapping = aes(x = pop, y = black/100,
                                    color = partywinner16)) +
  scale_color_manual(values = party_colors)
p2 
# Change the y scale as a percent and add texts and labels to the plot 
p3 <- p2 + scale_y_continuous(labels=scales::percent) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")
p3 
# Label all flipped counties based on their states conditionally, 
# percent black bigger than 25 
p4 <- p3 + geom_text_repel(data =
                             subset(county_data,
                                    flipped == "Yes" &
                                      black  > 25),
                           mapping =
                             aes(x = pop,
                                 y = black/100,
                                 label = state), size = 2)

p4 
# Move the legend position to the top to have more space 
p4  + theme(legend.position="top")

# "theme_minimal()": A minimalistic theme with no background annotations. 
p4 + theme_minimal() + theme(legend.position="top") 

# ====================== hands-on 1 starts =================

# theme_classic(): A classic-looking theme, with x and y axis lines and no gridlines. 

# theme_void(): A completely empty theme.

# ====================== hands-on 1 ends ====================

## 8.3 Change the Appearance of Plots with Themes
# theme_set() applies to all subsequent plots, and it remains active until 
# it is replaced by a different theme 
theme_set(theme_classic())
p4 + theme(legend.position="top")

theme_set(theme_grey())
p4 + theme(legend.position="top")
# Theme gray is the default theme for ggplot 

theme_set(theme_dark())
p4 + theme(legend.position="top")

# Install ggthemes is necessary 
if (!require("ggthemes")) install.packages("ggthemes")
# Load the ggthemes library
library(ggthemes)

# ls(): List Objects in the environment  
# List the other themes in the ggthemes package
ls(pattern = 'theme_', env = as.environment('package:ggthemes'))

theme_set(theme_economist())
p4 + theme(legend.position="top")

theme_set(theme_wsj())
p4 + theme(legend.position="top")
# the output plot is not in the good shape, so we need to revise it
p4 + theme(plot.title = element_text(size = rel(0.6)),
           legend.title = element_text(size = rel(0.35)),
           plot.caption = element_text(size = rel(0.35)),
           legend.position = "top")

# element_ functions specify the display of how non-data components of the plot are drawn.

theme_set(theme_excel_new())
p4 + theme(legend.position="top")

theme_set(theme_minimal())

# 

p4 + theme(legend.position = "top")

# Revise the theme_minimal theme
# Color, size, font type, and color of the title 
# Color, size, font type, and color of x axis

p4 + theme(legend.position = "top",
           plot.title = element_text(size=rel(2),
                                     lineheight=.5,
                                     family="Times",
                                     face="bold.italic",
                                     colour="orange"),
           axis.text.x = element_text(size=rel(1.1),
                                      family="Courier",
                                      face="bold",
                                      color="purple"))

# ====================== hands-on 2 starts =================
# Revise the theme_classic() theme
# Color of title = blue, 
# size of the title = 2, 
# font type of the title = serif  
# Font face of the title= italic ("plain", "italic", "bold", "bold.italic") 

# Color, size, font type, and color of x axis
# Color of x axis = orange, 
# size of the title = 1.2, 
# font type of the title = mono  
# Font face of the title= bold.italic ("plain", "italic", "bold", "bold.italic") 


# ====================== hands-on 2 ends =================

## 8.4 Use Theme Elements in a Substantive Way


# gss_lon dataset: General Social Survey data, 1972-2016
gss_lon <- gss_lon
# Determine which years a survey has been conducted
levels(as.factor(gss_lon$year))
# Only consider these years 1972 1976 1980 1984 1988 1993 1996 2000 2004 2008 2012 2016
yrs <- c(seq(1972, 1988, 4), 1993, seq(1996, 2016, 4))

mean_age <- gss_lon %>%
    filter(age %nin% NA && year %in% yrs) %>%
    group_by(year) %>%
    summarize(xbar = round(mean(age, na.rm = TRUE), 0))
# Create a new column, all values equal to 0.3 to place mean age on plots 
mean_age$y <- 0.3
# Create a new dataframe to place the year name at upper right of each plot 
# x = 85 and y = 0.8 are used for the year's cordinates
yr_labs <- data.frame(x = 85, y = 0.8,
                      year = yrs)

# Define environment only for specified years and age in the x axis
p <- ggplot(data = subset(gss_lon, year %in% yrs),
            mapping = aes(x = age))
# Use Geom_density to draw age distribution for each year
# Vertical lines to show the mean age for each year 
# The mean values are depicted about the white line 
# No major or minor grid lines 
# scale the y-axis between zero and one 
p1 <- p + geom_density(fill = "gray20", color = FALSE,
                       alpha = 0.9, mapping = aes(y = ..scaled..)) +
    geom_vline(data = subset(mean_age, year %in% yrs),
               aes(xintercept = xbar), color = "white", size = 0.5) +
    geom_text(data = subset(mean_age, year %in% yrs),
              aes(x = xbar, y = y, label = xbar), nudge_x = 7.5,
              color = "white", size = 3.5, hjust = 1) +
    geom_text(data = subset(yr_labs, year %in% yrs),
              aes(x = x, y = y, label = year)) +
    facet_grid(year ~ .)

p1 

p1 + theme_minimal() +
    theme(plot.title = element_text(size = 16),
          axis.text.x= element_text(size = 12),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = "Age",
         y = NULL,
         title = "Age Distribution of\nGSS Respondents")

# Same graph, but using ggridges package to allow facets to overlap vertically

library(ggridges)
# Order years as descending in the environment, 
# factor(year, levels = rev(unique(year)), ordered = TRUE)

p <- ggplot(data = subset(gss_lon, year %in% yrs),
            mapping = aes(x = age, y = factor(year, levels = rev(unique(year)), 
                                              ordered = TRUE)))

p + geom_density_ridges(alpha = 0.6, fill = "lightblue", scale = 1.5) +
    scale_x_continuous(breaks = c(20, 40, 60, 80)) +
    scale_y_discrete(expand = c(0.01, 0)) + 
    labs(x = "Age", y = NULL,
         title = "Age Distribution of\nGSS Respondents") +
    theme_ridges() +
    theme(title = element_text(size = 16, face = "bold"))
# expand argument in scale_y_discrete() adjusts the scaling of the y-axis slightly
# distance between the axis labels and the first distribution
# ====================== hands-on 3 starts =================
# if we want to order y axis as ascending order, revise the above code

# ====================== hands-on 3 ends =================


## Case Studies: Two y-axes
fredts <- fredts
str(fredts)
head

# Gather function create two columns named "series" and "score" and 
# using sp500_i:monbase_i columns to split them on those columns
fredts_m <- fredts %>% select(date, sp500_i, monbase_i) %>%
    gather(key = "series", value = "score", sp500_i:monbase_i)

head(fredts_m)

p <- ggplot(data = fredts_m,
            mapping = aes(x = date, y = score,
                          group = series,
                          color = series))
p1 <- p + geom_line() + theme(legend.position = "top") +
    labs(x = "Date",
         y = "Index",
         color = "Series")

p <- ggplot(data = fredts,
            mapping = aes(x = date, y = sp500_i - monbase_i))

p2 <- p + geom_line() +
    labs(x = "Date",
         y = "Difference")

cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(0.75, 0.25), align = "v")



## Case Studies: Redrawing a bad slide

head(yahoo)

p <- ggplot(data = yahoo,
            mapping = aes(x = Employees, y = Revenue))
p + geom_path(color = "gray80") +
    geom_text(aes(color = Mayer, label = Year),
              size = 3, fontface = "bold") +
    theme(legend.position = "bottom") +
    labs(color = "Mayer is CEO",
         x = "Employees", y = "Revenue (Millions)",
         title = "Yahoo Employees vs Revenues, 2004-2014") +
    scale_y_continuous(labels = scales::dollar) +
    scale_x_continuous(labels = scales::comma)


p <- ggplot(data = yahoo,
            mapping = aes(x = Year, y = Revenue/Employees))

p + geom_vline(xintercept = 2012) +
    geom_line(color = "gray60", size = 2) +
    annotate("text", x = 2013, y = 0.44,
             label = " Mayer becomes CEO", size = 2.5) +
    labs(x = "Year\n",
         y = "Revenue/Employees",
         title = "Yahoo Revenue to Employee Ratio, 2004-2014")



## Case Studies: Student debt

head(studebt)

p_xlab <- "Amount Owed, in thousands of Dollars"
p_title <- "Outstanding Student Loans"
p_subtitle <- "44 million borrowers owe a total of $1.3 trillion"
p_caption <- "Source: FRB NY"

f_labs <- c(`Borrowers` = "Percent of\nall Borrowers",
            `Balances` = "Percent of\nall Balances")

p <- ggplot(data = studebt,
            mapping = aes(x = Debt, y = pct/100, fill = type))
p + geom_bar(stat = "identity") +
    scale_fill_brewer(type = "qual", palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    guides(fill = FALSE) +
    theme(strip.text.x = element_text(face = "bold")) +
    labs(y = NULL, x = p_xlab,
      caption = p_caption,
      title = p_title,
      subtitle = p_subtitle) +
    facet_grid(~ type, labeller = as_labeller(f_labs)) +
    coord_flip()


library(viridis)

p <- ggplot(studebt, aes(y = pct/100, x = type, fill = Debtrc))
p + geom_bar(stat = "identity", color = "gray80") +
  scale_x_discrete(labels = as_labeller(f_labs)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = TRUE) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  labs(x = NULL, y = NULL,
       fill = "Amount Owed, in thousands of dollars",
       caption = p_caption,
       title = p_title,
       subtitle = p_subtitle) +
  theme(legend.position = "top",
        axis.text.y = element_text(face = "bold", hjust = 1, size = 12),
        axis.ticks.length = unit(0, "cm"),
        panel.grid.major.y = element_blank()) +
  coord_flip()


p0 <- ggplot(data = asasec,
             mapping = aes(x = Year, y = Members, label = Sname, group = Sname)) 
p1 <- p0 + geom_line() 

p1 + facet_wrap( ~ reorder(Sname, -Members), ncol = 11) +
  scale_x_continuous(breaks = c(2006, 2010, 2014)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(size = rel(0.65))) +
  labs(x = "")



