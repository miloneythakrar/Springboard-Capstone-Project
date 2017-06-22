library(gridExtra)
library(ggplot2)
library(dplyr)
library(grid)
library(readr)
library(tidyverse)
annual_us_gdp <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/annual-us-gdp-1990-2016.csv")
View(annual_us_gdp)
annual_us_gdp <- as.data.frame(annual_us_gdp)
str(annual_us_gdp)
annual_us_gdp <- dplyr::filter(annual_us_gdp, Year > 2002, Year < 2013)
plot_gdp <- ggplot() +
  geom_line(data = annual_us_gdp, mapping = aes(x = Year, y = GDP))
plot_gdp1 <- plot(annual_us_gdp)
plot_gdp2 <- boxplot(annual_us_gdp)
plot_gdp3 <- ggplot(data = annual_us_gdp) +
  geom_bar(mapping = aes(x = Year))
plot_gdp4 <- ggplot() + 
  geom_line(data = annual_us_gdp, aes (x = Year, y = GDP))
plot_gdp4
gdp <- max(annual_us_gdp$GDP)
plot_data <- annual_us_gdp %>%
  dplyr::mutate(IS_MAX = dplyr::if_else(GDP == gdp,
                                        true = TRUE, false = FALSE)) %>%
  dplyr::arrange(Year)
plot_data
line_color = "orangered3"
area_color = "gray29"
plotgdp5 <- ggplot() + 
  geom_line(data = plot_data, aes(x = Year, y = GDP),
            color = line_color, size = 3/2) +
  geom_point(data = subset(plot_data, IS_MAX == TRUE), aes (x = Year, y = GDP),
             size = 4, shape = 19, color = line_color)
plotgdp5
plotgdp9 <- plotgdp5 +
  scale_x_continuous(limits = range(plot_data$Year),
                     breaks = 2003:2012, 
                     labels = 2003: 2012) +
  scale_y_continuous(limits = range(0, 20000),
                     labels = seq.int(from = 0, to = 20000, by = 2500),
                     breaks = seq.int(from = 0, to = 20000, by = 2500)) +
  theme_minimal() +
  theme(axis.title = element_text(color = "gray40", face = "bold", size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", size = 1/3),
        axis.text = element_text(color = "gray40", face = "bold", size = 10),
        plot.margin = unit(c(4/3, .5, 1, .5), units = "lines"))
  
plotgdp6
plotgdp5
plotgdp6
plotgdp7
plotgdp8
plotgdp6
plotgdp7
plotgdp9
grob_plot <- ggplot_gtable(ggplot_build(plotgdp9))
grob_plot$layout$clip[grob_plot$layout$name == "panel"] <- "off"
grid.arrange(grob_plot)
g_title <- textGrob(
  label = "US GDP has steadily increased between 2003 and 2012.", 
  x = unit (0.5, "lines"),
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(
    fontsize = 16,
    fontface = "bold"
  ))
gg <- arrangeGrob(grob_plot, top = g_title)
grid.arrange(gg)

line_area_plot <- function(input_df,
                           x_axis_var,
                           y_axis_var,
                           include_area_geom = FALSE, 
                           line_color = "gray60",
                           area_color = "gray80")
{
  input_df$x_axis_var <- input_df[,x_axis_var]
  input_df$y_axis_var <- input_df[,y_axis_var]
  
  if (include_area_geom)
  {
    line_plot <- ggplot() +
      geom_area(data = )
  }}


