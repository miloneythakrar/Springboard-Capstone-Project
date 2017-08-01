#Load appropriate libraries
library(gridExtra)
library(ggplot2)
library(dplyr)
library(grid)
library(readr)
library(tidyverse)
library(tidyr)
#Plot 1: Annual GDP 
#Load csv file
annual_us_gdp <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/annual-us-gdp-1990-2016.csv")
#View data file to check
View(annual_us_gdp)
#Convert to data frame
plot_gdp <- as.data.frame(annual_us_gdp)
#Drop rows of irrevelant years
plot_gdp <- dplyr::filter(plot_gdp, Year > 1995)
#View data file to check
View(plot_gdp)
#Assign color to line
line_color <- "orangered3"
#Plot data
plot <- ggplot() +
  geom_line(data = plot_gdp, aes(x = Year, y = GDP),
            color = line_color, size = 3/2)
plot
#Touch up scale and theme options
plot1 <- plot +
  scale_x_continuous(limits = range(plot_gdp$Year),
                     breaks = 1996:2016,
                     labels = 1996:2016) +
  scale_y_continuous(limits = c(7500, 20000),
                     labels = seq.int(from = 7500, to = 20000, by = 2500),
                     breaks = seq.int(from = 7500, to = 20000, by = 2500)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", size = 1/3),
        axis.text = element_text(color= "gray40", face = "bold", size = 10),
        plot.margin = unit(c(4/3, 0.5, 1, 0.5), units = "lines"))
plot1

#Add Graph Title
grob_plot <- ggplot_gtable(ggplot_build(plot1))
grob_plot$layout$clip[grob_plot$layout$name == "panel"] <- "off"
grid.arrange(grob_plot)
g_title <- textGrob(
  label = "US GDP has steadily increased from 1996 to 2016.",
  x = unit(0.5, "lines"),
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(
    fontsize = 16,
    fontface = "bold"
  ))
gg <- arrangeGrob(grob_plot, top = g_title)
grid.arrange(gg)

#Plot 2: GDP growth rate
#Load csv file
gdp_growth <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/GDP_GrowthRate.csv")
#View data file to check
View(gdp_growth)
#Convert to data frame
plot_gdp_growth <- as.data.frame(gdp_growth)
str(gdp_growth))
#Assign line color
line_color <- "navy"
#Plot data
plot2 <- ggplot() +
  geom_line(data = plot_gdp_growth, aes(x = Year, y = GDP),
            color = line_color, size = 3/2)
plot2

#Touch up scale and theme options
plot3 <- plot2 +
  scale_x_continuous(limits = range(plot_gdp_growth$Year),
                     breaks = 1996:2016,
                     labels = 1996:2016) +
  scale_y_continuous(limits = c(-5, 5),
                     labels = seq.int(from = -5, to = 5, by = .5),
                     breaks = seq.int(from = -5, to = 5, by = .5)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", size = 1/3),
        axis.text = element_text(color= "gray40", face = "bold", size = 10),
        plot.margin = unit(c(4/3, 0.5, 1, 0.5), units = "lines"))
plot3

#Add Graph Title
grob_plot1 <- ggplot_gtable(ggplot_build(plot3))
grob_plot1$layout$clip[grob_plot1$layout$name == "panel"] <- "off"
grid.arrange(grob_plot1)
g_title1 <- textGrob(
  label = "US GDP growth rate plummets during the Great Recession of 2007 to 2009.",
  x = unit(0.5, "lines"),
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(
    fontsize = 16,
    fontface = "bold"
  ))
gg1 <- arrangeGrob(grob_plot1, top = g_title1)
grid.arrange(gg1)

#Plot 3: Female and male employment ratios
#Load appropriate libraries
library(gridExtra)
library(ggplot2)
library(dplyr)
library(grid)
library(readr)
library(tidyr)
#Read csv file downloaded from BLS website
BLS_data <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/BLS data_1976_2016.csv")
#View file to check data
View(BLS_data)
#Convert to data frame
BLS_data <- as.data.frame(BLS_data)
#Dropping rowing with missing values and assign to new file name
BLS_data1 <- drop_na(BLS_data)
#View data file to check changes
View(BLS_data1)
# Renaming columns in data file
BLS_data2 <- dplyr::rename(BLS_data1, Civilian_Population = X2, Civilian_LaborForce_Total = X3, CLF_PercentofPop = X4, CLF_TotalEmployed = X5, CLF_Employed_PercentofPop = X6, CLF_Employed_Agriculture = X7, CLF_Employed_NonAgriculture = X8, CLF_TotalUnemployed = X9, CLF_Unemployed_PercentofCLF = X10, NotInLaborForce = X11)
# Checking column names
head(BLS_data2)
View(BLS_data2)
# Renaming first column in data file
colnames(BLS_data2)[1] <- 'Year'
View(BLS_data2)
#Convert columns to numeric
BLS_data2[] <- lapply(BLS_data2, function(x) as.numeric(as.character(x)))
#Create new column for Sex; assign Male and Female to appropriate values
BLS_data2$Sex <- c(replicate(41,"Male"),replicate(41,"Female"))
View(BLS_data2)
str(BLS_data2)
#Drop rows so that only years remaining are 1996 - 2016
BLS_data2 [1:20, ]
BLS_data3 <- BLS_data2 [-(1:20), ]
View(BLS_data3)
BLS_data3 [22:41, ]
BLS_data4 <- BLS_data3 [-(22:41), ]
View(BLS_data4)
#Plot data
plot_employment <- ggplot(data = BLS_data4, aes(x= Year, y = CLF_Employed_PercentofPop, group = Sex, color = Sex)) +
  geom_line(size = 2)
plot_employment
#Create a new data frame that includes all 2016 observations
label_data <- subset(BLS_data4, Year == 2016)
#Display the data set
label_data
#Create a column named "palette" for each color. 
label_data$palette <- c("turquoise4", "gray29")
#Create another new column named "labels" for each label.
label_data$labels <- c("Female Employment Ratio", "Male Employment Ratio")
#View the updated label set. 
label_data
#Plot female and male employment ratios
plot_employment1 <- ggplot(data = BLS_data4, aes(x = Year, y = CLF_Employed_PercentofPop, group = Sex, color = Sex)) +
  geom_line(size = 2) +
  annotate("text", x = label_data$Year, y = label_data$CLF_Employed_PercentofPop, label = label_data$labels, color = label_data$palette, hjust = -0.5) +
  scale_color_manual(values = label_data$palette) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray40", size = 0.5)
  ) +
  scale_x_continuous(limits = range(BLS_data4$Year),
                     breaks = 1996:2016,
                     labels = 1996:2016) +
  scale_y_continuous(limits=c(50,75), breaks=seq(50,75,5))
plot_employment1

#Plot 4: Female to male employment ration

#Create subset data for female employment ratio
female_employment_ratio <- BLS_data4[c('Year','CLF_Employed_PercentofPop','Sex')]
View(female_employment_ratio)
female_employment_ratio <- female_employment_ratio[-c(1:21), ]
View(female_employment_ratio)
female_employment_ratio <- female_employment_ratio[-c(1,3)]
View(female_employment_ratio)
colnames(female_employment_ratio)[1] <- "Female_Employment_Ratio"
View(female_employment_ratio)
#Create subset data for male employment ratio
male_employment_ratio <- BLS_data4[c('Year','CLF_Employed_PercentofPop','Sex')]
View(male_employment_ratio)
male_employment_ratio <- male_employment_ratio[-c(22:42), ]
View(male_employment_ratio)
male_employment_ratio <- male_employment_ratio[-c(1,3)]
View(male_employment_ratio)
colnames(male_employment_ratio)[1] <- "Male_Employment_Ratio"
View(male_employment_ratio)
#Dropped year and sex columns to isolate male employment ratio
BLS_data4$female_employment_ratio <- female_employment_ratio$Female_Employment_Ratio
View(BLS_data4)
BLS_data4$male_employment_ratio <- male_employment_ratio$Male_Employment_Ratio
View(BLS_data4)
str(BLS_data4)
#Convert values from character to numeric
BLS_data5 <- within(BLS_data4, { 
  male_employment_ratio <- as.numeric(as.character(male_employment_ratio)) 
  female_employment_ratio <- as.numeric(as.character(female_employment_ratio)) 
})
View(BLS_data5)
#Create new column for female ratio/male ratio
BLS_data6 <- mutate(BLS_data5, female_to_male_ratio = female_employment_ratio/male_employment_ratio)
View(BLS_data6)
#Drop sex column
BLS_data7 <-BLS_data6[-c(12)]
View(BLS_data7)
#Drop rows to remove reductant values and years
BLS_data8 <- BLS_data7[-c(22:42), ]
View(BLS_data8)
#Assign color to line
line_color <- "navy"
#Plot data 
BLS_data9 <- transform(BLS_data8, Year=as.numeric(Year))
plot_employment_ratio <- ggplot(data = BLS_data9, aes(x= Year, y = female_to_male_ratio, group = 1)) +
  geom_line(size = 2, color = line_color)
plot_employment_ratio
#Touch up plot and scales
plot_employment_ratio1 <- plot_employment_ratio +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", size = 1/3),
        axis.text = element_text(color= "gray40", face = "bold", size = 10),
        plot.margin = unit(c(4/3, 0.5, 1, 0.5), units = "lines"))
plot_employment_ratio1
#Add Graph Title
grob_plot <- ggplot_gtable(ggplot_build(plot_employment_ratio1))
grob_plot$layout$clip[grob_plot$layout$name == "panel"] <- "off"
grid.arrange(grob_plot)
g_title <- textGrob(
  label = "US Female to Male Employment Ratio peaks during recession in 2009.",
  x = unit(0.5, "lines"),
  y = unit(0, "lines"),
  hjust = 0, vjust = 0,
  gp = gpar(
    fontsize = 16,
    fontface = "bold"
  ))
gg <- arrangeGrob(grob_plot, top = g_title)
grid.arrange(gg)

#Calculate linear regression of female to male employment ratio and GDP growth rate

#Load csv file
gdp_growth_employment_ratio <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/GDP_GrowthRate and Female Ratio.csv")
#View file to check data
View(gdp_growth_employment_ratio)
#Delete columns
gdp_growth_employment_ratio<- gdp_growth_employment_ratio[,-5]
#View file to check
View(gdp_growth_employment_ratio)
#linear regression of gdp growth and female to male employment ratio
gdp.modl <- lm(Female_to_Male_Ratio ~ Converted_GDPPercent, data = gdp_growth_employment_ratio)
summary(gdp.modl)
gdp.modl$residuals
SSE = sum(gdp.modl$residuals^2)
SSE
cor(gdp_growth_employment_ratio$Female_to_Male_Ratio,gdp_growth_employment_ratio$Converted_GDPPercent)

#Calculate linear regression of female to male labor force participation rate and GDP growth rate
#Load csv file
gdp_growth_lfpr_ratio <- read_csv("~/Desktop/Foundations of Data Science/Capstone Project/LFPR_Female_Ratio.csv")
#View file to check data
View(gdp_growth_lfpr_ratio)
#linear regression of gdp growth and female to male employment ratio
gdp.modl1 <- lm(Female_to_Male_LFPR ~ Converted_GDPPercent, data = gdp_growth_lfpr_ratio)
summary(gdp.modl1)
gdp.modl$residuals
SSE = sum(gdp.modl1$residuals^2)
SSE
cor(gdp_growth_lfpr_ratio$Female_to_Male_LFPR,gdp_growth_lfpr_ratio$Converted_GDPPercent)
