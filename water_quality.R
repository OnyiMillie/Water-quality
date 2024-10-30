#Obtained data from https://www.kaggle.com/datasets/patricklford/water-and-air-quality

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(hrbrthemes)
library(ggbump)
library(knitr)

Cities  <- read_csv("Water_quality/Cities1.csv")

# Check out the data and what it contains
dim(Cities)

colnames(Cities)
# What unique countries are contained therein
unique(Cities$Country)

# I am interested in the data from the Nordic countries
# Denmark
# Faroe Islands
# Finland
# Greenland
# Iceland
# Norway
Nordic_cities <- list( "Denmark",
                        "Faroe Islands",
                       "Finland",
                       "Greenland",
                       "Iceland",
                       "Norway",
                       "Sweden")
 # Subset the data to include only those from the Nordic Countries
Nordic_data <- Cities %>%
  filter(Country %in% Nordic_cities) %>%
  # All the cumeric datasets are in 5 decimal places. Round that to no decimal place
  mutate_if(is.numeric,round)


# Which Nordic countries were listed 
unique(Nordic_data$Country)
 # Only Norway, Finland and Iceland are contained in the data



# Plot the water quality and the air quality in the countries
# Air quality varies from 0 (bad quality) to 100 (top good quality).

# Water pollution varies from 0 (no pollution) to 100 (extreme pollution).

Air <- Nordic_data %>%
  group_by(Country) %>%
  summarise(av_air_quality = mean(AirQuality)) %>%
  # arrange(desc(av_air_quality)) %>%
  ggplot(aes(x = reorder(Country, av_air_quality), y = av_air_quality, fill = Country)) +
  geom_col()+
  ggtitle("Average air quality in Nordic countries") +
  xlab("Country")+
  ylab("Air quality in percentage")+
  theme(plot.title = element_text(hjust = 0.5))
  

 # Visualize water quality in same way
# This decribes how polluted the water in these countries are
Water <- Nordic_data %>%
  group_by(Country) %>%
  summarise(av_water_quality = mean(WaterPollution)) %>%
  ggplot(aes(x = reorder(Country, av_water_quality), y = av_water_quality, fill = Country)) +
  geom_col()+
  ggtitle("Average water quality in Nordic countries") +
  xlab("Country")+
  ylab("Water pollution in percentage")+
  theme(plot.title = element_text(hjust = 0.5))

# Iceland appears to have the most polluted water bodies. Followed by Norway

ggarrange(Air, Water, common.legend = T, legend = "bottom")




# Visualize Norway data alone
Norway <- Nordic_data %>%
  filter(Country == "Norway")

 # Visualize air and water quality in the cities in Norway 
 Nor_wat <- Norway %>%
  arrange(desc(WaterPollution)) %>%
  ggplot(aes(x = reorder(City, WaterPollution), y = WaterPollution)) +
  geom_col(fill = "#3333FF", color = "#000000", linewidth = 0.75) +
  labs(title = "Water pollution rates in Norwegian cities",
       x = NULL,
       y = "Average Water Pollution Score") +
  theme(plot.title = element_text( hjust = 0.5)) +
  coord_flip()



# Repeat for air quality
Nor_air <- Norway %>%
  arrange(desc(AirQuality)) %>%
  ggplot(aes(x = reorder(City, AirQuality), y = AirQuality)) +
  geom_col(fill = "#3333FF", color = "#000000", linewidth = 0.75) +
  labs(title = "Air quality in Norwegian cities ",
       x = NULL,
       y = "Average Air Quality Score") +
  theme(plot.title = element_text( hjust = 0.5)) +
  coord_flip()

plot_grid(Nor_wat, Nor_air)


# Take a look at the WHO data
WHO <- read_csv("Water_quality/WHO_PM.csv")
dim(WHO)
colnames(WHO)

unique(WHO$IndicatorCode)
# Multi-dimensional data, some redundant
# From the description on the kaggle website, the location column tells the country

unique(WHO$Location)
# Using the same list of Nordic countries, subset the data to those from Nordic countries

who_Nordic_data <- WHO %>%
  filter(Location %in% Nordic_cities) %>%
  # Too many columns, select only those of interest
  # Indicator gives the pollutant type
  # Location gives country name
  # Period gives year
  # Dim1 specifies if its urban or rural area
  # FactValueNumeic gives the value 
  # Value gives the Value with the highest and lowest range
  select(Indicator, Location, Period, Dim1,FactValueNumeric, Value) %>%
  # Contains the total values in a year. Filter it out
  filter(Dim1 != "Total")



who_Nordic_data %>%
  ggplot(aes(x = Period, y = FactValueNumeric, fill= Location, colour = Dim1))+
  geom_area()+
  geom_line()+
  ylab("Value of particulate matter in air")+
  theme_ipsum()

# Visualize using a bump plot
who_Nordic_data %>%
  ggplot(aes(x = Period, y = FactValueNumeric, group = Location)) +
  geom_bump(linewidth = 0.6, color = "gray90", smooth = 6) +
  geom_bump(aes(color = Location), linewidth = 0.8, smooth = 6)
