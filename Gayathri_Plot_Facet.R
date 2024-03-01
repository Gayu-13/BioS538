
#Doesn't play
library(viridis)
library(plotly)
library(tidyverse)

# Data_prep

# downloading the required libraries
library(maps)
library(gganimate)

# Getting coordinate data from the maps library for the world map bg
World_data <- map_data("world")

# Filtering out lat and long of just the countries in our gapminder dataset
World_data_gapminder <- world.cities %>% 
  filter(country.etc %in% gapminder$country)

# The dataset has several coordinate values to exactly map out regions, for our purposes just a rough average location works 

# Finding the average latitude position of each country 
lat_summarised <- World_data_gapminder %>% 
  group_by(country.etc) %>%
  summarise(Avg_lat = mean(lat))

# Finding the average longitude position of each country 
long_summarised <- World_data_gapminder %>% 
  group_by(country.etc) %>%
  summarise(Avg_long = mean(long))

# I realised that there was a naming mismatch which meant I could only extract coordinates for 131/142 countries in gapminder. I am filtering out just those countries for the plot now. 
gapminder_filtered <- gapminder %>%
  filter(country %in% lat_summarised$country.etc)

# Matching the country value in each row with the found averages and creating two new columns storing these average latitude and longitude values  

# Initializing dummy vectors
dummy_lat <- c()
dummy_long <- c()

for (cy in gapminder_filtered$country){ # go over every country in gapminder
  dummy_lat <- append(dummy_lat,lat_summarised$Avg_lat[match(cy,lat_summarised$country.etc)]) # match country on the average latitude dataset and add to dummy column
  dummy_long <- append(dummy_long,long_summarised$Avg_long[match(cy,long_summarised$country.etc)]) # match country on the average longitude dataset and add to dummy column
}

# Adding those coordinate_matched new columns to our dataset using mutate
gapminder_mapped <- gapminder_filtered %>%
  mutate(latitude = dummy_lat) %>%
  mutate(longitude = dummy_long)

# viewing the final dataset
head(gapminder_mapped)

gapminder_mapped_scaled <- gapminder_mapped %>%
  mutate(scaled_pop= pop/10000)
gapminder_mapped_pivoted <- pivot_longer(gapminder_mapped_scaled, cols = c(gdpPercap,scaled_pop), names_to='measures', values_to = 'values')

facet_plot <- ggplot() + 
  geom_polygon(data = World_data, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3)+
  geom_point(data = gapminder_mapped_pivoted, aes(x=longitude, y=latitude, size=values, fill=continent, frame = year),    
          shape=21,
          color= '#F8F9F9',
          alpha=0.7) +
  scale_size(range = c(0.1,15))+
  labs(title = 'World: GDP vs Population') +
  theme( text = element_text(family='Bitter'),
         plot.title = element_text (size = 15),
         plot.subtitle = element_text (size = 9, vjust = -0.5)) + 
facet_wrap(~measures, scales = 'free')

ggplotly(facet_plot) %>% 
  layout(showlegend = FALSE)


