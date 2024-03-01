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


# Plotting 
#Doesn't play
library(viridis)
library(plotly)
library(tidyverse)

my_bubble_pop <- ggplot()+
  geom_polygon(data = World_data, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data = gapminder_mapped,
             aes(x=longitude, y=latitude, size=pop,fill=continent, frame = year, text=paste(country,":", pop)),    
             shape=21,
             color= '#F8F9F9',
             alpha=0.7,
             show.legend= FALSE) +
  scale_size(range = c(0.1,20))+
  labs(title = 'World GDP vs Population',
       x= 'longitude',
       y= 'latitude')+
  theme( text = element_text(family='Bitter'),
         plot.title = element_text (size = 15),
         plot.subtitle = element_text (size = 9, vjust = -0.5))+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = 'A')


my_bubble_gdp <- ggplot()+
  geom_polygon(data = World_data, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data = gapminder_mapped,
             aes(x=longitude, y=latitude, size=gdpPercap,fill=continent, frame = year, text=paste(country,":", gdpPercap)),  
             shape=21,
             color= '#F8F9F9',
             alpha=0.7,
             show.legend= FALSE) +
  scale_size(range = c(0.1,20))+
  labs(title = 'World GDP vs Population',
       x= 'longitude',
       y= 'latitude') +
  theme( text = element_text(family='Bitter'),
         plot.title = element_text (size = 15),
         plot.subtitle = element_text (size = 9, vjust = -0.5))+
  scale_fill_viridis(discrete = TRUE, guide = FALSE, option = 'A')

Final_plt <- subplot(My_bubble_pop, My_bubble_gdp, shareY=T)

ggplotly(Final_plt, tooltip=c('text')) %>% 
  style(hoverlabel = list(font = list(family='Bitter',color="#99A3A4", size=9))) %>%
  layout(showlegend = FALSE, font=list(family='Bitter',size=14, annotations = list(
    list(x = 0.2 , y = 1.1, text = "GDP_percap", showarrow = FALSE,xref='paper', yref='paper'),
    list(x = 0.8 , y = 1.1, text = "Population",showarrow = FALSE,xref='paper', yref='paper')))) %>%
  animation_opts(1000, redraw = FALSE) %>%
  animation_button(x = 1, xanchor = "left", y = 0) %>%
  animation_slider(currentvalue = list(prefix = "Year ", font = list(color="#38C0AD"))) 


# IF we want to make plotly first and then patchwork or subplot 

#The_plot_pop <- ggplotly(My_bubble_pop, tooltip=c('text')) %>% 
#style(hoverlabel = list(font = list(family='Bitter',color="#99A3A4", size=9))) %>%
#layout(showlegend = FALSE, font=list(family='Bitter',size=14))

#The_plot_gdp <- ggplotly(My_bubble_gdp, tooltip=c('text')) %>% 
#style(hoverlabel = list(font = list(family='Bitter',color="#99A3A4", size=9))) %>%
#layout(showlegend = FALSE, font=list(family='Bitter',size=14)) 

#The_plot_pop + The_plot_gdp
