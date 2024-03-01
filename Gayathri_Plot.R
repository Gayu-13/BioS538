
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
