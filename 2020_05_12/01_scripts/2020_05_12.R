#################################################################################
# Script is Used to Visualize tidytuesday data for 2020-05-12
# More information about tidytuesday can be found here:
# https://github.com/rfordatascience/tidytuesday

# Scripted created by Chris Selig May 2020

#################################################################################

library(tidyverse)
library(ggrepel)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
#events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


# Count number of eruptions
eruption_counts <- eruptions %>% 
    mutate(
        # Create date columns for start and end of eruptions
        start_date = as.Date(with(eruptions, paste(start_year, start_month, start_day,sep="-")), "%Y-%m-%d"),
        end_date = as.Date(with(eruptions, paste(end_year,end_month,end_day, sep = '-')),'%Y-%m-%d'),
        # count days between beginning and end
        length_of_eruption = as.numeric(difftime(end_date,start_date, units = 'days'))
        #start_date = as.Date(start_year,start_month,start_day, origin = '1900-01-01')
    ) %>% 
    filter(eruption_category == 'Confirmed Eruption') %>% 
    group_by(volcano_number) %>% 
    summarize(
        number_of_eruptions = n(),
        avg_length_of_eruptions = round(mean(length_of_eruption, na.rm = TRUE),0)) %>% 
    filter(!is.na(avg_length_of_eruptions)) 

# Combine datasets
combined <- volcano %>% 
    left_join(eruption_counts, by = c('volcano_number' = 'volcano_number')) %>% 
    select(volcano_name,number_of_eruptions, avg_length_of_eruptions) %>% 
    filter(!is.na(number_of_eruptions)) %>% 
    # remove really large values
    filter(avg_length_of_eruptions > 4 & avg_length_of_eruptions <= 10) 

# Plotting
combined %>% 
    ggplot(aes(x = avg_length_of_eruptions, y = number_of_eruptions, label = volcano_name)) +
    geom_point(aes(size = number_of_eruptions, color = number_of_eruptions)) +
    geom_curve(data = combined %>% filter(avg_length_of_eruptions >= 8),
               aes(x = avg_length_of_eruptions, 
                    y = 0, 
                    xend = avg_length_of_eruptions, 
                    yend = number_of_eruptions, 
                    color = number_of_eruptions
                   ), 
               curvature = 1,
               angle = 25)+
    geom_curve(data = combined %>% 
                   filter(avg_length_of_eruptions < 8),
               aes(x = avg_length_of_eruptions, 
                   y = 0, 
                   xend = avg_length_of_eruptions, 
                   yend = number_of_eruptions, 
                   color = number_of_eruptions/6
                   ), 
               curvature = -1,
               angle = 25)+
    scale_colour_gradient2(low = "#F6BDC0", mid = '#F07470',high = "#CF1020") +
    # labels
    labs(
        x = 'Avg Length of Eruptions (in days)',
        y = 'Number of Eruptions',
        title = 'Number of Eruptions per Average Length of Eruptions',
        subtitle = 'Shows Volcanos with Average Length of Eruptions between 5 and 10 Days',
        caption = 'Data: https://volcano.si.edu/ | @ChrisSeligYYC'
    ) +
    geom_text_repel(aes(color = number_of_eruptions),
                    segment.colour = NA
                    ) +
    theme_minimal() +
    theme(
        # panel and plot color
        panel.background = element_rect(fill = "#282C35", colour = "#282C35"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#282C35"),
        # remove legend
        legend.position = "none",
        # axis formatting
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        # title/subtitle/caption formatting
        axis.title = element_text(colour = "#F5F5F5"),
        plot.title = element_text(size = 14, family = "memphis", face = 'bold',color = '#F5F5F5'),
        plot.subtitle = element_text(hjust = 0.01, size = 11,family = "Arno Pro Light Display",color = '#F5F5F5'),
        plot.caption = element_text(color = '#F5F5F5')
        
    )

ggsave('2020_05_12/02_plot/final.png',dpi = 600)
