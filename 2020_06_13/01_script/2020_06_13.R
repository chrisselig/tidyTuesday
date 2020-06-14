library(tidyverse)
library(ggtext)

# Get the Data

firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')

colors_palette <- c('#FFBC41','#067891','#A9AAB0','#EAEAEA','#BBACA3','#F2D6AE','#7C7D3D','#DF811D')
# Get random list of firsts to use for labels
#set.seed(1963823)

selected_rows <- c(1,10,22,31,69,97,113,143,149,232,260,365,399,428,479)
# Create a position for labels
position_number <- c(75,-50,15,-80,50,-20,95,15,-55,50,-15,-91,13,-45,68)
# subset_rows <- firsts %>% 
#     sample_n(size = 5, replace = TRUE)

# Subset rows 
subset_rows <- firsts[selected_rows,]

# Create label
subset_rows <- subset_rows %>% 
    # Extract name 
    mutate(name = str_extract(str_to_lower(person),'[a-z]* [a-z]*'),
           name = str_to_title(name)
           ) %>% 
    mutate(label = str_glue('{year}: {name}- \n{accomplishment}')) %>%
    
    # mutate(position = sample(-100:100, 5, replace=FALSE)) %>% 
    mutate(position = position_number)
    select(year,label, category,position)

# Build Plot
firsts_plot <- firsts %>% 
    ggplot(aes(x = year, y = 0, color = category)) +
    # Plot vertical lines
    geom_segment(data=subset_rows, 
                 aes(y=position,yend=0,xend=year, fill = category), 
                 size=0.2,
                 alpha = 0.3
                 ) +
    # Add points for select people
    geom_point(data = subset_rows, aes(year,position), color = 'black') +
    # Add points for each first
    geom_point(aes(x =  year, y = 0, color = category)) +
    geom_jitter() +
    # add horizontal line for x-axis
    geom_hline(yintercept = 0, color = 'black') +
    geom_textbox(data = subset_rows, aes(label = label, x =  year,y = position),
                 size = 3,
                 vjust = ifelse(subset_rows$position > 0,0,1),
                 fill = NA, 
                 label.color = NA,
              width = grid::unit(0.15, "npc"),
                 show.legend = FALSE) +
    scale_color_manual(values = colors_palette) +
    # Custom theme
    theme_void() +
    # Update labels
    labs(title = 'Timeline of African-American Firsts',
         subtitle = 'A very small subset of interesting firsts \n #BlackLivesMatter',
         caption = 'African American Achievements | TidyTuesday | Chris Selig'
         )+
    ylim(-115,115)+
    xlim(1720,2030) +
    # Adjust theme elements
    theme(
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(colour="#CBF8DF"),
        plot.margin=unit(c(1,0.1,1.5,1.2),"cm"),
        panel.background = element_rect(fill = "black", colour = "black"),
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(
            size = 14,
            family = "memphis",
            face = 'bold',
            color = '#A2AAB0'
        ),
        plot.subtitle = element_text(
            hjust = 0.01,
            size = 11,
            family = "Arno Pro Light Display",
            color = '#A2AAB0'
        ),
        plot.caption = element_text(color = '#A2AAB0')
    )


ggsave('2020_06_13/02_image/final.png', dpi = 600, height = 7, width =12)
