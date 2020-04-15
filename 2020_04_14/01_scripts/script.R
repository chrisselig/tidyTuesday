#################################################################################
# Script is Used to Visualize tidytuesday data for 2020-04-14
# More information about tidytuesday can be found here:
# https://github.com/rfordatascience/tidytuesday

# Scripted created by Chris Selig Apr 2020

#################################################################################

# Import Libraries ----
library(tidyverse)
library(ggalt)
library(ggthemes)
library(ggtext)
library(cowplot)

# Get the Data ----
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Tidy Data

# My rank for song
my_rank <- 1

# To help windows recognize my custom fonts
windowsFonts(LetMeRide=windowsFont("Let Me Ride"))
windowsFonts(FuturaCondensedMedium = windowsFont("Futura Condensed"))

# Manipulate data for plotting
plot_tbl <- polls %>% 
    # Filter on song of interest
    filter(title == 'Nuthin’ But A ‘G’ Thang') %>% 
    # Select columns of interest
    select(rank,title, artist, critic_name) %>% 
    # Add my rank to tibble
    mutate(my_rank = my_rank) %>% 
    # Create gap column
    mutate(gap = rank - my_rank) %>% 
    arrange(desc(gap))


# Make the plot
plot_tbl %>% 
    ggplot(aes(x = rank, xend = my_rank, y = reorder(critic_name,gap))) +
    geom_dumbbell(
        colour = "#EBECED",
        size = 2,
        colour_x = "#657fa7",
        colour_xend = "#A99B69"
        ) +
    annotate("text", x = 1.75, y = 1.65, label = "Best Critics", color = '#738d76') +
    labs(
        title = "Nuthin But a g Thang Critic Ranks",
        subtitle = "Variance between my rank of #1 and Critic's",
        caption = "Source: https://blog.datawrapper.de/best-hip-hop-songs-of-all-time-visualized/
            \nOpinion is my own, don't @ me",
        y = "Critic",
        x = ""
        ) +
    # Use edward Tufte theme
    ggthemes::theme_tufte() +
    # change some theme settings
    theme(
        plot.title = element_text(hjust = 0.01, size = 16, family = "LetMeRide",color = '#A99B69'),
        plot.subtitle = element_text(hjust = 0.01, size = 14,family = "FuturaCondensedMedium",  face = 'bold',color = '#657fa7'),
        ) +
    # Add album cover
    draw_image(
        "https://upload.wikimedia.org/wikipedia/en/1/19/Dr.DreTheChronic.jpg",
        x = 3.5, y = 1, width = 2, height = 6
    )
