#################################################################################
# Script is Used to Visualize tidytuesday data for 2020-05-05
# More information about tidytuesday can be found here:
# https://github.com/rfordatascience/tidytuesday

# Scripted created by Chris Selig May 2020

#################################################################################

# Libraries
library(tidyverse)
library(tidytext)
library(scales)
library(ggthemes)

# Get the Data
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
#items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
#villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

# Process Data
# add column identifying if it is a critic or user review score
critic$reviewer <- 'critic'
user_reviews$reviewer <- 'user'

# Combine datasets and remove unwanted columns
combined_reviews <- critic %>% 
    bind_rows(user_reviews) %>% 
    select(reviewer,text) %>% 
    filter(!is.na(text))

# Tokenize Reviews 
tidy_reviews <- combined_reviews %>% 
    unnest_tokens(word, text) %>% 
    # Remove stop words
    anti_join(stop_words)

# Plot review scores
freq_tbl <- tidy_reviews %>% 
    group_by(reviewer) %>% 
    count(reviewer,word) %>% 
    mutate(proportion = n/sum(n)) %>% 
    select(-n) %>% 
    pivot_wider(
        names_from = reviewer,
        values_from = proportion
    ) %>% 
    pivot_longer(
        cols = c(critic),
        names_to = 'reviewer',
        values_to = 'proportion'
    )

freq_plot <- freq_tbl %>% 
    ggplot(aes(x = proportion, y = user, color = abs(user - proportion))) +
    # Geoms
    geom_abline(color = 'gray40', lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5,width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    # Formatting
    labs(
        title = 'Word Frequencies by Reviewers',
        subtitle = 'Compares the frequencies of words between users and critics \nfor Animal Crossing: New Horizons',
        y = 'User Reviews', 
        x = NULL,
        caption = "Source: TidyTuesday, VillagerDB, Metacritic"
    ) +
    # Change theme
    theme_tufte() +
    # Adjust scales
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient(limits = c(0,0.001),
                         low = '#CBF8DF', high = '#4C586F') +
    facet_wrap(~reviewer, ncol = 2) +
    # Adjust theme elements
    theme(
        plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
        plot.subtitle = element_text(hjust = 0.01, size = 11,family = "Arno Pro Light Display"),
        legend.position = 'none'
    )

freq_plot

Words along dotted line appears at the same frequency for both reviewers.\n
Top quadrant are words that appear more often in user reviews (i.e. console)
