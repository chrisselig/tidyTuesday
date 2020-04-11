#################################################################################
# Script is Used to Visualize tidytuesday data for 2020-04-07
# More information about tidytuesday can be found here:
    # https://github.com/rfordatascience/tidytuesday

# Scripted created by Chris Selig Apr 2020

#################################################################################

# Import Libraries ----
library(tidyverse)
library(lubridate)
library(glue)
library(gt)


# Load Data ----
winners <- read.csv("00_data/tdf_winners.csv",encoding = "ISO-8601")

# Transform Data ----


# Turn winners into a tibble for plotting

winners_gt <- winners %>% 
    
    # Select the columns for the table
    select(winner_name,start_date,distance,time_margin, stage_wins, age) %>% 
    
    # Put years into decades
    mutate(decade = year(start_date) - year(start_date) %% 10,
            decade = glue("{decade}s", sep = "")) %>% 
    
    # Remove start_date
    select(-start_date) %>% 
    # Rename columns for plotting
    rename(
        `Total Distance` = distance,
        `Time Margin` = time_margin,
        `# of Stage Wins` = stage_wins,
        Age = age
    ) %>% 
    
    # Create the gt object so I can plot
    gt(rowname_col = "winner_name", groupname_col = "decade") %>% 
    fmt_missing(columns = everything()) 
    

# Create list of summary metrics to be used in table
fns_labels <- list(
    avg = ~mean(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    IQR = ~IQR(., na.rm = TRUE)
)

# Build a table ----

    winners_gt %>%
    # Create header
    tab_header(
        title = "Tour de France Winners",
        subtitle = "Summary Statistics by Decade"
    ) %>%
    # Create source note
    tab_source_note(
        source_note = "Source: Kaggle, https://www.kaggle.com/jaminliu/a-brief-tour-of-tour-de-france-in-numbers/data"
    ) %>% 
    # Format numbers to remove decimal places
    fmt_number(
        columns = vars(`Total Distance`, Age, `# of Stage Wins`),
        decimals = 0
    ) %>%
    # Format numbers
    fmt_number(
        columns = vars(`Time Margin`),
        decimals = 3
    ) %>%
    # Create summary rows with basic metrics
    summary_rows(
        groups = TRUE,
        columns = vars(`Time Margin`, `# of Stage Wins`, Age),
        fns = fns_labels,
        formatter = fmt_number,
        decimals = 2
    ) %>% 
    # Change summary row background color
    tab_options(
        summary_row.background.color = "#EBECED"
    ) %>%
    # Title Styling
    tab_style(
        style = list(
            cell_text(
                font = "memphis",
                color = "#4C586F")
        ),
        locations = cells_title(groups = c("title"))
    ) %>% 
    # Change font of different table areas
    tab_style(
        style = list(
            cell_text(font = "Arno Pro Light Display")
        ),
        locations = list(
            cells_body(columns = TRUE, rows = TRUE),
            cells_summary(groups = TRUE, columns = TRUE, rows = TRUE),
            cells_column_labels(columns = vars(`Time Margin`, `# of Stage Wins`, Age)),
            cells_row_groups()
        )
    ) %>% 
    tab_options(
        # Remove top border
        table.border.top.color = "white",
        # Change the style of horizontal lines (hlines)
        table_body.hlines.style = "dashed",
        # Change the color of the hlines
        table_body.hlines.color = '#CBF8DF'
    )

