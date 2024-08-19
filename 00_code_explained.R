# Load Packages
library(tidyverse) # all the necessary packages to code and plot graphs
library(patchwork) # allows graphs to be put side by side if wanted
library(readr) # reads in data
library(kableExtra) # creates nicer looking tables

# Set Seed 
set.seed(1968) # makes sure the outcome of every piece of code remains consistent every time the qmd is rendered

# Leveling Example
# Ensure that the observations in a specific variable are in this specific order no matter how it's used (in a table, graph etc.)
entry_form_data$work_hard <- factor(entry_form_data$work_hard, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))

# Counting Pre and Post Form Amounts
entry_form_data %>% # call the data set
  count(survey_type) %>% # count the observations for this specific variable
  mutate( # adding two additional variables to the counting table
    proportion = n / sum(n), ## proportion
    percent = proportion * 100 ## percent
  ) %>%
  arrange(desc(n)) %>% # arrange the count amount in decreasing order (highest to lowest)
  rename( # renaming the variable where it's "new" = "old" names (`` needs to be used if there is space in between the variable name, old or new)
    `Number of Forms` = n, 
    `Survey Type` = survey_type, 
    Proportion = proportion, 
    Percent = percent
  ) %>%
  kbl(caption = "Distribution of Pre and Post Forms") %>% # title the new looking table
  kable_styling() # make the table look nicer

# Career Amount Graph Example
## One
pre_career_amount_plot <- ggplot(data = pre_entry_form_data, mapping = aes(x = career_amount)) + 
  geom_density() + # creates a density graph
  geom_rug() + # creates tick marks at the bottom of the graph to show where most of the points are clumped in another way
  theme_minimal() + 
  labs(
    x = "Number of Careers", # x-axis title
    y = "Density", # y-axis title
    title = "Pre Test", 
    subtitle = "Unique Students", 
    caption = "Source: WiSTEM 2023-2024"
  ) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) # makes the title of the graph bold

## Two
pre_career_amount_table <- pre_entry_form_data %>%
  count(career_amount) %>%
  mutate(
    mean = mean(career_amount), # finds the mean
    max = max(career_amount), # finds the maximum value
    min = min(career_amount), # finds the minimum value
    std_dev = sd(career_amount), # finds the standard deviation
    median = median(career_amount), # finds the median
    test = "pre" # sets all the test values to "pre"
  ) %>%
  select(test, min, median, mean, std_dev, max) %>% # selecting only the variables I want to present in the specific order I want them
  head(1) # showing only the first row of the new data set

## Three
career_amount_table <- pre_career_amount_table %>%
  bind_rows(post_career_amount_table) %>% # combines this data set with the other one called by adding the rows together into a 'longer' data set
  rename(
    Type = test, 
    Min = min, 
    Median = median, 
    Mean = mean, 
    `Standard Dev` = std_dev, 
    Max = max
  )
