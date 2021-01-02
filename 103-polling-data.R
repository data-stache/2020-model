{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
  library(broom)
  library(knitr)
  library(dslabs)
  library(caret)
  library(radiant.data)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Master Variables

RUN_DATE <- ymd(20201103)

START_DATE <- ymd(20200415) # Biden Presumptive Nominee
election_day <- ymd(20201103)

# poll weight sample size - sqrt(sample / 600)
# poll weight recentness - Half Life = .5 ^ (days since poll / 14 days)

# 2020 Polls -------------------------------------------------------------------
# Load
all_polls <- read.csv('data/president_polls.csv')

# 2020 Polls Data Frame
all_polls <- all_polls %>%
  # Select Relevant Columns
  select(-cycle, -pollster_rating_id, -pollster_rating_name, -office_type, -seat_number, -seat_name, -created_at, -notes, -url, -candidate_id, -sponsor_ids, -candidate_name,
         -ranked_choice_reallocated, -nationwide_batch, -partisan, -sponsor_candidate, -tracking, -race_id) %>%
  # Convert Dates to Date Format
  mutate(start_date = mdy(start_date),
         end_date = mdy(end_date),
         # Merge Candidate and Party
         answer = paste(answer, candidate_party, sep = '_')) %>%
  # Drop Party Columns
  select(-candidate_party) %>%
  # Spread to One Row Per Poll
  spread(key = answer, value = pct) %>%
  # Replace NAs with 0
  mutate_at(vars(Amash_LIB:Yang_DEM), ~replace_na(., 0)) %>%
  # Select Relevant Columns from FTE Polls
  select(state, start_date, end_date, pollster, fte_grade, sample_size, population, methodology,
         Amash_LIB:Yang_DEM) %>%
  # Build All Polls Table
  mutate(spread = Trump_REP / 100 - Biden_DEM / 100,
         weight_sample = sqrt(sample_size / 600),
         weight_recent = .5 ^ ((as.numeric(RUN_DATE) - as.numeric(end_date)) / 14),
         weight_grade = case_when(fte_grade == 'A+' ~ 1,
                                  fte_grade == 'A' ~ .95,
                                  fte_grade == 'A-' ~ .93,
                                  fte_grade == 'A/B' ~ .9,
                                  fte_grade == 'B+' ~ .88,
                                  fte_grade == 'B' ~ .85,
                                  fte_grade == 'B-' ~ .83,
                                  fte_grade == 'B/C' ~ .8,
                                  fte_grade == 'C+' ~ .78,
                                  fte_grade == 'C' ~ .75,
                                  fte_grade == 'C-' ~ .73,
                                  fte_grade == 'C/D' ~ .7,
                                  fte_grade == 'D-' ~ .63,
                                  fte_grade == '' ~ .5),
         weight = weight_sample * weight_recent * weight_grade) %>%
  select(state, start_date, end_date, pollster, fte_grade, sample_size, population, weight, spread,
         Amash_LIB:Yang_DEM,
         weight_sample, weight_recent, weight_grade)

# Reorder Columns, more relevant data up front
all_polls <- all_polls[, c(1:9, 12, 54, 34, 10:11, 13:33, 35:53, 55:62)]

# Week Ending
all_polls <- all_polls %>%
  mutate(x = end_date,
         wd = weekdays(x),
         week_ending = ceiling_date(x, 'week') + ifelse(weekdays(x) %in% c('Saturday', 'Sunday'), 5, -2),
         count_down = as.numeric(end_date) - as.numeric(election_day)) %>%
  select(-x, -wd)

# Fill '' with 'National'
all_polls$state[all_polls$state == ''] <- 'National'

# Save
save(all_polls, file = 'rda/all_polls.rda')


# Build Smoothing Parameter (Needs Work) ---------------------------------------
# Train Span Paramater and Degree Parameter
# Only States that appear more than once
states <- unique(all_polls$state[duplicated(all_polls$state)])

# Train and Fit all Polled States
poll_trends <- map_df(states, function(ST) {
  # Filter State, relevant columns for smoothing
  dat <- all_polls %>%
    filter(state == ST & !is.na(weight)) %>%
    select(state, count_down, spread, weight)
  
  # Train Loess Curve - get span and degree
  train <- train(spread ~ count_down, data = dat, method = 'gamLoess')
  span <- train$results %>% .$span
  degree <- train$results %>% .$degree
  
  # Fit a model
  fit <- loess(spread ~ count_down, weights = weight, degree = degree, span = span,  data = dat)
  
  # Empty Data Frame for prediction
  data <- data.frame(state = paste(ST),
                     count_down = seq(min(dat$count_down), 0, 1))
  
  # Build smoothed Data Frame
  trend <- predict(fit, newdata = data)
  
  # Bind Frames together
  data <- data %>% cbind(trend)
})

rownames(poll_trends) <- c()

# Save
save(poll_trends, file = 'rda/poll_trends.rda')

# Visualize
all_polls %>%
  filter(count_down > -150, state == 'Florida') %>%
  ggplot(aes(x = count_down, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point(color = 'darkblue', alpha = .5) +
  geom_line(aes(x = count_down, y = trend), data = poll_trends %>% filter(count_down > -150 & state == 'Florida'), color = 'red') +
  theme_DataStache() +
  coord_flip() +
  scale_x_reverse()

# MESSY FUCKING WAY TO DO THIS - solve a way to incorporate a trend line adjustment
#all_polls %>%
#  select(state:Jorgensen_LIB, weight_sample:count_down) %>%
#  right_join(poll_trends) %>%
#  group_by(state) %>%
#  mutate(test = trend - trend[6])


# Build Poll Averages and Confidence Interval ----------------------------------

# Load Polling Error and Pull T - Dist of error
load('rda/polling_error.rda')

polling_error <- polling_error %>%
  select(state, poll_t_dist)

# Build poll results table
poll_results <- all_polls %>%
  filter(!is.na(weight)) %>%
  group_by(state) %>%
  summarize(polls_avg = weighted.mean(spread, weight),
            polls_sd = weighted.sd(spread, weight),
            polls_N = n(),
            polls_se = polls_sd / sqrt(polls_N)) %>%
  left_join(polling_error) %>%
  mutate(polls_start = polls_avg - poll_t_dist,
         polls_end = polls_avg + poll_t_dist)
  

save(poll_results, file = 'rda/poll_results.rda')


# Visualize Poll Results -------------------------------------------------------

# Poll Order
poll_ord <- poll_results %>%
  arrange(polls_avg) %>%
  .$state

poll_results %>%
  mutate(state = factor(state, levels = poll_ord)) %>%
  ggplot(aes(x = state, y = polls_avg, ymin = polls_start, ymax = polls_end)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(color = ifelse(poll_results$state == 'National', 'red', 'blue'), size = ifelse(poll_results$state == 'National', 1, .5)) +
  geom_point(color = ifelse(poll_results$state == 'National', 'red', 'blue'), size = ifelse(poll_results$state == 'National', 3, 1)) +
  coord_flip()
  
  
  
  
  
  
  
  
  
