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


# Load Poll Data
all_polls <- read.csv('data/president_polls.csv')

# Transform Poll Data
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
  mutate(spread = Trump_REP / 100 - Biden_DEM / 100)

# Load Polling Error
load('rda/polling_error.rda')


# RUN ITERATIONS ---------------------------------------------------------------
# Master Variables
DATES <- seq(as.Date(ymd(20200415)), as.Date(ymd(20201103)), 'day')
election_day <- ymd(20201103)

# TESTS
# D <- ymd(20201001)

poll_results_over_time <- map_df(DATES, function(D) {
  RUN_DATE <- D
  election_day <- ymd(20201103)

  # 2020 Polls -----------------------------------------------------------------
  # 2020 Polls Data Frame
  all_polls <- all_polls %>%
    filter(end_date <= RUN_DATE) %>%
    # Build All Polls Table
    mutate(weight_sample = sqrt(sample_size / 600),
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


# Build Poll Averages and Confidence Interval ----------------------------------
polling_error <- polling_error %>%
  select(state, poll_t_dist)

# Build poll results table
all_polls %>%
  filter(!is.na(weight)) %>%
  group_by(state) %>%
  summarize(polls_avg = weighted.mean(spread, weight),
            polls_sd = weighted.sd(spread, weight),
            polls_N = n(),
            polls_se = polls_sd / sqrt(polls_N)) %>%
  left_join(polling_error) %>%
  mutate(polls_start = polls_avg - poll_t_dist,
         polls_end = polls_avg + poll_t_dist,
         end_date = RUN_DATE)
})



# Visulaize Polling Only Through Time ------------------------------------------
poll_results_over_time %>%
  ggplot(aes(x = end_date, y = polls_avg)) +
  geom_line() +
  facet_wrap(.~state)


# Build Posteriors Over Time ---------------------------------------------------
load('rda/state_spread_prior.rda')
load('rda/nat_spread_prior.rda')

head(state_spread_prior)
head(nat_spread_prior)
head(poll_results_over_time)

z_score <- median(state_spread_prior$spread_z)

priors_polls <- nat_spread_prior %>%
  mutate(national_cor = 1) %>%
  rename(spread_mu = n_spread_mu, 
         spread_se = n_spread_se,
         spread_t_dist = n_spread_t_dist,
         spread_start = n_spread_start,
         spread_end = n_spread_end,
         spread_2016 = n_spread_2016) %>%
  rbind(state_spread_prior %>% select(-spread_z))

priors_polls <- poll_results_over_time %>%
  left_join(priors_polls) %>%
  select(-spread_start, -spread_end, -polls_start, -polls_end)

national_adjustments <- priors_polls %>%
  filter(state == 'National') %>%
  mutate(nat_adjust = polls_avg - spread_2016) %>%
  select(end_date, nat_adjust)

priors_polls <- priors_polls %>%
  left_join(national_adjustments)

priors_polls <- priors_polls %>%
  mutate(nat_adjust = nat_adjust * national_cor,
         polls_avg = ifelse(is.na(polls_avg), nat_adjust + spread_2016, polls_avg),
         poll_t_dist = ifelse(is.na(poll_t_dist), max(poll_t_dist, na.rm = TRUE), poll_t_dist))


# RUN ITERATIONS ---------------------------------------------------------------
# TESTS
# D <- ymd(20201001)

bias_sd <- 0.025
posteriors_over_time <- map_df(DATES, function(D) {
  posteriors <- priors_polls %>%
    filter(end_date == D) %>%
    mutate(sigma = sqrt(poll_t_dist^2 + bias_sd^2),
           B = sigma^2 / (sigma^2 + spread_t_dist^2),
           posterior_mean = B * spread_mu + (1 - B) * polls_avg,
           posterior_se = sqrt(1 / (1 / sigma^2 + 1 / spread_t_dist^2)),
           posterior_z = z_score,
           posterior_t_dist = posterior_z * poll_t_dist,
           posterior_start = posterior_mean - posterior_t_dist,
           posterior_end = posterior_mean + posterior_t_dist) %>%
    select(state, end_date, sigma:posterior_end)
})

posteriors_over_time %>%
  ggplot(aes(x = end_date, y = posterior_mean)) +
  geom_line() +
  facet_wrap(.~state)


# Simulate Electoral College ---------------------------------------------------
# Load Data
load('rda/electoral_votes.rda')

# Attach Electoral Votes
posteriors_over_time <- posteriors_over_time %>%
  left_join(electoral_votes)

# RUN ITERATIONS ---------------------------------------------------------------
# TESTS
# D <- ymd(20201001)

ec_odds <- map_df(DATES, function(D) {
  posteriors <- posteriors_over_time %>%
    filter(end_date == D)

# Set Variables
B <- 10000 # Iterations
N <- length(posteriors$state) # Count of States (plus National)

# Build Empty Results Matrix
results_matrix <- matrix(0, nrow = N, ncol = B)

# Build list for T Distribution Curve
dist <- list(
  gamma = 0,
  delta = case_when(election_day - D < 7 & election_day - D >= 0 ~ .9,
                    election_day - D < 14 & election_day - D >= 7 ~ .8,
                    election_day - D < 21 & election_day - D >= 14~ .7,
                    election_day - D >= 21 ~ .5), # delta 1 = normal distributions - adjust this parameter to 1 as the election draws near
  xi = 0.01,
  lambda = 1,
  type = "SN"
)

# Select Distributions
dist_multiplier <- rJohnson(B, dist)

posteriors <- as.data.frame(posteriors)

# Run Simulation
for(i in 1:N){
  results_matrix[i, ] <- posteriors[i, 5] + dist_multiplier * posteriors[i, 6]
}

# P of Trump Wins
trump_wins <- ifelse(results_matrix > 0, 1, 0)
trump_state_probs <- apply(trump_wins, 1, sum)/B

# Attach to Posteriors Table
posteriors$trump_prob <- trump_state_probs

# P of Biden Wins
biden_wins <- ifelse(results_matrix < 0, 1, 0)
biden_state_probs <- apply(biden_wins, 1, sum)/B

# Attach to Posteriors Table
posteriors$biden_prob <- biden_state_probs

# Make EV Matrix
votes <- matrix(posteriors$ev, ncol = 1)

# Count Electoral Votes
for(i in 1:10000){
  trump_wins[ ,i] <- trump_wins[ ,i] * votes
  biden_wins[ ,i] <- biden_wins[ ,i] * votes
}

# Sum Electoral Votes
trump_votes <- apply(trump_wins, 2, sum, na.rm = TRUE)
biden_votes <- apply(biden_wins, 2, sum, na.rm = TRUE)

# Results Data Frame
results <- data.frame(trump_votes, biden_votes)

# Label Winner
results <- results %>% 
  mutate(winner = ifelse(trump_votes >= biden_votes, "Trump", "Biden"))

# Build a table of wins
win_table <- results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(percent = n) %>% 
  mutate(percent = percent/B)

# Display table of wins
results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>%   
  mutate(Percent = round(Percent/B, digits = 3),
         End_Date = D)
})



# RUN ITERATIONS STATES---------------------------------------------------------------
# TESTS
# D <- ymd(20201001)

state_odds <- map_df(DATES, function(D) {
  posteriors <- posteriors_over_time %>%
    filter(end_date == D)
  
  # Set Variables
  B <- 10000 # Iterations
  N <- length(posteriors$state) # Count of States (plus National)
  
  # Build Empty Results Matrix
  results_matrix <- matrix(0, nrow = N, ncol = B)
  
  # Build list for T Distribution Curve
  dist <- list(
    gamma = 0,
    delta = case_when(election_day - D < 7 & election_day - D >= 0 ~ .9,
                      election_day - D < 14 & election_day - D >= 7 ~ .8,
                      election_day - D < 21 & election_day - D >= 14~ .7,
                      election_day - D >= 21 ~ .5), # delta 1 = normal distributions - adjust this parameter to 1 as the election draws near
    xi = 0.01,
    lambda = 1,
    type = "SN"
  )
  
  # Select Distributions
  dist_multiplier <- rJohnson(B, dist)
  
  posteriors <- as.data.frame(posteriors)
  
  # Run Simulation
  for(i in 1:N){
    results_matrix[i, ] <- posteriors[i, 5] + dist_multiplier * posteriors[i, 6]
  }
  
  # P of Trump Wins
  trump_wins <- ifelse(results_matrix > 0, 1, 0)
  trump_state_probs <- apply(trump_wins, 1, sum)/B
  
  # Attach to Posteriors Table
  posteriors$trump_prob <- trump_state_probs
  
  # P of Biden Wins
  biden_wins <- ifelse(results_matrix < 0, 1, 0)
  biden_state_probs <- apply(biden_wins, 1, sum)/B
  
  # Attach to Posteriors Table
  posteriors$biden_prob <- biden_state_probs
  
  return(posteriors)
})

state_odds <- state_odds %>%
  rename(trump = trump_prob, biden = biden_prob) %>%
  gather(candidate, odds, trump:biden)

# Visulaizations ---------------------------------------------------------------
party_col <- c('darkblue', 'red')

ec_odds %>%
  ggplot(aes(x = End_Date, y = Percent, color = winner)) +
  geom_line() +
  scale_color_manual(values = party_col)

state_odds %>%
  ggplot(aes(x = end_date, y = odds, color = candidate)) +
  geom_line() +
  scale_color_manual(values = party_col) +
  facet_wrap(. ~ state)
