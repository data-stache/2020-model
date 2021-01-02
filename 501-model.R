{# Libraries
  library(tidyverse)
  library(tidylog)
  library(SuppDists)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Load Data
load('rda/posteriors.rda')
load('rda/electoral_votes.rda')

# Attach Electoral Votes
posteriors <- posteriors %>%
  left_join(electoral_votes)

# Set Variables
B <- 40000 # Iterations
N <- length(posteriors$state) # Count of States (plus National)

# Build Empty Results Matrix
results_matrix <- matrix(0, nrow = N, ncol = B)

# Build list for T Distribution Curve
dist <- list(
  gamma = 0,
  delta = 0.5, # delta 1 = normal distributions - adjust this parameter to 1 as the election draws near
  xi = 0.01,
  lambda = 1,
  type = "SN"
)

# Select Distributions
dist_multiplier <- rJohnson(B, dist)

# Run Simulation
for(i in 1:N){
  results_matrix[i, ] <- posteriors[i, 4] + dist_multiplier * posteriors[i, 5]
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

# Build this out - National vote / Electoral vote / Ties etc

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
  mutate(Percent = scales::percent(Percent/B, accuracy = 1.1))

# Other ------------------------------------------------------------------------
# Load 2020 actuals
load('rda/pres_results_2020.rda')
head(pres_results_2020)

# Attach National Spreads
dat_nat <- pres_results_2020 %>%
  summarise(biden = sum(Biden_DEM_vote),
            trump = sum(Trump_REP_vote),
            other = sum(Jorgensen_LIB_vote, Hawkins_GRN_vote, Other_vote),
            total = sum(biden, trump, other),
            biden_sh = biden / total,
            trump_sh = trump / total,
            actual = trump_sh - biden_sh) %>%
  mutate(state = 'National') %>%
  select(state, actual)
dat_res <- pres_results_2020 %>%
  select(state, actual = Margin_share_DvR) %>%
  rbind(dat_nat)

# Display forecast vs Actual
posteriors %>%
  select(state, biden_prob, trump_prob, ev, posterior_mean) %>%
  left_join(dat_res) %>%
  mutate(posterior_mean = round(posterior_mean, digits = 4),
         actual = round(actual, digits = 4),
         proj_winner = ifelse(biden_prob > trump_prob, 'Biden', 'Trump'),
         proj_winner = ifelse(biden_prob > trump_prob, paste(proj_winner, ' +', abs(posterior_mean * 100), sep = ''), paste(proj_winner, ' +', posterior_mean * 100, sep = ''))) %>%
  arrange(actual) %>%
  kable()

