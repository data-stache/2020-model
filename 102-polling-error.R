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

# Read Pertinent Files
load('rda/pres_spreads_all.rda')
load('rda/nat_spreads.rda')

# DF Prep ----------------------------------------------------------------------
# Condense actuals, rename states
# Name State for National Spread National
nat_spreads <- nat_spreads %>%
  mutate(state = 'National') %>%
  select(cycle, state, D = nat_spread)

pres_spreads_all <- pres_spreads_all %>%
  rename(D = spread) %>%
  rbind(nat_spreads) %>% 
  arrange(desc(cycle)) %>%
  ungroup()

# Load Poll History ------------------------------------------------------------
# Load
poll_history <- read.csv('https://raw.githubusercontent.com/fivethirtyeight/data/master/polls/pres_pollaverages_1968-2016.csv', stringsAsFactors = FALSE, header = TRUE)

# Convert Poll Average Dates to Lubridate Format
poll_history <- poll_history %>%
  mutate(modeldate = mdy(modeldate))

# Get vector of final averages
final_models <- poll_history %>%
  select(cycle:pct_estimate, - candidate_id) %>%
  group_by(cycle) %>%
  summarize(max_date = max(modeldate)) %>%
  filter(!cycle %in% c(1968, 1972)) %>%
  .$max_date

# Filter Final Averages
poll_history <- poll_history %>%
  select(cycle:pct_estimate, - candidate_id) %>%
  filter(modeldate %in% final_models) %>%
  mutate(state = ifelse(state == 'ME-1', 'Maine CD-1', state),
         state = ifelse(state == 'ME-2', 'Maine CD-2', state),
         state = ifelse(state == 'NE-1', 'Nebraska CD-1', state),
         state = ifelse(state == 'NE-2', 'Nebraska CD-2', state),
         state = ifelse(state == 'NE-3', 'Nebraska CD-3', state))

# Cycles
cycles <- unique(poll_history$cycle)


# Build DF of Polling Error ----------------------------------------------------
# 2016
dat_errors <- poll_history %>%
  filter(cycle == cycles[1]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Donald Trump` / 100 - `Hillary Rodham Clinton` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

# 2012
dat_temp <- poll_history %>%
  filter(cycle == cycles[2]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Mitt Romney` / 100 - `Barack Obama` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2008
dat_temp <- poll_history %>%
  filter(cycle == cycles[3]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `John McCain` / 100 - `Barack Obama` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2004
dat_temp <- poll_history %>%
  filter(cycle == cycles[4]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George W. Bush` / 100 - `John Kerry` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 2000
dat_temp <- poll_history %>%
  filter(cycle == cycles[5]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George W. Bush` / 100 - `Al Gore` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1996
dat_temp <- poll_history %>%
  filter(cycle == cycles[6]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Bob Dole` / 100 - `Bill Clinton` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1992
dat_temp <- poll_history %>%
  filter(cycle == cycles[7]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George Bush` / 100 - `Bill Clinton` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1988
dat_temp <- poll_history %>%
  filter(cycle == cycles[8]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `George Bush` / 100 - `Michael S. Dukakis` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1984
dat_temp <- poll_history %>%
  filter(cycle == cycles[9]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Ronald Reagan` / 100 - `Walter F. Mondale` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1980
dat_temp <- poll_history %>%
  filter(cycle == cycles[10]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Ronald Reagan` / 100 - `Jimmy Carter` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# 1976
dat_temp <- poll_history %>%
  filter(cycle == cycles[11]) %>%
  spread(candidate_name, pct_estimate) %>%
  mutate(D_hat = `Gerald R. Ford` / 100 - `Jimmy Carter` / 100) %>%
  select(cycle, state, D_hat) %>%
  left_join(pres_spreads_all) %>%
  mutate(error = D_hat - D)

dat_errors <- dat_errors %>%
  rbind(dat_temp)

# State Error Table ------------------------------------------------------------
polling_error <- dat_errors %>%
  group_by(state) %>%
  summarize(poll_avg_error = mean(error, na.rm = TRUE),
            poll_sd_error = sd(error, na.rm = TRUE),
            poll_N_error = n(),
            poll_z = qt(0.9, poll_N_error - 1),
            poll_t_dist = poll_sd_error * poll_z) %>%
  filter(!is.na(poll_avg_error)) %>%
  mutate(cycle = 2020)

polling_error <- polling_error[, c(7, 1:6)]

polling_error %>%
  arrange(poll_t_dist) %>%
  kable()

save(polling_error, file = 'rda/polling_error.rda')


# Visualize Polling Error ------------------------------------------------------
X_hat <- -.02 # Poll says DEMs by 2%
X <- .03 # Reality REPs by 3%

X_hat - X

vec_error_ord <- polling_error %>%
  arrange(desc(poll_avg_error)) %>%
  .$state

polling_error %>%
  mutate(state = factor(state, levels = vec_error_ord),
         start = poll_avg_error - poll_t_dist,
         end = poll_avg_error + poll_t_dist) %>%
  ggplot(aes(x = state, y = poll_avg_error, ymin = start, ymax = end, label = state)) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(polling_error$poll_avg_error - polling_error$poll_t_dist), color = 'blue', size = .5) +
  geom_hline(yintercept = mean(polling_error$poll_avg_error + polling_error$poll_t_dist), color = 'blue', size = .5) +
  geom_hline(yintercept = -.04, color = 'red') +
  geom_hline(yintercept = .04, color = 'red') +
  geom_errorbar() +
  geom_point() +
  geom_text(aes(y = start), hjust = 1, nudge_y = -.02) +
  scale_y_continuous(breaks = seq(-1,1,.02)) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))


dat_errors %>%
  left_join(polling_error) %>%
  mutate(start = poll_avg_error - poll_t_dist,
         end = poll_avg_error + poll_t_dist) %>%
  ggplot(aes(x = cycle, y = poll_avg_error)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(y = D), color = 'darkblue') +
  geom_point(aes(y = D_hat), color = 'red') +
  geom_line(aes(y = D), color = 'darkblue') +
  geom_line(aes(y = D_hat), color = 'red') +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(. ~ state)
























