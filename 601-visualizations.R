{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
  library(knitr)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}

load('rda/state_spread_prior.rda')
load('rda/nat_spread_prior.rda')
load('rda/poll_results.rda')
load('rda/posteriors.rda')

load('rda/pres_results_2020.rda')

head(posteriors)

# Modify Data ------------------------------------------------------------------
dat_polls <- poll_results %>%
  select(state, avg = polls_avg, start = polls_start, end = polls_end) %>%
  mutate(var = 'polls')

dat_nat <- nat_spread_prior %>%
  select(state, avg = n_spread_mu, start = n_spread_start, end = n_spread_end)

dat_state <- state_spread_prior %>%
  select(state, avg = spread_mu, start = spread_start, end = spread_end) %>%
  rbind(dat_nat) %>%
  mutate(var = 'prior')

dat_post <- posteriors %>%
  select(state, avg = posterior_mean, start = posterior_start, end = posterior_end) %>%
  mutate(var = 'posterior')

ord <- pres_results_2020 %>%
  arrange(Margin_share_DvR) %>%
  .$state

dat_res <- pres_results_2020 %>%
  select(state, avg = Margin_share_DvR) %>%
  mutate(start = 0,
         end = 0,
         var = 'election')


# Build Visualizations ---------------------------------------------------------
# Vector of DEM states
blue <- pres_results_2020 %>%
  filter(Margin_share_DvR < 0) %>%
  .$state

# Vector of REP states
red <- pres_results_2020 %>%
  filter(Margin_share_DvR > 0) %>%
  .$state

# Prior vs Polls vs Actual Republican states 2020
dat_state %>%
  rbind(dat_polls) %>%
  rbind(dat_res) %>%
  rbind(dat_post) %>%
  filter(state %in% red) %>%
  mutate(var = factor(var, levels = c('prior', 'polls', 'posterior', 'election')),
         state = factor(state, levels = ord)) %>%
  ggplot(aes(x = state, y = avg, ymin = start, ymax = end, color = var)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = 'dodge') +
  geom_point(position = position_dodge(width = .9)) +
  coord_flip()

# Prior vs Polls vs Actual Democrat states 2020
dat_state %>%
  rbind(dat_polls) %>%
  rbind(dat_res) %>%
  rbind(dat_post) %>%
  filter(state %in% blue) %>%
  mutate(var = factor(var, levels = c('prior', 'polls', 'posterior', 'election')),
         state = factor(state, levels = ord)) %>%
  ggplot(aes(x = state, y = avg, ymin = start, ymax = end, color = var)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = 'dodge') +
  geom_point(position = position_dodge(width = .9)) +
  coord_flip()

# Prior vs Polls vs Actual Pick-A-State
dat_state %>%
  rbind(dat_polls) %>%
  rbind(dat_res) %>%
  rbind(dat_post) %>%
  filter(state == 'Arizona') %>%
  mutate(var = factor(var, levels = c('prior', 'polls', 'posterior', 'election')),
         state = factor(state, levels = ord)) %>%
  ggplot(aes(x = state, y = avg, ymin = start, ymax = end, color = var)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(position = 'dodge') +
  geom_point(position = position_dodge(width = .9)) +
  coord_flip()









