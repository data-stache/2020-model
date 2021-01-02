{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
  library(broom)
  library(knitr)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Master Variables
RUN_DATE <- ymd(20200701)

election_day <- ymd(20201103)

# Graph Colors
party_col <- c('darkblue', 'red4', 'green4')


# State LM Trends --------------------------------------------------------------

# Get state names / abbreviations
state_names <- read.csv("data/us-census-population-data.csv", stringsAsFactors = FALSE, header = TRUE)
state_names <- state_names %>% select(state, state_name) %>% rename(state = state_name, state_abb = state)

# Load Election Results 1976 - 2016
pres_results_all <- read.csv("data/potus_results_76_16_wCDs.csv", stringsAsFactors = FALSE, header = TRUE)

# Fill NAs with 0s
pres_results_all$other[is.na(pres_results_all$other)] <- 0

# Gather Table
pres_results_all <- pres_results_all %>%
  gather(party, share, dem:other) %>%
  mutate(party = factor(party, (levels = c('dem', 'rep', 'other')))) %>%
  rename(state_abb = state) %>%
  left_join(state_names) %>%
  mutate(state_cd = case_when(state_abb == 'ME-1' ~ 'Maine CD-1',
                           state_abb == 'ME-2' ~ 'Maine CD-2',
                           state_abb == 'NE-1' ~ 'Nebraska CD-1',
                           state_abb == 'NE-2' ~ 'Nebraska CD-2',
                           state_abb == 'NE-3' ~ 'Nebraska CD-3'),
         state = ifelse(is.na(state_cd), state, state_cd)) %>%
  select(cycle = year, state, state_abb, party, vote_share = share, total_votes) %>%
  arrange(cycle, state, party) %>%
  mutate(total_votes = vote_share * total_votes)

# Visualize All State Trends
pres_results_all %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = party_col) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth(method = 'lm') +
  facet_wrap(. ~ state) +
  theme(axis.text.x = element_text(angle = 90))

# Visualize One State Trend
pres_results_all %>%
  filter(state == 'Connecticut') %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = party_col) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth(method = 'lm') +
  theme(axis.text.x = element_text(angle = 90))

save(pres_results_all, file = 'rda/pres_results_all.rda')


# Visualize Spread Trends (Dem vs Rep Only)
pres_spreads_all <- pres_results_all %>%
  group_by(cycle, state) %>%
  summarize(spread = vote_share[2] - vote_share[1])

save(pres_spreads_all, file = 'rda/pres_spreads_all.rda')

# Visualize Spread Trends (Dem vs Rep Only)
pres_spreads_all %>%
  ggplot(aes(x = cycle, y = spread)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'lm') +
  coord_flip() +
  scale_x_reverse(breaks = seq(1972, 2028, 4)) +
  facet_wrap(. ~ state) +
  theme(axis.text.x = element_text(angle = 90))



# State Party Linear Models ----------------------------------------------------
# Unique States
vec_states <- unique(pres_spreads_all$state)

state_fit <- map_df(vec_states, function(ST) {
  fit <- pres_spreads_all %>%
    select(state, cycle, spread) %>%
    filter(state == ST) %>%
    lm(spread ~ cycle, data = .)
  
  data <- data.frame(state = paste(ST),
                     cycle = 2020)
  
  pred <- predict.lm(fit, data, se.fit = TRUE)
  
  data.frame(state = paste(ST),
             cycle = 2020,
             pred_spread = pred$fit,
             pred_se = pred$se.fit,
             pred_df = pred$df,
             pred_res = pred$residual.scale)
})

# Clear Row Names
row.names(state_fit) <- c()

# Prior Summary Stats
state_spread_prior <- state_fit %>%
  mutate(spread_mu = pred_spread,
         # 80% Confidence Interval, T-Test Z Score
         spread_z = qt(0.9, pred_df),
         spread_t_dist = spread_z * pred_se,
         spread_start = spread_mu - spread_t_dist,
         spread_end = spread_mu + spread_t_dist,
         cycle = 2020) %>%
  left_join(pres_spreads_all %>% filter(cycle == 2016), by = 'state') %>%
  select(cycle = cycle.x, state, spread_mu, spread_se = pred_se, spread_t_dist, spread_start, spread_end, spread, spread_z) %>%
  rename(spread_2016 = spread)

head(state_spread_prior)

save(state_spread_prior, file = 'rda/state_spread_prior.rda')

# Visualize Forecasted Spread Trends (Dem vs Rep Only)
vec_prior_ord <- state_spread_prior %>%
  arrange(desc(spread_mu)) %>%
  .$state

prior_party <- c('red4', 'purple', 'blue4')

state_spread_prior %>%
  mutate(party = case_when(spread_start > 0 & spread_end > 0 ~ 'rep',
                           spread_start < 0 & spread_end > 0 ~ 'toss',
                           spread_start < 0 & spread_end < 0 ~ 'dem'),
         party = factor(party, levels = c('rep', 'toss', 'dem')),
         state = factor(state, levels =  vec_prior_ord)) %>%
  ggplot(aes(x = state, y = spread_mu, ymin = spread_start, ymax = spread_end, color = party, label = state)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_errorbar() +
  geom_text(aes(y = spread_start), hjust = 1, nudge_y = -.02) +
  scale_color_manual(values = prior_party) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90))



# National Trends --------------------------------------------------------------

# Load Election Results 1976 - 2016
temp <- read.csv("data/potus_results_76_16_wCDs.csv", stringsAsFactors = FALSE, header = TRUE)

# Fill NAs with 0s
temp$other[is.na(temp$other)] <- 0

# National Spreads by year
nat_spreads <- temp %>%
  filter(!state %in% c('ME-1', 'ME-2', 'NE-1', 'NE-2', 'NE-3')) %>%
  mutate(dem_vote = total_votes * dem,
         rep_vote = total_votes * rep,
         other_vote = total_votes * other) %>%
  select(year, state, dem_vote, rep_vote, other_vote) %>%
  group_by(year) %>%
  summarize(dem = sum(dem_vote),
            rep = sum(rep_vote),
            other = sum(other_vote)) %>%
  mutate(total_votes = dem + rep + other,
         dem = dem / total_votes,
         rep = rep / total_votes,
         other = other / total_votes,
         nat_spread = rep - dem) %>%
  select(cycle = year, nat_spread)

save(nat_spreads, file = 'rda/nat_spreads.rda')

# Correlate States to national spread
nat_cor <- pres_spreads_all %>%
  left_join(nat_spreads) %>%
  group_by(state) %>%
  summarize(cor = cor(spread, nat_spread))

# Correlation Table
nat_cor %>%
  arrange(desc(cor)) %>%
  kable()

save(nat_cor, file = 'rda/nat_cor.rda')

# Join National Correlation with State Prior
load('rda/state_spread_prior.rda')
state_spread_prior <- state_spread_prior %>%
  left_join(nat_cor) %>%
  rename(national_cor = cor)

save(state_spread_prior, file = 'rda/state_spread_prior.rda')



# National Linear Models -------------------------------------------------------
# Unique States

nat_fit <- nat_spreads %>%
  lm(nat_spread ~ cycle, data = .)
  
data <- data.frame(cycle = 2020)
  
nat_pred <- predict.lm(nat_fit, data, se.fit = TRUE)
  
national_fit <- data.frame(state = 'National',
                           cycle = 2020,
                           pred_spread = nat_pred$fit,
                           pred_se = nat_pred$se.fit,
                           pred_df = nat_pred$df,
                           pred_res = nat_pred$residual.scale)

# Prior Summary Stats
nat_spread_prior <- national_fit %>%
  mutate(n_spread_mu = pred_spread,
         # 80% Confidence Interval, T-Test Z Score
         n_spread_z = qt(0.9, pred_df),
         n_spread_t_dist = n_spread_z * pred_se,
         n_spread_start = n_spread_mu - n_spread_t_dist,
         n_spread_end = n_spread_mu + n_spread_t_dist) %>%
  cbind(nat_spreads %>% filter(cycle == 2016) %>% select(nat_spread)) %>%
  select(cycle, state, n_spread_mu, n_spread_se = pred_se, n_spread_t_dist, n_spread_start, n_spread_end, nat_spread) %>%
  rename(n_spread_2016 = nat_spread)

head(nat_spread_prior)

save(nat_spread_prior, file = 'rda/nat_spread_prior.rda')
