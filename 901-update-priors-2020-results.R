{# Libraries
  library(tidyverse)
  library(tidylog)
  library(lubridate)
  library(broom)
  library(knitr)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda") # GGPlot Theme
}
options(scipen = 999)

# Graph Colors
party_col <- c('darkblue', 'red4', 'green4')
# Revise Pres Results ----------------------------------------------------------

# Load Election Results 1976 - 2016
pres_results_all <- read.csv("data/potus_results_76_16_tidy.csv", stringsAsFactors = FALSE, header = TRUE)
pres_results_all <- pres_results_all[, 2:7]

# Add 2020 Results
pres_results_2020 <- read.csv('data/pres-results-2020.csv', stringsAsFactors = FALSE, header = TRUE)
pres_results_2020 <- pres_results_2020[, 2:16]

head(pres_results_2020)
head(pres_results_all)

abbs <- pres_results_all %>%
  select(state, state_abb) %>%
  group_by(state) %>%
  summarize(state_abb = state_abb[1])

pres_results_all <- pres_results_2020 %>%
  left_join(abbs) %>%
  mutate(other = Jorgensen_LIB_share + Hawkins_GRN_share + Other_share) %>%
  select(cycle,
         state,
         state_abb,
         total_votes = total_vote, 
         dem = Biden_DEM_share,
         rep = Trump_REP_share,
         other) %>%
  gather(party, vote_share, dem:other) %>%
  mutate(total_votes = total_votes * vote_share) %>%
  select(cycle, state, state_abb, party, vote_share, total_votes) %>%
  rbind(pres_results_all)

head(pres_results_all)

cycle_votes <- pres_results_all %>%
  filter(!state %in% c('Maine CD-1',
                       'Maine CD-2',
                       'Nebraska CD-1',
                       'Nebraska CD-2',
                       'Nebraska CD-3')) %>%
  group_by(cycle) %>%
  summarize(total = sum(total_votes)) %>%
  ungroup()

nat_results_all <- pres_results_all %>%
  filter(!state %in% c('Maine CD-1',
                       'Maine CD-2',
                       'Nebraska CD-1',
                       'Nebraska CD-2',
                       'Nebraska CD-3')) %>%
  group_by(cycle, party) %>%
  summarize(total_votes = sum(total_votes)) %>%
  left_join(cycle_votes) %>%
  mutate(vote_share = total_votes / total,
         state = 'National',
         state_abb = 'NAT') %>%
  select(cycle, state, state_abb, party, vote_share, total_votes)

pres_results_all <- pres_results_all %>%
  rbind(nat_results_all) %>%
  mutate(party = factor(party, c('dem', 'rep', 'other'))) %>%
  arrange(cycle, party)

write.csv(pres_results_all, file = 'out/potus_results_76_20_tidy.csv')


# Visualize All State Trends
pres_results_all %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = party_col) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth() +
  facet_wrap(. ~ state) +
  theme(axis.text.x = element_text(angle = 90))

# Visualize One State Trend
pres_results_all %>%
  mutate(party = factor(party, (levels = c('dem', 'rep', 'other')))) %>%
  filter(state == 'Florida') %>%
  ggplot(aes(x = cycle, y = vote_share, color = party)) +
  geom_point() +
  scale_color_manual(values = party_col) +
  scale_x_continuous(breaks = seq(1972, 2028, 4)) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 90))



# Spreads Table ----------------------------------------------------------------
# Visualize Spread Trends (Dem vs Rep Only)
pres_spreads_all <- pres_results_all %>%
  group_by(cycle, state) %>%
  summarize(spread = vote_share[2] - vote_share[1])

write.csv(pres_spreads_all, file = 'out/potus_spreads_76_20.csv')

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



