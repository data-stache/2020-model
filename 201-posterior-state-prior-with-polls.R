load('rda/state_spread_prior.rda')
load('rda/nat_spread_prior.rda')
load('rda/poll_results.rda')

head(state_spread_prior)
head(nat_spread_prior)
head(poll_results)

z_score <- median(state_spread_prior$spread_z)

priors_polls <- nat_spread_prior %>%
  mutate(national_cor = 1) %>%
  rename(spread_mu = n_spread_mu, 
         spread_se = n_spread_se,
         spread_t_dist = n_spread_t_dist,
         spread_start = n_spread_start,
         spread_end = n_spread_end,
         spread_2016 = n_spread_2016) %>%
  rbind(state_spread_prior %>% select(-spread_z)) %>%
  left_join(poll_results) %>%
  select(-spread_start, -spread_end, -polls_start, -polls_end)

priors_polls <- priors_polls %>%
  mutate(nat_adjust = priors_polls$polls_avg[priors_polls$state == 'National'] - priors_polls$spread_2016[priors_polls$state == 'National'],
         nat_adjust = nat_adjust * national_cor,
         polls_avg = ifelse(is.na(polls_avg), nat_adjust + spread_2016, polls_avg),
         poll_t_dist = ifelse(is.na(poll_t_dist), max(poll_t_dist, na.rm = TRUE), poll_t_dist))

head(priors_polls)

bias_sd <- 0.025
posteriors <- priors_polls %>%
  mutate(sigma = sqrt(poll_t_dist^2 + bias_sd^2),
         B = sigma^2 / (sigma^2 + spread_t_dist^2),
         posterior_mean = B * spread_mu + (1 - B) * polls_avg,
         posterior_se = sqrt(1 / (1 / sigma^2 + 1 / spread_t_dist^2)),
         posterior_z = z_score,
         posterior_t_dist = posterior_z * poll_t_dist,
         posterior_start = posterior_mean - posterior_t_dist,
         posterior_end = posterior_mean + posterior_t_dist) %>%
  select(state, sigma:posterior_end)

head(posteriors)

save(posteriors, file = 'rda/posteriors.rda')

load('rda/pres_results_2020.rda')

head(pres_results_2020)

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

act_ord <- dat_res %>%
  arrange(actual) %>%
  .$state

posteriors %>%
  left_join(dat_res) %>%
  mutate(state = factor(state, levels = act_ord)) %>%
  ggplot(aes(x = state, y = posterior_mean, ymin = posterior_start, ymax = posterior_end)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point() +
  geom_point(aes(x = state, y = actual), color = 'red') +
  coord_flip()

