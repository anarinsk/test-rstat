# From https://www.danielphadley.com/bootstrap_tutto/
# Data is from https://raw.githubusercontent.com/DanielHadley/DanielHadley2.0/master/data/helmets.csv 

#usethis::browse_github_pat()
#usethis::edit_r_environ()

#devtools::install_github("tidymodels/tidymodels")

library(tidyverse)
library(tidymodels)

# Loading data 
helmets <- read_csv('https://raw.githubusercontent.com/DanielHadley/DanielHadley2.0/master/data/helmets.csv')

# 
set.seed(1876)
helmet_bootraps <- bootstraps(helmets, times = 3e3, apparent = TRUE)
as_tibble(helmet_bootraps$splits[[55]]) %>% head()

#1
calc_mips_mean <- function(split){
  dat <- analysis(split) %>% 
    filter(mips) %>% 
    pull(score)
  # Put it in this tidy format to use int_pctl
  return(tibble(
    term = "mean",
    estimate = mean(dat),
    std.err = sd(dat)/sqrt(length(dat))))
}

# 2
base_model <- function(split){
  lm(score ~ style + mips + wavecel, data = analysis(split)) %>% 
    tidy()
}


# 3
beta_diff <- function(split){
  model <- lm(score ~ style + mips + wavecel, data = analysis(split)) 
  # Put it in this tidy format to use int_pctl
  return(tibble(
    term = "diff_mips_wavecel",
    estimate = model$coefficients["wavecelTRUE"] - model$coefficients["mipsTRUE"],
    std.err = NA_real_))
}

helmet_stats <- helmet_bootraps %>%
  mutate(
    mips_mean = map(splits, calc_mips_mean),
    coef_info = map(splits, base_model),
    coef_diff = map(splits, beta_diff))

head(helmet_stats)

int_pctl(helmet_stats, mips_mean)

helmet_coefs <- helmet_stats %>%
  unnest(coef_info)

helmet_coefs %>%
  select(estimate, term) %>% 
  filter(term %in% c("wavecelTRUE", "mipsTRUE")) %>% 
  ggplot(aes(estimate, fill = term)) +
  geom_histogram(alpha = 0.7, position="identity") +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  labs(x = "Estimated Impact of the Technology",
       y = "Count /3k Bootstrapped Models",
       title = "WaveCel is better, but there is overlap")

int_pctl(helmet_stats, coef_diff)


proportion <- helmet_stats %>%
  unnest(coef_diff) %>% 
  filter(estimate >= -2.551318 & estimate <= 0.2744458) %>% 
  group_by(id) %>% 
  mutate(pos = sum(estimate > 0),
         neg = sum(estimate < 0)) 

sum(proportion$neg) / nrow(proportion)

