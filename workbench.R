library(readr)
library(dplyr)
library(ggplot2)

# Setup

raw <- read_csv("Sample Dataset.csv", col_names = FALSE, show_col_types = FALSE)

var_labels <- raw %>% slice(1) %>% unlist(use.names = FALSE)
var_names  <- raw %>% slice(2) %>% unlist(use.names = FALSE)

dat <- raw %>% slice(-(1:2))
names(dat) <- var_names

# Question 1

n_completed <- nrow(dat)
n_completed

# Question 2

# 2a) Field dates
dat <- dat %>%
  mutate(submitdate = as.Date(submitdate, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")))

range(dat$submitdate, na.rm = TRUE)
sum(is.na(dat$submitdate))  # quick parse check

# 2b) Survey modes
dat %>% count(Mode, sort = TRUE)

# Optional spotcheck: confirm Mode and modenew are identical
all(dat$Mode == dat$modenew)

# Phone vs text
dat %>% count(phonetext, sort = TRUE)

# Phone breakdown (among phone interviews only)
dat %>% filter(phonetext == "Phone") %>% count(cell, sort = TRUE)

dat %>%
  count(Mode) %>%
  ggplot(aes(x = Mode, y = n, fill = Mode)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Survey Mode Distribution",
    x = "Mode",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 12)

# Question 3
# Unweighted proportion of voters of each gender who completed the survey

gender_unweighted <- dat %>%
  filter(!is.na(samplegender)) %>%          # exclude missing from proportions
  count(samplegender, sort = TRUE) %>%      # unweighted counts
  mutate(
    proportion = n / sum(n),
    percent = round(proportion * 100, 1)
  )

gender_unweighted

gender_unweighted %>%
  ggplot(aes(x = reorder(samplegender, percent), y = percent)) +
  geom_col(fill = "#55A868") +
  coord_flip() +
  labs(
    title = "Unweighted Gender Composition of Sample",
    x = "Gender",
    y = "Percent of Respondents"
  ) +
  theme_minimal(base_size = 12)

# Question 4
# Of the last four elections (G21, P21, G17, P17), how many participants voted 
# in 0, 1, 2, 3, or 4 elections?

voting_summary <- dat %>%
  mutate(
    across(
      c(G21, P21, G17, P17),
      ~ case_when(
        .x == "Voted" ~ 1,
        .x == "Did not vote" ~ 0,
        TRUE ~ NA_real_
      )
    ),
    elections_voted = rowSums(across(c(G21, P21, G17, P17)), na.rm = TRUE)
  ) %>%
  count(elections_voted) %>%
  mutate(
    category = case_when(
      elections_voted == 0 ~ "Voted in 0 of 4 elections",
      elections_voted == 1 ~ "Voted in 1 of 4 elections",
      elections_voted == 2 ~ "Voted in 2 of 4 elections",
      elections_voted == 3 ~ "Voted in 3 of 4 elections",
      elections_voted == 4 ~ "Voted in all 4 elections"
    ),
    proportion = n / sum(n),
    percent = round(proportion * 100, 1)
  ) %>%
  select(category, n, percent) %>%
  arrange(percent)

voting_summary

voting_summary %>%
  ggplot(aes(x = category, y = percent)) +
  geom_col(fill = "#C44E52") +
  coord_flip() +
  labs(
    title = "Voter Participation in Recent Elections",
    subtitle = "Number of elections voted in (out of four)",
    x = "",
    y = "Percent of Respondents"
  ) +
  theme_minimal(base_size = 12)

# Question 5
# Distribution of survey weights

# Ensure weight is numeric
dat <- dat %>%
  mutate(weight = as.numeric(weight))

# Check for any conversion issues
sum(is.na(dat$weight))

# 1) Histogram of weights
ggplot(dat, aes(x = weight)) +
  geom_histogram(
    bins = 30,
    fill = "#4C72B0",
    color = "white",
    alpha = 0.85
  ) +
  labs(
    title = "Distribution of Survey Weights",
    subtitle = "Unweighted count of respondents",
    x = "Weight",
    y = "Number of Respondents"
  ) +
  theme_minimal(base_size = 12)

# 2) Minimum weight and how many observations have it
min_weight <- min(dat$weight, na.rm = TRUE)

n_min_weight <- dat %>%
  filter(weight == min_weight) %>%
  nrow()

# 3) Maximum weight
max_weight <- max(dat$weight, na.rm = TRUE)

# 4) Median weight
median_weight <- median(dat$weight, na.rm = TRUE)

# 5) Print results
cat("Minimum weight:", min_weight, "\n")
cat("Number of observations with minimum weight:", n_min_weight, "\n")
cat("Maximum weight:", max_weight, "\n")
cat("Median weight:", median_weight, "\n")
