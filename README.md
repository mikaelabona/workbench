# Questions for Dataset Review

Setup Code:
```
library(readr)
library(dplyr)
library(ggplot2)

# Setup

raw <- read_csv("Sample Dataset.csv", col_names = FALSE, show_col_types = FALSE)

var_labels <- raw %>% slice(1) %>% unlist(use.names = FALSE)
var_names  <- raw %>% slice(2) %>% unlist(use.names = FALSE)

dat <- raw %>% slice(-(1:2))
names(dat) <- var_names
```
---
a. How many completed interviews are in the survey?  
Answer: 300  
```
n_completed <- nrow(dat)
n_completed
```
---
b. What dates were the survey responses collected? What modes were used to collect responses?  
Answer: The survey responses were collected 09-29-2024 to 09-30-2024. The modes used were cell, landline, and text.
<img width="1360" height="922" alt="image" src="https://github.com/user-attachments/assets/480868c2-0874-44d9-95c5-f1ca3f4bd882" />
```
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
```
---
c. What is the unweighted proportion of voters of each gender who completed the survey?  
Answer:  
| Gender   | Count (n) | Proportion | Percent |
|----------|-----------|------------|---------|
| Female   | 162       | 0.540      | 54.0%   |
| Male     | 130       | 0.433      | 43.3%   |
| Unknown  | 8         | 0.027      | 2.7%    |

<img width="1360" height="922" alt="image" src="https://github.com/user-attachments/assets/ea9f5d86-4d5f-45fd-97ae-012ddaee2207" />

```
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
```

---
d. Of the last four elections in the dataset (G21, P21, G17, P17) how many participants voted in 0/4 elections? 1/4 elections? 2/4? 3/4? 4/4?  
Answer:
| Voting History                    | Count (n) | Percent |
|----------------------------------|-----------|---------|
| Voted in 0 of 4 elections         | 80        | 26.7%   |
| Voted in 1 of 4 elections         | 27        | 9.0%    |
| Voted in 2 of 4 elections         | 40        | 13.3%   |
| Voted in 3 of 4 elections         | 38        | 12.7%   |
| Voted in all 4 elections          | 115       | 38.3%   |

<img width="1360" height="922" alt="image" src="https://github.com/user-attachments/assets/ca29a937-a969-43f6-9e7b-596d13298bf7" />

```
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
```
---
e. Create a histogram showing the distribution of weights in this dataset. What is the minimum weight, and how many observations are assigned that weight? The maximum weight? The median?  
Answer:   
Minimum weight: 0.4194878  
Number of observations with minimum weight: 2  
Maximum weight: 2.497186  
Median weight: 0.7871497  

<img width="1360" height="922" alt="image" src="https://github.com/user-attachments/assets/76ab7f61-7064-4a16-904d-625db732a811" />

```
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
```
