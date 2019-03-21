library(readxl)
library(tidyverse)
library(anchors)
library(plotly)
rm(list = ls())

cohort.2 <- read_xlsx("data2 to 061518.xlsx", col_names = TRUE) # 8 animals in cohort 2, out of 10 total - very few days though, so not sure how useful they'll be...
cohort.2 <- cohort.2 %>%
  filter(is.na(Session) == FALSE) %>%
  replace.value("Reward", from = -1, to = as.integer(0)) %>%
  distinct()

cohort.1 <- read_xlsx("data to 061518.xlsx", col_names = TRUE) # 12 animals in cohort 1, out of 16 total
cohort.1 <- cohort.1 %>%
  filter(is.na(Session) == FALSE) %>%
  replace.value("Reward", from = -1, to = as.integer(0)) %>%
  distinct()

cohort.both <- rbind(cohort.1, cohort.2)


## cohort 1 summary stats
summary(cohort.1)
# Max days in protocol = 87

cohort.1 %>%
  group_by(Day, Subject, Session) %>%
  summarise(Total.Presses = max(Trial), Total.Correct = sum(Response == 1), Ratio.Correct = Total.Correct/Total.Presses) ->
  cohort.1.summary.by.day

cohort.1.summary.by.day %>%
  ggplot(aes(x=Day, y= Ratio.Correct, colour = Session)) +
  geom_point() +
  facet_wrap(~Subject)

# Initially 16 animals
# in first stage 12 animals (2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16)
# reversed 12 animals
# 70:30 10 animals (2, 3, 9, 10, 11, 12, 13, 14, 15, 16)
# 70:30 reversed 7 animals (2, 9, 10, 12, 13, 14, 16)
# 60:40 4 animals (9, 10, 13, 14)
# 60:40 reversed 3 animals (9, 10, 14)

cohort.1.summary.by.day %>% filter(Ratio.Correct>1)

# days to worry about - probably doubling up on the day counts - wil need to investigate and fix

# Day Subject
# 7      11 weathertaskR            99           137          1.38 2 days in 1
# 8      12 weathertaskL            99           253          2.56  4 days in 1?!
# 30       9 weathertaskL            99           113          1.14 2 in 1
# 41       2 weatherhardL            99           111          1.12 2 in 1
# 53      14 weatherhardR            99           114          1.15 2 in 1
# 58      12 weatherhardR            99           101          1.02 2 in 1
# 77      10  WTL and WharderR

# days missing
#
# 2 - 40, 42
# 3 - 19, 21
# 4 - 38, 40. 42
# 8 - 33, 35, 36, 55
# 9 - 31, 58
# 10 - 52, 53, 59
# 11 - 3, 40
# 12 - 1, 4, 5, 27, 38, 56
# 13 - 1, 4, 49
# 14 - 1, 12, 13, 14, 15, 16, 50
# 15 - 17
# 16 - 22
write.csv(cohort.1, "cohort_1_for_cleaning.csv")


## cohort 2 summary stats

cohort.2 %>%
  group_by(Day, Subject, Session) %>%
  summarise(Total.Presses = max(Trial), Total.Correct = sum(Response == 1), Ratio.Correct = Total.Correct/Total.Presses) ->
  cohort.2.summary.by.day

cohort.2.summary.by.day %>%
  ggplot(aes(x=Day, y= Ratio.Correct, colour = Session)) +
  geom_point() +
  facet_wrap(~Subject)

# 10 animals to begin with
# in first stage 8 animals (2, 3, 4, 5, 6, 7, 8, 9)
# reversed 5 animals (2, 3, 4, 6, 8)
# reversed back 4 animals (2, 3, 4, 6)
# and back again 1 animal (2)

###############################################################################

# data cleaned, and ready for reimporting form here


rm(list=ls())
library(DataExplorer)
cohort.1 <- read.csv("cohort_1_cleaned.csv")

plot_missing(cohort.1)
plot_bar(cohort.1)
plot_histogram(cohort.1)

cohort.1 %>%
  mutate(Reward = ifelse(is.na(Reward),
                         ifelse(Response == 1, rbinom(1, 1, 0.8), rbinom(1, 1, 0.2)), Reward)) %>%
  mutate(Reward = ifelse(Reward == -1, 0, Reward)) %>%
  mutate(Day = as.factor(Day)) %>%
  mutate(Subject = as.factor(Subject)) ->
  df

create_report(df)
