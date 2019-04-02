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

# start here
cohort.1 <- read.csv("cohort_1_final.csv") %>%
  select(Day, Subject, Session, Time, Response, Reward, Trial) %>%
  mutate(Subject = as.factor(Subject)) %>%
  group_by(Day, Subject, Session) %>%
  mutate(cumResponse = cumsum(Response)) %>%
  mutate(cumReward = cumsum(Reward))

plot <- ggplot(cohort.1, aes(colour = as.factor(Day), x = Trial)) +
  geom_point(aes(y = cumResponse))
plot
ggplotly(plot)


check2 <- cohort.1 %>% filter(Trial == 0)

# days to competion data
cohort.1.daily.rewards <- cohort.1 %>%
  group_by(Day, Subject, Session) %>%
  summarise(totalReward =max(cumReward))

plot <- ggplot(cohort.1.daily.rewards, aes(x = Day, y = totalReward, colour = Session ))+
  geom_point() +
  facet_wrap(~Subject)
plot

# left first : 2, 4, 8, 10, 12, 14, 16
# right first : 3, 9, 11, 13, 15

cohort.1 <- cohort.1 %>%
  mutate(SessionOrder = case_when(
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weathertaskL" ~ 1,
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weathertaskR" ~ 2,
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weatherhardL" ~ 3,
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weatherhardR" ~ 4,
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weatherharderL" ~ 5,
    Subject %in% c(2, 4, 8, 10, 12, 14, 16) & Session == "weatherharderR" ~ 6,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weathertaskR" ~ 1,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weathertaskL" ~ 2,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weatherhardR" ~ 3,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weatherhardL" ~ 4,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weatherharderR" ~ 5,
    Subject %in% c(3, 9, 11, 13, 15) & Session == "weatherharderL" ~ 6
  ))

# days in each session

cohort.1.sessiondays <- cohort.1 %>%
  group_by(Subject, SessionOrder) %>%
  summarise(MaxDays = max(Day)-min(Day)+1)

cohort.1.sessiondays.summary <- cohort.1.sessiondays %>%
  ungroup() %>%
  mutate(Subject = as.numeric(Subject)) %>%
  mutate(SessionOrder= as.factor(SessionOrder)) %>%
  group_by(SessionOrder) %>%
  summarise(meanDays = mean(MaxDays), SEMDays = sd(MaxDays)/n(), n = n())

sessiondays.plot <- ggplot(cohort.1.sessiondays.summary, aes(x=SessionOrder, y = meanDays, fill = SessionOrder)) +
  geom_col()+
  geom_errorbar(aes(ymin = meanDays-SEMDays, ymax = meanDays + SEMDays)) +
  ggtitle("Mean Days in Each Stage")
sessiondays.plot

cohort.1 <- cohort.1 %>%
  group_by(Day, Subject, SessionOrder) %>%
  mutate(TimeBetween = Time - lag(Time)) %>%
  ungroup()

cohort.1$TimeBetween <- replace_na(cohort.1$TimeBetween, 0)



# daily summaries by animal

cohort.1.daily.individuals <- cohort.1 %>%
  group_by(Day, Subject, SessionOrder) %>%
  summarise(TotalTrials = max(Trial),
            TotalReward = sum(Reward),
            TotalCorrect = sum(Response == 1),
            TotalIncorrect = sum(Response == -1),
            TotalOmissions = sum(Response == 0),
            MeanTime = mean(TimeBetween))

plot <- ggplot(cohort.1.daily.individuals, aes(x = Day, shape = as.factor(SessionOrder))) +
  geom_point(aes(y = TotalReward, colour = "Reward"))+
  geom_line(aes(y = TotalCorrect, colour = "Correct"))+
  geom_line(aes(y = TotalIncorrect, colour = "Incorrect"))+
  geom_line(aes(y = TotalOmissions, colour = "Omissions"))+
  facet_wrap(~Subject)
plot
ggplotly(plot)

#average trial times in sessions
ggplot(cohort.1.daily.individuals, aes(x = Day, shape = as.factor(SessionOrder))) +
  geom_point(aes(y=MeanTime)) +
  facet_wrap(~Subject)

# "blocks" of 10 trials

cohort.1 <- cohort.1 %>%
  mutate(Block = ceiling(Trial/10)) %>%
  mutate(Block = replace(Block, Block==0, 1))

