library(readxl)
library(tidyverse)
library(anchors)
rm(list = ls())

weather0 <- read_xlsx("data2 to 061518.xlsx", col_names = TRUE)
weather0 <- weather0 %>%
  filter(is.na(Session) == FALSE) %>%
  mutate(Subject = Subject)

weather1 <- read_xlsx("data to 061518.xlsx", col_names = TRUE)
weather1 <- weather1 %>%
  filter(is.na(Session) == FALSE)

weather <- rbind(weather0, weather1)

weather <- replace.value(weather, "Reward", from = -1, to = as.integer(0))

