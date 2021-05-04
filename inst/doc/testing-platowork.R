## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  message = FALSE,
  fig.width = 7,
  fig.height = 5
)

## ----peek, echo=TRUE----------------------------------------------------------
library(platowork)
head(plato)

## ----data---------------------------------------------------------------------
library(ggplot2)
library(dplyr)
theme_set(theme_minimal())

plato %>%
  group_by(subject, session) %>% 
  mutate(first_test = min(date),
         last_test = max(date),
         session_duration = difftime(last_test, first_test, units = "mins")) %>% 
  group_by(subject, stimulus) %>%
  summarise(no_tests = n(),
            no_sessions = n_distinct(session),
            first_test = min(first_test), 
            avr_session_duration = round(sum(session_duration) / sum(no_tests), 1)) %>%
  ungroup() %>% 
  arrange(subject, first_test)

## ----plot1--------------------------------------------------------------------
data_summary <- plato %>% 
  group_by(subject, session, stimulus) %>% 
  summarise(wpm = mean(wpm),
            date = mean(date),
            .groups = "drop")

plato %>% 
  ggplot(aes(x = session, y = wpm, color = stimulus)) +
  geom_jitter(size = 1.0, width = 0.15, height = 0, show.legend = T) +
  geom_crossbar(data = data_summary, aes(ymin = wpm, ymax = wpm), show.legend = F) +
  scale_x_continuous(breaks = 1:13) +
  facet_grid(rows = vars(subject)) +
  labs(title = "Performance in speed typing tests with or without tDCS stimulus",
       subtitle = "Each dot represent one speed typing test. Horizontal bars are average results for each session",
       caption = "Two subjects, double blind experiment with unknown stimulus\nSpeed typing tests performed at 10fastfingers.com. Analysis by @lassehmadsen",
       x = "Session number", 
       y = "wpm, words-per-minute",
       color = "Stimulus") +
  theme(strip.background = element_rect(fill = grey(0.93), linetype = "blank"),
        legend.justification = c(1, 0),
        legend.position = c(1, 0))

## ----plot2--------------------------------------------------------------------
data_summary <- plato %>% 
  #filter(stimulus != "None") %>% 
  group_by(subject, stimulus) %>% 
  summarise(wpm = mean(wpm), .groups = "drop")

plato %>% 
  #filter(stimulus != "None") %>% 
  ggplot(aes(x = wpm, color = stimulus, fill = stimulus)) +
  geom_density(alpha = 0.4, show.legend = c("color" = FALSE, "fill" = TRUE)) +
  geom_vline(data = data_summary, aes(xintercept = wpm, color = stimulus), show.legend = FALSE) +
  facet_grid(rows = vars(subject)) +
  labs(title = "Performance in speed typing tests with or without tDCS stimulus",
       subtitle = "Density distributions. Vertical lines are the averages for each subject/stimulus",
       caption = "Two subjects, double blind experiment with unknown stimulus\nSpeed typing tests performed at 10fastfingers.com. Analysis by @lassehmadsen",
       x = "wpm, words-per-minute", 
       y = "density",
       fill = "Stimulus") +
  theme(strip.background = element_rect(fill = grey(0.93), linetype = "blank"),
        legend.justification = c(1, 1),
        legend.position = c(1, 1))

## ----anova1, echo=TRUE--------------------------------------------------------
m <- aov(wpm ~ stimulus, data = plato)
summary(m)

## ----wrangling, echo=TRUE-----------------------------------------------------
# 1) Standardize wpm;
# 2) Filter out no stimulus;
# 3) Set factor levels for later plotting.

data <- plato %>% 
  group_by(subject) %>% 
  mutate(wpm_stand = (wpm - mean(wpm)) / sd(wpm)) %>% 
  ungroup() %>% 
  filter(stimulus != "None") %>%
  mutate(stimulus = factor(stimulus, levels = c("Sham", "Real")))

## ----anova2, echo=TRUE--------------------------------------------------------
m <- aov(wpm ~ stimulus, data = data)
summary(m)

## ----anova3, echo=TRUE--------------------------------------------------------
m <- aov(wpm_stand ~ stimulus, data = data)
summary(m)

## ----anova4, echo=TRUE--------------------------------------------------------
m <- aov(wpm_stand ~ stimulus * subject, data)
summary(m)

## ----plot3--------------------------------------------------------------------
data %>% 
  ggplot(aes(x = session, y = wpm_stand, color = stimulus)) +
  geom_jitter(size = 1.0, width = 0.15, height = 0, show.legend = F) +
  geom_smooth(method = "lm", show.legend = F) +
  scale_x_continuous(breaks = 1:13) +
  facet_grid(rows = vars(subject), cols = vars(stimulus), scales = "free_x") +
  labs(title = "Performance in speed typing tests with or without tDCS stimulus",
       subtitle = "Words-per-minute plotted by session number. Slope of linear fit indicates learning rate",
       caption = "Two subjects, double blind experiment with unknown stimulus\nSpeed typing tests performed at 10fastfingers.com. Analysis by @lassehmadsen",
       x = "Session number", 
       y = "wpm, words-per-minute, standardized",
       fill = "Stimulus") +
  theme(strip.background = element_rect(fill = grey(0.93), linetype = "blank"),
        legend.justification = c(1, 0),
        legend.position = c(1, 0))

## ----regression, echo=TRUE----------------------------------------------------
m <- lm(wpm_stand ~ session * stimulus * subject, data = data)
summary(m)

## ----split_combine, echo=TRUE-------------------------------------------------
library(purrr) # For map function

data %>%
  split(list(.$stimulus, .$subject)) %>%
  map(~ lm(wpm_stand ~ session, data = .)) %>%
  map(summary) %>%
  map("coefficients") %>% 
  map(as_tibble, rownames = "parameter") %>% 
  bind_rows(.id = "model") %>% 
  filter(parameter == "session") %>% 
  select(model, parameter, slope = Estimate, p_value = `Pr(>|t|)`) %>% 
  mutate(across(where(is.numeric), round, 3))

