library(tidyverse)
library(tibble)
library(ggplot2)
library(gt)
rm(list = ls())

setwd("/Users/jeremydumalig/Documents/GitHub/Biomechanics-and-Player-Availability")

raw_nba <- read_csv("nba_wearables.csv")

# Data Cleaning
nba <-
  raw_nba %>%
  filter(lastMP >= 15, # filter out non-rotation players
         MP >= 15) %>%
  group_by(Player, Season) %>%
  arrange(desc(GP)) %>%
  slice_max(GP) %>% # some players played for multiple teams in same season
  slice(1) %>% # only keep aggregate data for each player's season
  ungroup()

# More Data Cleaning
did_data <-
  nba %>%
  filter(Season %in% c(2013, 2014)) %>%
  group_by(Player) %>%
  filter(n_distinct(Season) == 2, # one entry per season
         n_distinct(tech) == 1) %>% # treatment group doesn't change
  ungroup()

# Data Aggregation
aggregate <-
  did_data %>%
  group_by(tech, time) %>%
  summarize(Count = n(),
            GP = mean(GP),
            .groups='drop') %>%
  ungroup() %>%
  mutate(time = ifelse(time == 0, 2013, 2014),
         tech = ifelse(tech == 0, "Control", "Treatment"))
aggregate

# Difference-in-Difference Estimation
did_model <- lm(GP ~ lastGP + age + tech*time, data=did_data)
summary(did_model)

# Coefficients in Tabular Form
data.frame(summary(did_model)$coefficients) %>%
  tibble::rownames_to_column(var="Variable") %>%
  mutate(Estimate = round(Estimate, 2),
         `p-value` = case_when(`Pr...t..` < 0.01 ~ "<0.01",
                       TRUE ~ as.character(round(`Pr...t..`, 3)))) %>%
  select(Variable, Estimate, `p-value`) %>%
  gt() %>%
  tab_header(title = md("**Linear Model: Coefficients**"),
             subtitle = "Difference-in-Differences Estimation")

# GGplot Custom Theme
my_theme <-
  theme_bw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "grey90",
                                   color = "black"),
    legend.box.background = element_rect(size=0.75),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=36,
                                margin = margin(t=15, r=0, b=15, l=0)),
    axis.title.y = element_text(size=36,
                                margin = margin(t=0, r=15, b=0, l=15)),
    plot.title = element_text(size=36,
                              face="bold",
                              margin = margin(t=15, r=0, b=15, l=0)),
    plot.subtitle = element_text(size=28),
    plot.caption = element_text(size=20),
    legend.title = element_text(size=28),
    legend.text = element_text(size=20),
    legend.margin = margin(10, 10, 10, 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())

# Aggregate-Level Calculations
control_before <- filter(aggregate, tech == "Control", time == 2013)$GP
control_after <- filter(aggregate, tech == "Control", time == 2014)$GP
tx_before <- filter(aggregate, tech == "Treatment", time == 2013)$GP
tx_after <- filter(aggregate, tech == "Treatment", time == 2014)$GP

# Difference-in-Differences Visualization
aggregate %>%
  select(-Count) %>%
  add_row(tech = "Counterfactual", 
          time = 2013, 
          GP = tx_before) %>%
  add_row(tech = "Counterfactual", 
          time = 2014, 
          GP = tx_before + (control_after - control_before)) %>%
  ggplot(aes(x=factor(time),
             y=GP,
             group=tech,
             linetype=tech,
             color=factor(tech))) +
  geom_segment(aes(x="2014", xend="2014", 
                   y=tx_before + (control_after - control_before), yend=tx_after),
               linetype='dashed',
               color='black') +
  geom_line(size=2,
            show.legend=FALSE) +
  geom_point(size=6,
             show.legend=FALSE) +
  scale_color_manual(values=c("maroon", "lightblue", "darkblue")) +
  scale_linetype_manual(values=c('solid', 'longdash', 'solid')) +
  ylim(c(61.5, 70.5)) +
  labs(title="Diff-in-Diff Intuition",
       x="Season",
       y="Average Games Played",
       color="Group") +
  guides(alpha="none") +
  my_theme

# Parallel Trends Assumption
nba %>%
  filter(Player %in% c(unique(did_data$Player)),
         Season <= 2013) %>%
  mutate(tech = case_when(tech == 1 ~ "Treatment",
                          TRUE ~ "Control"),
         Season = case_when(Season == 2012 ~ "2012*",
                            TRUE ~ as.character(Season))) %>%
  group_by(Player) %>%
  filter(n_distinct(tech) == 1) %>%
  ungroup() %>%
  group_by(Season, tech) %>%
  summarize(Count = n(),
            SD = sd(GP),
            GP = mean(GP),
            .groups='drop') %>%
  ungroup() %>%
  ggplot(aes(x=Season,
             y=GP,
             group=tech,
             color=factor(tech))) +
  geom_line(size=2,
            show.legend=FALSE) +
  geom_errorbar(aes(ymin = GP - 1.96*SD/sqrt(Count), ymax = GP + 1.96*SD/sqrt(Count)),
                width=0.1) +
  geom_point(size=6) +
  scale_color_manual(values=c("maroon", "darkblue")) +
  labs(title="Pre-Treatment: Parallel Trends Assumption",
       caption="* Scaled to account for 66-game shortened season",
       x="Season",
       y="Average Games Played",
       color="Group") +
  my_theme