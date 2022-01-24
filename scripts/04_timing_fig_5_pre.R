## Replication file for Typecast
## Douglas Ahler and Gaurav Sood

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(MASS)
library(erer)
library(lme4)
library(margins)

# Set Working dir. to project folder
setwd(paste0(githubdir, "typecast/"))

#****************************************
#*** Time pressure/requirements study ***
#****************************************

#* Load data
timing <- read.dta13("data/timing.dta")
timing_sm <- timing %>% filter(timing_cond  == "fast" | timing_cond == "slow")

small_time <- timing_sm %>%
	dplyr::select(timing_cond, 
		   timing_dem_black_fast, timing_dem_black_slow, 
		   timing_dem_aa_fast, timing_dem_aa_slow, 
		   timing_dem_lgb_fast, timing_dem_lgb_slow,
		   timing_dem_union_fast, timing_dem_union_slow,
		   timing_rep_evang_fast, timing_rep_evang_slow,
		   timing_rep_old_fast, timing_rep_old_slow,
		   timing_rep_south_fast, timing_rep_south_slow,
		   timing_rep_rich_fast, timing_rep_rich_slow,
		   responseid) %>%
	gather(var_name, var_value, timing_dem_black_fast:timing_rep_rich_slow, -timing_cond, -responseid)

# Let's add some dummies
small_time$black  <- grepl("black", small_time$var_name)
small_time$aa     <- grepl("aa", small_time$var_name)
small_time$lgb    <- grepl("lgb", small_time$var_name)
small_time$union  <- grepl("union", small_time$var_name)
small_time$evang  <- grepl("evang", small_time$var_name)
small_time$old    <- grepl("old", small_time$var_name)
small_time$south  <- grepl("south", small_time$var_name)
small_time$rich   <- grepl("rich", small_time$var_name)

# Group
small_time <- small_time %>%
              mutate(group = case_when(
              	black == TRUE ~ "black",
              	aa == TRUE ~ "aa",
              	lgb == TRUE ~ "lgb",
              	union == TRUE ~ "union",
              	evang == TRUE ~ "evang",
              	old == TRUE ~ "old",
              	south == TRUE ~ "south",
              	rich == TRUE ~ "rich"
              ))

# Basic regressions
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(black)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(aa)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(lgb)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(union)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(evang)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(old)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(south)))
summary(lm(var_value ~ timing_cond, data = small_time %>% filter(rich)))

# Long time
m1 <- with(small_time,  lmer(var_value ~ timing_cond + (1|responseid)))
m2 <- with(small_time[small_time$timing_cond == "slow",],  lmer(var_value ~ 1 + (1|responseid)))
m3 <- with(small_time[small_time$timing_cond == "fast",],  lmer(var_value ~ 1 + (1|responseid)))

confint(m1)
margins_summary(m1)

# Plot
res = data.frame(mean = c(fixef(m2), fixef(m3)),
                 lower.ci = c(confint(m2)[3, 1], confint(m3)[3, 1]), 
                 upper.ci = c(confint(m2)[3, 2], confint(m3)[3, 2]),  
                 time_pressure = c(0, 1), 
                 time_cond = c("Time requirements", "Time constraints"))

### Figure 5: TIMING EXPERIMENT
#Plot
# Custom Theme
cust_theme <- 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(.5, "cm"),
        legend.text           = element_text(color = "grey30"),
        legend.title          = element_text(size = 12),
        axis.title   = element_text(size = 10, color = "grey30"),
        axis.text    = element_text(size = 10, color = "grey30"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(res, aes(y = as.factor(time_pressure), x = mean, xmin = lower.ci, xmax = upper.ci)) +
  geom_point(color = "#42C4C7") +
  geom_errorbarh(height = 0, size = .75, color = "#000000") +
  scale_x_continuous(name = "Average reported percentage of party composed by stereotypical groups", limits = c(30,45)) +
  ylab("Respondents given...") +
  scale_y_discrete(labels = c("0" = "Time requirements", "1" = "Time constraints")) +
  cust_theme
ggsave("figs/fig_5_timing.pdf")
