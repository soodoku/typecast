## Avatars

## Replication file for Typecast
## Douglas Ahler and Gaurav Sood

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(car)
library(reshape2)
library(MASS)
library(erer)
library(margins)

# Set Working dir. to project folder
setwd("~/Dropbox/xperceive/typecast/replication/")
setwd(paste0(dropboxdir, "xperceive/typecast/replication/"))

# Load data
avatars <- read.dta13("data/turk_recoded_old.dta")

# Recoding
avatars$troll        <- ifelse(avatars$troll_count_2 == "TRUE", 1, 0)
avatars$trolling     <- ifelse(avatars$trolling == "NA", "", avatars$trolling)
avatars$trolling_cat <- car::recode(avatars$trolling, "c('1', '2', '3') = 0; c('4', '5') = 1")

#/*VARIABLE INFO 
#blacklisted = IP appears on a blacklist
#missing_ip = IP not recorded by Qualtrics; only 2 in the dataset
#duplicated = duplicated IP
#foreign_ip = IP recorded as originating from outside U.S.
# funny_ip = any of the above
# troll_count_2 = respondent answered 2 or more low-incidence screeners
# untrustworthy = if funny_ip=="TRUE"|troll_count_2="TRUE" */

avatars$susp   <- ifelse(avatars$untrustworthy == "TRUE", 1, 0)

# Filter by suspect
avatars_small <- subset(avatars, susp != 1)

# Rename
names(avatars_small)[names(avatars_small) == "viz_usa_racewhite"]   <- "viz_usa_white"
names(avatars_small)[names(avatars_small) == "viz_usa_raceblack"]   <- "viz_usa_black"
names(avatars_small)[names(avatars_small) == "viz_usa_raceanotherraceethnicity"] <- "viz_usa_other"

names(avatars_small)[names(avatars_small) == "viz_usa_gendermen"]   <- "viz_usa_men"
names(avatars_small)[names(avatars_small) == "viz_usa_genderwomen"] <- "viz_usa_women"

names(avatars_small)[names(avatars_small) == "viz_dem_racewhite"]   <- "viz_dem_white"
names(avatars_small)[names(avatars_small) == "viz_dem_raceblack"]   <- "viz_dem_black"
names(avatars_small)[names(avatars_small) == "viz_dem_raceanotherraceethnicity"] <- "viz_dem_other"

names(avatars_small)[names(avatars_small) == "viz_dem_gendermen"]   <- "viz_dem_men"
names(avatars_small)[names(avatars_small) == "viz_dem_genderwomen"] <- "viz_dem_women"

names(avatars_small)[names(avatars_small) == "viz_rep_racewhite"]   <- "viz_rep_white"
names(avatars_small)[names(avatars_small) == "viz_rep_raceblack"]   <- "viz_rep_black"
names(avatars_small)[names(avatars_small) == "viz_rep_raceanotherraceethnicity"] <- "viz_rep_other"

names(avatars_small)[names(avatars_small) == "viz_rep_gendermen"]   <- "viz_rep_men"
names(avatars_small)[names(avatars_small) == "viz_rep_genderwomen"] <- "viz_rep_women"

# Recode
avatars_small$viz_dem_women <- as.numeric(ifelse(avatars_small$viz_dem_women == "NA", "", avatars_small$viz_dem_women))
avatars_small$viz_rep_women <- as.numeric(ifelse(avatars_small$viz_rep_women == "NA", "", avatars_small$viz_rep_women))

avatars_small$viz_dem_black <- as.numeric(ifelse(avatars_small$viz_dem_black == "NA", "", avatars_small$viz_dem_black))
avatars_small$viz_rep_black <- as.numeric(ifelse(avatars_small$viz_rep_black == "NA", "", avatars_small$viz_rep_black))

avatars_small$viz_dem_other <- as.numeric(ifelse(avatars_small$viz_dem_other == "NA", "", avatars_small$viz_dem_other))
avatars_small$viz_rep_other <- as.numeric(ifelse(avatars_small$viz_rep_other == "NA", "", avatars_small$viz_rep_other))

avatars_small$viz_dem_nonwhite <- avatars_small$viz_dem_black + avatars_small$viz_dem_other
avatars_small$viz_rep_nonwhite <- avatars_small$viz_rep_black + avatars_small$viz_rep_other

avatars_small$viz_women        <- avatars_small$viz_dem_women - avatars_small$viz_rep_women
avatars_small$viz_nonwhite     <- avatars_small$viz_dem_nonwhite - avatars_small$viz_rep_nonwhite

# Analysis
with(avatars_small[avatars_small$viz_cond == "demsreps_eq",], t.test(viz_dem_nonwhite, viz_rep_nonwhite))
with(avatars_small[avatars_small$viz_cond == "demsreps_eq",], t.test(viz_dem_women, viz_rep_women))

# Long form
avatars_l <- avatars_small %>%
    filter(viz_cond == "demsreps_eq") %>%
	dplyr::select(viz_dem_nonwhite, viz_rep_nonwhite,
		          viz_dem_women, viz_rep_women) %>%
	gather(var_name, var_value, viz_dem_nonwhite:viz_rep_women)

av_sum <- avatars_l %>% 
	group_by(var_name) %>% 
	summarise(mean = mean(var_value, na.rm = TRUE),
              sd   = sd(var_value, na.rm = TRUE),
              n    = n()) %>%
    mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 

av_sum$party <- ifelse(grepl("dem", av_sum$var_name), "Democrats", "Republicans")
av_sum$group <- ifelse(grepl("nonwhite", av_sum$var_name), "People of color", "Women")

av_sum_small <- av_sum %>% dplyr::select(party, group, mean, lower.ci, upper.ci)
av_sum_small$party_n <- car::recode(av_sum_small$party, '"Democrats" = 1; "Republicans" = 0')

cust_theme <- 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.title   = element_text(size = 10, color = "#555555"),
        axis.text    = element_text(size = 10, color = "#555555"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(av_sum_small, aes(y = as.factor(party_n), x = mean, xmin = lower.ci, xmax = upper.ci, colour = party)) +
  geom_point() +
  geom_errorbarh(height = 0, size = .75) +
  scale_colour_manual("", values = c("#000000", "#aaaaaa", "#000000", "#aaaaaa")) +
  scale_x_continuous(name = "Average reported # of group members in party", limits = c(10,13)) +
  ylab("") +
  scale_y_discrete(labels = c("0" = "Democrats", "1" = "Republicans")) +
  cust_theme +
  facet_wrap(~ group, ncol=1, drop = TRUE, scales = "free_y") +
  cust_theme
ggsave("figs/fig_si_41_avatars.pdf")
