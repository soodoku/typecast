### Plotting Misinfo./Typcast

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(lme4)
library(stargazer)

# Set Working dir. to project folder
setwd("~/Dropbox/xperceive/typecast/replication/")
setwd(paste0(dropboxdir, "xperceive/typecast/replication/"))


# Read in data
pcomp <- read.dta13("data/timing.dta")

# Analyses
# TAKEAWAY: Nothing much to write home about, and certainly not in the direction people think. 
# (I.e., if anything, people become a bit worse when asked "out of 100" instead of about percentages.)
with(pcomp, t.test(pcomp_rep_old_perc, pcomp_rep_old_100))
with(pcomp, t.test(pcomp_rep_rich_perc, pcomp_rep_rich_100))
with(pcomp, t.test(pcomp_rep_evang_perc, pcomp_rep_evang_100))
with(pcomp, t.test(pcomp_rep_south_perc, pcomp_rep_south_100))
with(pcomp, t.test(pcomp_dem_black_perc, pcomp_dem_black_100))
with(pcomp, t.test(pcomp_dem_aa_perc, pcomp_dem_aa_100))
with(pcomp, t.test(pcomp_dem_lgb_perc, pcomp_dem_lgb_100))
with(pcomp, t.test(pcomp_dem_union_perc, pcomp_dem_union_100))

# Summarizing results of measurement/wording for imm. and black/poor items
# Sig. difference in "expected" direction for immigration, but not for black/poor
with(pcomp, t.test(imm_perc, imm_100))
with(pcomp, t.test(black_poor_perc, black_poor_100))

# Summarizing results of soda experiment (untimed)
# Providing the base rate does not improve accuracy. If anything, it makes diff. in perceptions of % of men/women who drink diet soda worse, but p=0.14...
pcomp$soda_dif <- pcomp$soda_women - pcomp$soda_men

with(pcomp, t.test(soda_dif ~ soda_cond))
with(pcomp, t.test(soda_women ~ soda_cond))
with(pcomp, t.test(soda_men ~ soda_cond))
		
# Summarizing results of the timing study -- P-Comp
# IMPROVEMENT IN ALL 8 WITH SLOW TIME (Comparison to non-timed quesitons not quite as clean, but generaly non-timed responses fall between "fast" and "slow")
with(pcomp, t.test(timing_dem_black_fast, timing_dem_black_slow))
with(pcomp, t.test(timing_dem_aa_fast, timing_dem_aa_slow))
with(pcomp, t.test(timing_dem_lgb_fast, timing_dem_lgb_slow))
with(pcomp, t.test(timing_dem_union_fast, timing_dem_union_slow))
with(pcomp, t.test(timing_rep_old_fast, timing_rep_old_slow))
with(pcomp, t.test(timing_rep_evang_fast, timing_rep_evang_slow))
with(pcomp, t.test(timing_rep_south_fast, timing_rep_south_slow))
with(pcomp, t.test(timing_rep_rich_fast, timing_rep_rich_slow))

# Summarizing results of measurement/wording for imm. and black/poor in timing study
with(pcomp, t.test(timing_imm_fast, timing_imm_slow))
with(pcomp, t.test(timing_blackpoor_fast, timing_blackpoor_slow))

# Soda study in timing experiment
# Not much to write home about... Implies stereotypes are pretty powerful?
with(pcomp, t.test(timing_soda_women_fast, timing_soda_women_slow))
with(pcomp, t.test(timing_soda_men_fast, timing_soda_men_slow))
with(pcomp, t.test(timing_soda_br_fast, timing_soda_br_slow))
# 	*No difs
pcomp$soda_dif = ifelse(pcomp$timing_cond == "fast", pcomp$timing_soda_women_fast - pcomp$timing_soda_men_fast,  
						ifelse(pcomp$timing_cond == "slow", pcomp$timing_soda_women_slow - pcomp$timing_soda_men_slow, NA))

with(pcomp[pcomp$timing_cond != "", ], summary(lm(soda_dif ~ timing_cond)))

# Soda Analysis in the Chapter
pcomp$fast <- ifelse(pcomp$timing_cond == "fast", 1, 0)
pcomp$slow <- ifelse(pcomp$timing_cond == "slow", 1, 0)
pcomp$br_shown <- ifelse(pcomp$soda_cond == "br", 1, 0)

# Analysis
with(pcomp, summary(lm(soda_dif ~ br_shown))) ##fast br_shown##slow

# 	*Priming the base rate fails to reduce reliance on stereotypes. In fact, it almost makes people significantly more stereotype-biased. 
#	*But...
# People do reduce their belief about % of women who drink diet soda when forced to think harder after being asked about the base rate. Not really true for perceptions about men drinking diet soda.

pcomp$soda_women[is.na(pcomp$soda_women)] <- pcomp$timing_soda_women_fast[is.na(pcomp$soda_women)]
pcomp$soda_women[is.na(pcomp$soda_women)] <- pcomp$timing_soda_women_slow[is.na(pcomp$soda_women)]

pcomp$soda_men[is.na(pcomp$soda_men)] <- pcomp$timing_soda_men_fast[is.na(pcomp$soda_men)]
pcomp$soda_men[is.na(pcomp$soda_men)] <- pcomp$timing_soda_men_slow[is.na(pcomp$soda_men)]

# Analysis
with(pcomp, summary(lm(soda_women ~ br_shown))) 

# Recoding
pcomp$soda_br[is.na(pcomp$soda_br)] <- pcomp$timing_soda_br_fast[is.na(pcomp$soda_br)]
pcomp$soda_br[is.na(pcomp$soda_br)] <- pcomp$timing_soda_br_slow[is.na(pcomp$soda_br)]

pcomp$implied_pct <- .5*pcomp$soda_men + .5 * pcomp$soda_women
pcomp$implied_pct <-  abs(pcomp$soda_br - pcomp$implied_pct)

# Bayes
# *Bayes study DVs -- a quick summary -- SEEMS REALLY PROMISING

tidy_pcomp <- pcomp %>%
	filter(bayes_cond != "") %>%
	dplyr::select(bayes_cond, bayes_rep_rich_dv, bayes_rep_evang_dv, bayes_dem_lgb_dv, bayes_dem_union_dv) %>%
	gather(var_name, var_value, bayes_rep_rich_dv:bayes_dem_union_dv, -bayes_cond) %>%
	group_by(bayes_cond, var_name) %>%
	summarize(so_mean = mean(var_value, na.rm = T), se_var = sd(var_value, na.rm = T)/sqrt(n()))

tidy_pcomp$bayes_cond <- plyr::revalue(tidy_pcomp$bayes_cond, c("base_rate" = "Base rates", "control" = "Control", "group_comp" = "GC"))
tidy_pcomp$var_name   <- plyr::revalue(tidy_pcomp$var_name, c("bayes_dem_lgb_dv" = "Democrats: Gay, Lesbian, or Bisexual",
															  "bayes_dem_union_dv" = "Democrats: Union Members",
															   "bayes_rep_evang_dv" = "Republicans: Evangelicals",
															   "bayes_rep_rich_dv" = "Republicans: $250k/year or more"))

# Plot
# Custom Theme
cust_theme <- 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(.5, "cm"),
        strip.text.x = element_text(size = 12),
        axis.title   = element_text(size = 12, color = "#555555"),
        axis.text    = element_text(size = 12, color = "#555555"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(tidy_pcomp, aes(bayes_cond, so_mean)) +
  geom_point() + 
  facet_wrap(~ tidy_pcomp$var_name) + 
  scale_y_continuous("Perceived Share", limits = c(0, 60), labels = function(x) paste0(x, "%")) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  xlab("Experimental condition") +
  geom_errorbar(aes(ymin = so_mean - 2*se_var, ymax = so_mean + 2*se_var), width = .05) + 
  cust_theme + 
  theme(legend.position = c(.8, .90),
        legend.background = element_rect(size=0.5, linetype="solid"))
ggsave("figs/fig_4_bayes.pdf")

# More recoding
pcomp$br_treat[pcomp$study == "bayes"] <- 0 
pcomp$br_treat <-  ifelse(pcomp$bayes_cond == "base_rate", 1, 0)

pcomp$gc_treat[pcomp$study == "bayes"] <- 0 
pcomp$gc_treat <-  ifelse(pcomp$bayes_cond == "group_comp", 1, 0)

small_pcomp <- pcomp %>%
	filter(bayes_cond != "") %>%
	dplyr::select(bayes_cond, bayes_rep_rich_dv, bayes_rep_evang_dv, bayes_dem_lgb_dv, bayes_dem_union_dv, responseid, br_treat, gc_treat, num_index) %>%
	gather(var_name, var_value, bayes_rep_rich_dv:bayes_dem_union_dv, -bayes_cond, -responseid, -br_treat, -gc_treat, -num_index)

m1 <- with(small_pcomp, lmer(var_value/100 ~ br_treat + gc_treat + (1|responseid) + (1|var_name)))
m2 <- with(small_pcomp, lmer(var_value/100 ~ br_treat*num_index + gc_treat*num_index + (1|responseid) + (1|var_name)))

vars.order <- c("num_index", "br_treat", "br_treat:num_index", "gc_treat", "num_index:gc_treat")

stargazer(m1, m2, style="ajps", 
           title = "Representativeness and innumeracy cause people to over-apply party stereotypes", 
           dep.var.labels = c("DV: Reported Perception of p(group | party)"),
           digits = 2,
           label = "tab:representativeness",
           order=paste0("^", vars.order , "$"),
           covariate.labels=c( "Numeracy", "Base rate treatment", "BR * numeracy", "Group composition treatment", "GC * numeracy", "Constant"),
           out   = "tabs/table_2.tex"
)
