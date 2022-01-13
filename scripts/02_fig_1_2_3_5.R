### Plotting Typecast

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(reshape2)

# Set Working dir. to project folder
setwd("~/Dropbox/xperceive/typecast/replication/")
setwd(paste0(dropboxdir, "xperceive/typecast/replication/"))

### Figure 1: LINDA
# Read data for James and Linda
data <- read_csv("data/linda_clean.csv")

linda_cond_means <- 
    data[c("linda", "linda_cf")] %>%
    group_by(linda) %>%
    summarize(mean = mean(I(linda_cf == "Linda is a bank teller and is active in the feminist movement") * 100),
              sd = sd(I(linda_cf == "Linda is a bank teller and is active in the feminist movement") * 100),
              count = n())

linda_cond_means$se <- linda_cond_means$sd / sqrt(linda_cond_means$count) 

#Plot
# Custom Theme
cust_theme <- 
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
        panel.grid.minor = element_blank(),
        legend.key.width = unit(.5, "cm"),
        axis.title   = element_text(size = 10, color = "#555555"),
        axis.text    = element_text(size = 10, color = "#555555"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(linda_cond_means, aes(y = as.factor(linda), x = mean, xmin = mean + 1.96*se, xmax = mean - 1.96*se)) +
      geom_point(color = "#42C4C7") +
      geom_errorbarh(height = 0, size = .75, color = "#42C4C7") +
      scale_x_continuous(name = "% respondents saying Linda is a feminist bank teller", 
            limits = c(50, 90)) + 
      ylab("Linda described as...") +
      cust_theme

ggsave("figs/linda_gg.pdf")

### Figure 2: JAMES, MAXIMAL CONTRAST:

# Get Means by Treatment Condition
treat_means <- 
    data[c("james_cf_n", "james_cond_n")] %>%
    group_by(james_cond_n) %>%
    summarize(Salesman = mean(I(james_cf_n == 2)*100), 
            `Republican Salesman` = mean(I(james_cf_n == 3)*100), 
            `Democratic Salesman` = mean(I(james_cf_n == 1)*100))

# Convert to long-form
treat_means_long <- melt(treat_means, id = "james_cond_n", variable.name = "Choice")

# Reorder factors
treat_means_long$Choice <- ordered(treat_means_long$Choice, levels = c("Democratic Salesman", "Salesman", "Republican Salesman"))

# Plot
ggplot(treat_means_long, aes(james_cond_n, value)) +
  geom_bar(aes(fill = Choice), position = "dodge", stat="identity") + 
  scale_y_continuous("Percent Choosing", limits = c(0, 80), labels = function(x) paste0(x, "%")) + 
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  xlab("Experimental condition") +
  cust_theme + 
  theme(legend.position = c(.8, .90),
        legend.background = element_rect(size=0.5, linetype="solid"))

ggsave("figs/fig_2_mturk_prop_by_treat_party_rep.pdf")

### Figure 3: JAMES, FULLY FACTORIAL
james_ff_sum <- read.csv("tabs/james_ff_summary.csv")

ggplot(james_ff_sum, aes(y = treat, x = cf_mean, xmin = cf_min, xmax = cf_max, colour = party)) +
  geom_errorbarh(height = 0, size=.5) +
  scale_colour_manual(values = c("#4393C3", "#D6604D"), guide=guide_legend(nrow=2)) +
  geom_point() +
  scale_x_continuous(name = "Change in predicted probability of committing the...", limits = c(-20, 30)) + 
  ylab("James described as...") +
  geom_vline(xintercept = 0, colour = "#999999") +
  cust_theme +
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid"),
        legend.title = element_blank())
ggsave("figs/james_ff.pdf")

### Figure 5: TIMING EXPERIMENT
# See Stata replication file 01_tab_2_fig_5_pre.do for regression of "time_" absorbing indicators for group-party dyad and clustering SE at resp. level
timing_data <- read_csv("tabs/timing_results.csv")

ggplot(timing_data, aes(y = as.factor(time_pressure), x = timing_cond_means, xmin = timing_cond_means + 1.96*timing_cond_se, xmax = timing_cond_means - 1.96*timing_cond_se)) +
  geom_point(color = "#42C4C7") +
  geom_errorbarh(height = 0, size = .75, color = "#42C4C7") +
  scale_x_continuous(name = "Average reported percentage of party composed by stereotypical groups", limits = c(30,45)) +
  ylab("Respondents given...") +
  scale_y_discrete(labels = c("0" = "Time requirements", "1" = "Time constraints")) +
  cust_theme
ggsave("figs/timing.pdf")
