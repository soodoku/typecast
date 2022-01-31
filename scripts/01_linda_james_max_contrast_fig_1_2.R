### Plotting Typecast

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(reshape2)

# Set Working dir. to project folder
setwd(paste0(githubdir, "typecast/"))

# Read data for James and Linda
data <- read_csv("data/linda_clean.csv")

# How many respondents commit the conjunction fallacy in each condition?
# * Results in text and reported in Figure 1 (see figures script for replication of figure)
with(data, t.test(linda_cf_n ~ linda))

### Figure 1: LINDA
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
        legend.text           = element_text(color = "grey30"),
        legend.title          = element_text(size = 12),
        axis.title   = element_text(size = 10, color = "grey30"),
        axis.text    = element_text(size = 10, color = "grey30"),
        axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(vjust = 1),
        axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
        axis.text.x  = element_text(vjust = .3),
        plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(linda_cond_means, aes(y = as.factor(linda), x = mean, xmin = mean + 1.96*se, xmax = mean - 1.96*se)) +
      geom_point(color = "grey30") +
      geom_errorbarh(height = 0, size = .75, color = "grey30") +
      scale_x_continuous(name = "% respondents saying Linda is a feminist bank teller", 
            limits = c(50, 90)) + 
      ylab("Linda described as...") +
      cust_theme

ggsave("figs/fig_1_linda_gg.pdf")

#****************************************
#*** James study #1: Maximal Contrast ***
#****************************************
#* Data still loaded from "linda_clean.csv"
#* Produce statistics consistent with Figure 2 (and in-text in the section)

#* Democratic conjunction fallacy
with(data, summary(lm(james_dem_cf ~ james_dem + james_rep)))
# * Republican conjunction fallacy
with(data, summary(lm(james_rep_cf ~ james_dem + james_rep)))
# * Respondent does not commit conjunction fallacy
with(data, summary(lm(james_no_cf ~ james_dem + james_rep)))
with(data, summary(lm(james_no_cf ~ I(james_dem|james_rep))))

  
#*NOTE: constant term == % commiting conjunction fallacy (or not) in control condition
#* james_dem == additional likelihood of committing the particular conjunction fallacy when James is described as representative of Democrats
#* james_rep == additional likelihood of commiting the particular conjunction fallacy when James is described as representative of Republicans

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
  scale_fill_grey() +
  xlab("Experimental condition") +
  cust_theme + 
  theme(legend.position = c(.8, .90),
        legend.background = element_rect(size=0.5, linetype="solid"))

ggsave("figs/fig_2_james_max_contrast.pdf")
