### Plotting Typecast

# Load Libs
library(readstata13)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(reshape2)
library(MASS)
library(margins)
library(erer)
library(car)
library(xtable)

# Set Working dir. to project folder
setwd(paste0(githubdir, "typecast/"))

#****************************************
#*** James study #2: Fully Factorial  ***
#**************************************** 

james <- read_csv("data/james_ff_clean.csv")

# * Drop responses that are suspicious, according to procedures from Ahler, Roush, and Sood (forthcoming)
james_trust <- james %>% filter(untrustworthy == FALSE)
table(james_trust$untrustworthy, useNA = "always")

#* Turn string variables indicating conditions into numeric
james_trust$james_black  <- james_trust$cf_race == "black"
james_trust$james_white  <- james_trust$cf_race == "white"
james_trust$james_gay    <- james_trust$cf_spouse == "Keith"
james_trust$james_straight  <- james_trust$cf_spouse == "Karen"
james_trust$james_lib    <- james_trust$cf_policy == "living-wage demonstrations"
james_trust$james_con    <- james_trust$cf_policy == "anti-tax demonstrations"
james_trust$james_noideo <- james_trust$cf_policy == "student government"
james_trust$james_evang  <- james_trust$cf_relig == "leads his son's Cub Scouts group, organized through the Baptist Church the family attends"
james_trust$james_aa     <- james_trust$cf_relig == "leads his son's Junior Explorers group, organized through the Secular Families Foundation"
james_trust$james_norel  <- james_trust$cf_relig == "coaches his son's youth sports teams"

james_trust <-james_trust %>%
  mutate(dem_cf = case_when(
      james_cf == 2 ~ 1,
      james_cf == 1 | james_cf == 3 ~ 0
      ))

james_trust <-james_trust %>%
  mutate(rep_cf = case_when(
      james_cf == 3 ~ 1,
      james_cf == 1 | james_cf == 2 ~ 0
      ))

james_trust <-james_trust %>%
  mutate(james_cf_ord = case_when(
      james_cf == 1 ~ 0,
      james_cf == 2 ~ -1,
      james_cf == 3 ~ 1
      ))

mo  <- polr(as.factor(james_cf_ord) ~ james_black + james_gay + james_evang + james_aa + james_lib + james_con, data = james_trust, Hess = TRUE, method = "logistic")
mo2 <- polr(as.factor(james_cf_ord) ~ james_black*james_gay*james_evang*james_aa*james_lib*james_con, data = james_trust, Hess = TRUE, method = "logistic")

summary(mo2)

stargazer(mo, style="ajps", 
           title = "Full model results for the fully-factorial ``James'' experiment", 
           dep.var.labels = c("\\shortstack{DV: Democratic (+1) \\\\ or Republican (-1) conjunction fallacy}"),
           digits = 2,
           label = "tab:logit_james",
           covariate.labels=c("Black (vs. white)", "Gay (vs. straight)", "Evangelical (vs. nothing)", "Secular (vs. nothing)", "Liberal (vs. nothing)", "Conservative (vs. nothing)"),
           out   = "tabs/table_si_31_james_ff.tex"
)

margins_summary(mo)
margins_summary(mo, at = list(james_black = TRUE, james_cf_ord = -1))

x <- ocME(mo)

# Plotting
### Figure 3: JAMES, FULLY FACTORIAL
### Table SI 3.2

res <- data.frame(treat = NA, cf_mean = NA, cf_min = NA, cf_max = NA, party = NA)
res[1:12, "treat"]    <- rep(rownames(x$out[1][[1]]), 2)
res$treat <- car::recode(res$treat, "'james_blackTRUE' = 'Black (vs. white)'; 
                                     'james_gayTRUE'   = 'Gay (vs. straight)';
                                     'james_evangTRUE' = 'Evangelical (vs. no cue)';
                                     'james_aaTRUE'    = 'Secular (vs. no cue)';
                                     'james_libTRUE'   = 'Liberal (vs. no cue)';
                                     'james_conTRUE'   = 'Conservative (vs. no cue)'")

res$party[1:6]  <- "Democratic conjunction fallacy"
res$party[7:12] <- "Republican conjunction fallacy"

res[1:6,  "cf_mean"] <- x$out[1][[1]][, c(1)]*100 
res[7:12, "cf_mean"] <- x$out[3][[1]][, c(1)]*100

res[1:6, c("cf_min", "cf_max")]  <- cbind(res$cf_mean[1:6]  - 1.96*x$out[1][[1]][, c(2)]*100,  res$cf_mean[1:6] + 1.96*x$out[1][[1]][, c(2)]*100)
res[7:12, c("cf_min", "cf_max")] <- cbind(res$cf_mean[7:12] - 1.96*x$out[3][[1]][, c(2)]*100, res$cf_mean[7:12] + 1.96*x$out[3][[1]][, c(2)]*100)

res <- res %>% arrange(res$treat)


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


ggplot(res, aes(y = treat, x = cf_mean, xmin = cf_min, xmax = cf_max, colour = party)) +
  geom_errorbarh(height = 0, size=.5) +
  scale_colour_manual(values = c("#aaaaaa", "#000000"), guide=guide_legend(nrow=2)) +
  geom_point() +
  scale_x_continuous(name = "Change in predicted probability of committing the...", limits = c(-20, 30)) + 
  ylab("James described as...") +
  geom_vline(xintercept = 0, colour = "#777777") +
  cust_theme +
  theme(legend.position = "bottom",
        legend.background = element_rect(size=0.5, linetype="solid"),
        legend.title = element_blank())
ggsave("figs/fig_3_james_ff.pdf")

# SI Table 3.2

res$party <- car::recode(res$party, "'Democratic conjunction fallacy' = 'Dem-CF'; 'Republican conjunction fallacy' = 'Rep-CF'")
res$treat <- paste(res$treat, res$party)
table_si_32 <- res[, c(1:4)]

names(table_si_32) <- c("When James is described as", "Effect", "Lower CI", "Upper CI")
print(
        xtable(table_si_32,
          digits = 2,
          align = "llccc",
          caption = "Model coefficients converted to changes in predicted probabilities in the fully-factorial ``James'' experiment", 
          label = "tab:tab:logit_james_pp"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\small", 
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "tabs/table_si_32_james_ff.tex")

# SI Table 3.3

james_l <- james_trust %>%
  dplyr::select(james_black, james_white, james_gay, james_straight, james_evang, james_aa, james_norel, james_lib, james_con, james_noideo, james_cf_ord) %>%
  gather(var_name, var_value, james_black:james_noideo, -james_cf_ord)

james_r <- james_trust %>%
  filter(james_trust$pid3 == "republican") %>%
  dplyr::select(james_black, james_white, james_gay, james_straight, james_evang, james_aa, james_norel, james_lib, james_con, james_noideo, james_cf_ord) %>%
  gather(var_name, var_value, james_black:james_noideo, -james_cf_ord)

james_d <- james_trust %>%
  filter(james_trust$pid3 == "democrat") %>%
  dplyr::select(james_black, james_white, james_gay, james_straight, james_evang, james_aa, james_norel, james_lib, james_con, james_noideo, james_cf_ord) %>%
  gather(var_name, var_value, james_black:james_noideo, -james_cf_ord)

james_i <- james_trust %>%
  filter(james_trust$pid3 == "independent") %>%
  dplyr::select(james_black, james_white, james_gay, james_straight, james_evang, james_aa, james_norel, james_lib, james_con, james_noideo, james_cf_ord) %>%
  gather(var_name, var_value, james_black:james_noideo, -james_cf_ord)


dem_rep_all <- james_l %>% 
              group_by(group = paste0(var_name, var_value)) %>% 
              summarize(mean = mean(james_cf_ord == -1)) %>% 
              filter(grepl("TRUE", group))

rep_rep_all <- james_l %>% 
               group_by(group = paste0(var_name, var_value)) %>% 
               summarize(mean = mean(james_cf_ord == 1)) %>% 
               filter(grepl("TRUE", group))

dem_rep_dem <- james_d %>% 
              group_by(group = paste0(var_name, var_value)) %>% 
              summarize(mean = mean(james_cf_ord == -1)) %>% 
              filter(grepl("TRUE", group))

rep_rep_dem <- james_d %>% 
               group_by(group = paste0(var_name, var_value)) %>% 
               summarize(mean = mean(james_cf_ord == 1)) %>% 
               filter(grepl("TRUE", group))

dem_rep_rep <- james_r %>% 
              group_by(group = paste0(var_name, var_value)) %>% 
              summarize(mean = mean(james_cf_ord == -1)) %>% 
              filter(grepl("TRUE", group))

rep_rep_rep <- james_r %>% 
               group_by(group = paste0(var_name, var_value)) %>% 
               summarize(mean = mean(james_cf_ord == 1)) %>% 
               filter(grepl("TRUE", group))

dem_rep_ind <- james_i %>% 
              group_by(group = paste0(var_name, var_value)) %>% 
              summarize(mean = mean(james_cf_ord == -1)) %>% 
              filter(grepl("TRUE", group))

rep_rep_ind <- james_i %>% 
               group_by(group = paste0(var_name, var_value)) %>% 
               summarize(mean = mean(james_cf_ord == 1)) %>% 
               filter(grepl("TRUE", group))


table_si_33 <- data.frame(label = 1:20, 'fs' = NA, Democrats = NA, Independents = NA, Republicans = NA)


base  <- car::recode(dem_rep_all$group, "'james_blackTRUE' = 'Black';
                                         'james_whiteTRUE' = 'White';
                                         'james_evangTRUE' = 'Evangelical';
                                         'james_aaTRUE' = 'Secular';
                                         'james_norelTRUE' = 'No relig. cue';
                                         'james_gayTRUE'   = 'Gay';
                                         'james_straightTRUE' = 'Straight';
                                         'james_conTRUE' = 'Conserv.';
                                         'james_libTRUE' = 'Liberal';
                                         'james_noideoTRUE' = 'No ideo cue'")

table_si_33$label[seq(1, 20, 2)] <- paste0(base, "-Dem. CF")
table_si_33$label[seq(2, 20, 2)] <- paste0(base, "-Rep. CF")

table_si_33$fs[seq(1, 20, 2)] <- c(dem_rep_all$mean)
table_si_33$fs[seq(2, 20, 2)] <- c(rep_rep_all$mean)

table_si_33$Democrats[seq(1, 20, 2)] <- c(dem_rep_dem$mean)
table_si_33$Democrats[seq(2, 20, 2)] <- c(rep_rep_dem$mean)

table_si_33$Independents[seq(1, 20, 2)] <- c(dem_rep_ind$mean)
table_si_33$Independents[seq(2, 20, 2)] <- c(rep_rep_ind$mean)

table_si_33$Republicans[seq(1, 20, 2)] <- c(dem_rep_rep$mean)
table_si_33$Republicans[seq(2, 20, 2)] <- c(rep_rep_rep$mean)

table_si_33 <- table_si_33[c(3, 4, 19, 20, 9, 10, 17, 18, 7, 8, 1, 2, 15, 16, 11, 12, 5, 6, 13, 14), ]

names(table_si_33)[c(1, 2)] <- c("Group-CF dyad", "Full sample")
print(
        xtable(table_si_33,
          digits = 2,
          align = "llcccc",
          caption = "Marginal rates at which conjunction fallacies (CFs) are committed, given particular traits of ``James''", 
          label = "tab:james_marginal"), 
        include.rownames = FALSE,
        include.colnames = TRUE, 
        size="\\small", 
        type = "latex", 
        sanitize.text.function = function(x){x},
        caption.placement = "top",
        table.placement = "!htb",
        file = "tabs/table_si_33_james_ff.tex")
