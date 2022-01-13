*** Replication file for Typecast
*** Douglas Ahler and Gaurav Sood

* First, set a working directory with data
cd "~/Dropbox/xperceive/typecast/replication/data"
	* Remember, replace this with your own path
	
*******************
*** Linda study ***
*******************

* Load data
insheet using "linda_clean.csv", clear names

* How many respondents commit the conjunction fallacy in each condition?
	* Results in text and reported in Figure 1 (see figures script for replication of figure)
ttest linda_cf_n, by(linda)

****************************************
*** James study #1: Maximal Contrast ***
****************************************

* Data still loaded from "linda_clean.csv"

* Produce statistics consistent with Figure 2 (and in-text in the section)
reg james_dem_cf james_dem james_rep
	* Democratic conjunction fallacy
reg james_rep_cf james_dem james_rep
	* Republican conjunction fallacy
reg james_no_cf james_dem james_rep
	* Respondent does not commit conjunction fallacy
	
	*NOTE: constant term == % commiting conjunction fallacy (or not) in control condition
		* james_dem == additional likelihood of committing the particular conjunction fallacy when James is described as representative of Democrats
		* james_rep == additional likelihood of commiting the particular conjunction fallacy when James is described as representative of Republicans
		

****************************************
*** James study #2: Fully Factorial  ***
****************************************	

* Load data
insheet using "james_ff_clean.csv", clear names

* Drop responses that are suspicious, according to procedures from Ahler, Roush, and Sood (forthcoming)
drop if untrustworthy == "TRUE" | untrustworthy == ""
	* Check n
	tab untrustworthy

* Turn string variables indicating conditions into numeric
gen james_black = 0
replace james_black = 1 if cf_race == "black"
		
gen james_gay = 0
replace james_gay = 1 if cf_spouse == "Keith"
		
gen james_lib = 0
replace james_lib = 1 if cf_policy == "living-wage demonstrations"

gen james_con = 0
replace james_con = 1 if cf_policy == "anti-tax demonstrations"
		
gen james_evang = 0
replace james_evang = 1 if cf_relig == "leads his son's Cub Scouts group, organized through the Baptist Church the family attends"

gen james_aa = 0
replace james_aa = 1 if cf_relig == "leads his son's Junior Explorers group, organized through the Secular Families Foundation"

* Produce ordered outcome variable
destring james_cf, replace 

gen dem_cf = .
replace dem_cf = 1 if james_cf == 2
replace dem_cf = 0 if inlist(james_cf, 1, 3)

gen rep_cf = .
replace rep_cf = 1 if james_cf == 3
replace rep_cf = 0 if inlist(james_cf, 1, 2)

recode james_cf (1=0)(2=-1)(3=1), gen(cf_ordered)

* Analysis -- produces numbers for Figure 3 and SI 3
ologit cf_ordered i.james_black i.james_gay i.james_evang i.james_aa i.james_lib i.james_con
		margins, dydx(i.james_black) predict(outcome(-1))
		margins, dydx(james_black) predict(outcome(1))
		margins, dydx(james_gay) predict(outcome(-1))
		margins, dydx(james_gay) predict(outcome(1))
		margins, dydx(james_evang) predict(outcome(-1))
		margins, dydx(james_evang) predict(outcome(1))
		margins, dydx(james_aa) predict(outcome(-1))
		margins, dydx(james_aa) predict(outcome(1))
		margins, dydx(james_lib) predict(outcome(-1))
		margins, dydx(james_lib) predict(outcome(1))
		margins, dydx(james_con) predict(outcome(-1))
		margins, dydx(james_con) predict(outcome(1))
		
		* NOTE: Lines 84-95 used to generate james_ff_summary.csv, used to generate Figure 3 (in R). Marginal estimates are copied from Stata and appropriately rounded to create james_ff_summary.csv  
		
***************************
*** Bayesian Cues study ***
***************************

* Load data
insheet using "bayesian_cues.csv", clear names

gen br_treat = 0 if study == "bayes"
replace br_treat = 1 if bayes_cond == "base_rate"
gen gc_treat = 0 if study == "bayes"
replace gc_treat = 1 if bayes_cond == "group_comp"

rename bayes_rep_rich_dv bayes_dv_rich
rename bayes_rep_evang_dv bayes_dv_evang
rename bayes_dem_union_dv bayes_dv_union
rename bayes_dem_lgb_dv bayes_dv_lgb

reshape long bayes_dv_, i(responseid) j(item) string
	* Converting missing values to "" and destringing DV
	replace bayes_dv_ = "" if bayes_dv_ == "NA"
	destring bayes_dv_, replace
	replace bayes_dv_ = bayes_dv_ / 100

* Table 2, Column 1
areg bayes_dv_ br_treat gc_treat, a(item) cluster(responseid)

* Table 2, Column 2
areg bayes_dv_ i.br_treat##c.num_index i.gc_treat##c.num_index, a(item) cluster(responseid)

****************************************
*** Time pressure/requirements study ***
****************************************

* Load data
use "timing.dta", clear 

* Clean data
keep if inlist(timing_cond, "fast", "slow")

rename timing_dem_black_fast time_black_fast
rename timing_dem_black_slow time_black_slow

rename timing_dem_aa_fast time_aa_fast
rename timing_dem_aa_slow time_aa_slow

rename timing_dem_lgb_fast time_lgb_fast
rename timing_dem_lgb_slow time_lgb_slow

rename timing_dem_union_fast time_union_fast
rename timing_dem_union_slow time_union_slow

rename timing_rep_old_fast time_old_fast
rename timing_rep_old_slow time_old_slow

rename timing_rep_evang_fast time_evang_fast
rename timing_rep_evang_slow time_evang_slow

rename timing_rep_south_fast time_south_fast
rename timing_rep_south_slow time_south_slow

rename timing_rep_rich_fast time_rich_fast
rename timing_rep_rich_slow time_rich_slow

reshape long time_black_ time_aa_ time_lgb_ time_union_ time_old_ time_evang_ time_south_ time_rich_, i(responseid) j(fastslow) string

gen pressure = 0 if timing_cond == "slow"
replace pressure = 1 if timing_cond == "fast"

* Analysis 
reg time_black_ pressure
reg time_aa_ pressure
reg time_lgb_ pressure
reg time_union_ pressure
reg time_old_ pressure
reg time_evang_ pressure
reg time_south_ pressure
reg time_rich_ pressure

reshape long time_, i(responseid fastslow) j(group) string

* Difference between conditions, reported in Figure 5 and in text in section 4
	areg time_ if timing_cond == "slow", a(group) cluster(responseid)
	areg time_ if timing_cond == "fast", a(group) cluster(responseid)
		* NOTE: Output from these mean calculations used to generate timing_results.csv. Regression coefficients (means, without an X variable specified) and standard errors (used to generate 95% confidence intervals) copied over from Stata into timing_results.csv, then used to generate Figure 5 in R. 

