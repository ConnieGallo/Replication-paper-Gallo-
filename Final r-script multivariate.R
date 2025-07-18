library(tidyverse)
library(estimatr)
library(here)

i_am("Clusters/cluster_results.R")
#Load in data 
dat <- read.csv(here("Clusters", "clusters.csv"))
dat <- dat %>%
  mutate(sex_f = case_when(sex_f>.9~1,
                           sex_f<.1~0))
dat
table(dat$sex_f, useNA = "always")
# sex_f has 496 males (0), 815 females (1), and 78 missing values (NA)
# add variable to denote nearly-homogeneous gender clusters
dat <- dat %>%
  mutate(sex_f = case_when(sex_f>.9~1,
                           sex_f<.1~0))
# This variable isolates the gender mix effect among the social groups,
# making it impossible to confuse the analysis.

# The authors themselves include the code to replicate this with the clustered data,
# or they give a rationale for why it cannot be replicated (e.g., no data available or because clusters do not apply to this specific analysis).
dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(tract_unemp:tract_college, ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(tract_unemp:tract_college)) %>%
  filter(age_group!="adult") %>%
  mutate(ra_group_factor = factor(ra_group_factor, levels=c("control", "section 8", "experimental"), ordered=TRUE)) %>%
  mutate(ra_group_factor = recode(ra_group_factor, "control"="Control",
                                  "section 8" = "Section 8", "experimental"="Experimental")) %>%
  mutate(name = factor(name, levels=c("tract_poverty", "tract_unemp", "tract_welf", "tract_femalehead", "tract_minority", "tract_college"))) %>%
  mutate(name = recode(name, "tract_college" = "Tract pct. college grad",
                       "tract_femalehead" = "Tract pct. female-headed",
                       "tract_minority" = "Tract pct. minority race",
                       "tract_poverty" = "Tract poverty rate",
                       "tract_unemp" = "Tract unemployment rate",
                       "tract_welf" = "Tract pct. on welfare")) %>%
  mutate(age_group = recode(age_group, "old_kid"="Teens", "young_kid"="Children")) %>%
  ggplot() +
  geom_bar(aes(x=age_group, y=value, group=ra_group_factor, fill=ra_group_factor), stat="identity", position="dodge") + 
  facet_wrap(~name, nrow = 1) + 
  theme_bw() +
  scale_fill_manual(name="Treatment", values=c("black", "gray40", "gray80")) +
  xlab("Age Group") + ylab("Duration-weighted Percentage") + 
  theme(text=element_text(size=14))

# There are no adults here, only children and teens.

#The variables displayed are the rate of poverty in the neighbourhood, rate of unemployment, percentage of welfare-receiving families, percentage of minority residents, and percentage of residents with college degrees. The bars show three treatment groups: black for the control group, dark grey for the Section 8 group, and light grey for the experimental voucher group.

dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(c(grad_hs, attend_coll, work, parent, jail), ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(grad_hs:jail)) %>%
  filter(age_group!="adult") %>%
  mutate(age_group = recode(age_group, "old_kid"="Teens", "young_kid"="Children")) %>%
  mutate(ra_group_factor = factor(ra_group_factor, levels=c("control", "section 8", "experimental"), ordered=TRUE)) %>%
  mutate(ra_group_factor = recode(ra_group_factor, "control"="Control",
                                  "section 8" = "Section 8", "experimental"="Experimental")) %>%
  mutate(name = factor(name, levels=c("grad_hs", "attend_coll", "work", "parent", "jail"), ordered=TRUE)) %>%
  mutate(name = recode(name, "grad_hs"="Graduated HS", "attend_coll"="Attended College",
                       "work" = "Working", "parent"="Parent", "jail"="Been to Jail/Prison")) %>%
  ggplot() +
  geom_bar(aes(x=age_group, y=value, group=ra_group_factor, fill=ra_group_factor), stat="identity", position="dodge") + 
  facet_wrap(~name, nrow = 1) + 
  theme_bw() +
  scale_fill_manual(name="Treatment", values=c("black", "gray40", "gray80")) +
  xlab("Age Group") + ylab("Mean") + 
  theme(text=element_text(size=14))
#Here the examination focuses on participants' individual life courses and five variables: 
#attainment of educational degree (high school diploma), college enrolment, work status, family establishment, and contact with the justice system
# Replicate means from table using clusters:

mns <- dat %>%
  group_by(age_group, ra_group_factor) %>%
  summarize(across(c(tract_unemp:tract_college, grad_hs, attend_coll, work, parent, jail), ~weighted.mean(w=f_wt_totcore98, x=., na.rm=T))) %>%
  pivot_longer(cols=c(tract_unemp:tract_college,grad_hs:jail)) %>%
  filter(age_group!="adult") %>%
  pivot_wider(id_cols=c(age_group, name), names_from=ra_group_factor, values_from=value)

# Omit calculating p-values using clusters, as DF and family clusters can't be preserved
# This version does only calculate weighted means, no statistical testing (p-values) is done.


dat %>%
  group_by(age_group) %>%
  summarize(matched = mean(matched, na.rm=T))
# Figure 2 cannot exactly be recreated with this data, because it requires
# sorting the data by matching status (i.e., was the person matched
# to the voter file or not), and this cannot fully be achieved using the data available.
# NOTE: This code will not exactly reproduce the original results
# because it does not include clusters (families) which are not gender-homogeneous

dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(matched))

# Calculates the average percentage of individuals matched with the voter file,
# divided according to age group and gender.
# It is used to establish if matching becomes less effective for some groups.
# It is NOT possible to cluster by marital status,
# because it is not homogeneous in the family clusters.

# Table showing that men have, in total,
# a lower posterior matching score than females.
dat %>%
  group_by(sex_f, age_group) %>%
  summarize(mean(posterior, na.rm = TRUE))

# APPENDICE A5
# CANNOT be replicated exactly with this data,
# since calculating means by cluster (family) alters individual outcome distributions.
# TABELLA 1
dat %>%
  group_by(ra_group_factor) %>%
  summarize(
    matched      = mean(matched),
    evervote     = mean(evervoted_post),
    voterate     = mean(r_postturnout),
    votepostreg  = mean(r_postregturnout, na.rm = TRUE)
  )

# Summary of political participation by treatment group, Table 1

# Neither Experimental nor Section 8 groups experience significant increases
# in political participation compared to the Control group.

# Experimental group (voucher, low-poverty neighborhood) has essentially the same values
# on all measures (e.g., turnout: 3.81% both) as the Control group, which
# suggests that better neighborhoods alone do not expand voting.

# The Section 8 group performs worse (e.g., turnout: 3.32%),
# proposing mobility without local area quality would be hurtful to participation.

# Conclusion: Shelter aid alone will not work —
# political participation likely needs more social and economic aid.

# Appendix A3 

# recall that standard errors are NOT preserved in cluster creation
# note that models with controls will vary more from full data estimates due to additional error from averaging control variables within clusters

# declare covariates 
covs_ad <- c("x_f_ad_36_40", "x_f_ad_41_45", "x_f_ad_46_50",
             "x_f_ad_edged", "x_f_ad_edgradhs", "x_f_ad_edgradhs_miss", "x_f_ad_edinsch",
             "x_f_ad_ethn_hisp", "x_f_ad_le_35", "x_f_ad_male",
             "x_f_ad_nevmarr", "x_f_ad_parentu18", "x_f_ad_race_black", "x_f_ad_race_other", "x_f_ad_working",
             "x_f_hh_afdc", "x_f_hh_car", "x_f_hh_disabl", "x_f_hh_noteens",
             "x_f_hh_size2", "x_f_hh_size3", "x_f_hh_size4", "x_f_hh_victim",
             "x_f_hood_5y", "x_f_hood_chat", "x_f_hood_nbrkid", "x_f_hood_nofamily",
             "x_f_hood_nofriend", "x_f_hood_unsafenit", "x_f_hood_verydissat",
             "x_f_hous_fndapt", "x_f_hous_mov3tm", "x_f_hous_movdrgs", "x_f_hous_movschl", "x_f_hous_sec8bef",
             "x_f_site_balt", "x_f_site_bos", "x_f_site_chi", "x_f_site_la")
# covs_ad variable stores all the covariates used to condition against the pre-treatment
# characteristics of individuals/families in statistical models that estimate the
# effect of a treatment (e.g., moving to a different neighborhood with a voucher).

# TABLE A 3.1
mod1 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

# Outcome: matched
# Whether individuals appear in voter rolls (1 = yes, 0 = no).

# Adults: No significant effect (+0.0067, p = 0.60).
# Treated status didn't affect appearing in records.

# Older children: Strong negative effect (–0.0478, p = 0.025).
# Treated children were less likely to appear in voter rolls as adults.

# Young children: No effect (+0.018, p = 0.19).
# Treatment didn't change adult match rates.

f1 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("matched~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

# We check if membership in the treatment group has an effect on the likelihood of being in voter records,
# controlling for things like age, gender, education, and site.

# This is important because if the treatment affects how likely people are to be visible in the data,
# follow-up effects (e.g., voting) could be biased.

# Result: No effect.
# Membership in the treatment group does not have any effect on the probability of being matched.
# This would mean that later analyses of votes are not biased by differences in visibility.

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)

# Comparable to age group analysis
# Adults: no treatment effect. Males more likely; having kids or being in small families decreases the probability.
# Older kids: no treatment effect. Males more likely; having fewer neighbors decreases visibility.
# Young kids: minimal positive effect for experimental group. Males more likely; younger adults (≤35) less likely.
# Conclusion: treatment only affects matching for young children. Gender is the best predictor for all groups.

# TABLE A 3.2
f1 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("r_postturnout~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

# These lines define the formulas for three individual regressions.
# The aim is to see how membership of a treatment group (ra_group_factor)
# influences the chances of voting (r_postturnout),
# controlling for a lot of other personal factors (such as age, gender, education, etc.).

# Separate models are run for three groups:
# - f1: adults
# - f2: adolescents (now adults)
# - f3: children (now adults)

# It helps to look at if the effect of the treatment is dependent on age during moving time.
# Controlling for the other characteristics ensures that the contrast between groups is fair.

mod1 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

# Treatment effects on voter turnout after registration

# For adults, treatment does not have a significant impact on turnout.
# Adolescents in the experiment vote less, but this effect is lost
# when additional controls are added. Men vote more, and not having friends in the area reduces turnout.

# For young children, treatment actually increases turnout to some extent. 
# Treatment effects are small in general. The single positive effect that appears is for young children in the treatment group.
# Other personal and social factors play a more important role in voting.

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)
# The MTO treatment has minimal effect on voting of adults,
# with residence in Baltimore having a weak positive effect,
# and Los Angeles negatively.

# Among teens, the experimental treatment strongly lowers turnout.
# Among children, the experimental treatment modestly increases political activism.
# Male gender always increases turnout among youth.
# Social context variables, such as neighborhood peers,
# are important to adolescents.

# TABLE A 3.3

f1 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f2 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))
f3 <- as.formula(paste0("evervoted_post~", paste0(covs_ad, sep="+", collapse=""), "ra_group_factor"))

mod1 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod5 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod1)
summary(mod3)
summary(mod5)

# Model objective:
# Predict the treatment effect of the MTO treatment on probability to vote since treatment,
# by age group: adults, teens ("old_kid"), and young children ("young_kid"), adjusting for assignment location.
#
# Results:
# - No significant treatment effect for adults and young kids.
# - Significant negative treatment effect for teens: they are less likely to have voted.
# - The voters in the home city decide: compared to New York, the turnout of voters in other cities
#   (especially Baltimore, Los Angeles, and Chicago) is lower.

mod2 <- lm_robust(f1, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod4 <- lm_robust(f2, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod6 <- lm_robust(f3, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])

summary(mod2)
summary(mod4)
summary(mod6)

# The hypothesis was to test whether treatment effects seen in simple models
# persist after accounting for other important factors.

# For adults, the treatment effect still remains near zero and not statistically significant,
# meaning no detectable effect on their likelihood of voting.

# For adolescents (old children), the negative treatment effect persists with controls,
# i.e., individuals treated as teenagers vote less when they are adults.
# This suggests a potential long-run negative impact for this subgroup.

# For young children, the treatment impact is slightly positive and borderline significant,
# indicating that the program has a small positive impact on political activity
# for individuals who were very young at the time of assignment.


# TABLE A 3.4 cannot be duplicated since the clusters cannot be restricted by
# match status, which is necessary in order to narrow the analysis down to
# registered participants.

# Figures 4 and 5 of the paper cannot be replicated because the models with
# clustered standard errors do not work for subgroups (e.g., compliance stage,
# gender, or race). Nor do the post-registration turnout models deal
# with the clusters. So we refer to the supplementary tables (Appendix A3, p. 31 onwards)
# for subgroup findings in detail.

dat_all_groups <- dat


# create subgroup dataframes
boys <- dat %>% filter(sex_f==0)
girls <- dat %>% filter(sex_f==1)
baltimore <- dat %>% filter(ra_site==1)
boston <- dat %>% filter(ra_site==2)
chicago <- dat %>% filter(ra_site==3)
la <- dat %>% filter(ra_site==4)
nyc <- dat %>% filter(ra_site==5)
# In summary, you have created separate datasets to perform specific analyses by gender and by city.

dat <- girls # compare to tables starting on page 31

#table 21: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Impact of treatment on matching in voter files

# Adults: no noticeable impact.
# Teenagers: significant negative influence (–4.8%), less likely to exist in archives.
# Children: no significant impact.

# Teens alone have a chronic negative impact on visibility in the voter file.

## table 22: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Treatment effect on turnout (registered voters)

# Adults: none.
# Teens: small significant reduction (–1.9%).
# Young children: none.

# Only teens have reduced turnout after treatment.

## table 23: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Ever voting effect of treatment after intervention
# Adults: no significant effect. Treatment had no impact on probability of voting at least once.
# Adolescents: significant negative effect (–5.1%) for treatment group.
# Strongest and most concerning result—least likely to have voted as adults.
# Children: no effect of either treatment.

# Conclusion: adolescents are the only group showing a clear negative effect on ever voting.

# table 24 cannot be replicated due to subsetting
# assign second subgroup and run all models (note: not enough male adults to run adult models)
dat <- boys #models for comparison start on page 34

#table 25: matching to voter file/registration
mod2 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(matched~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)
# GOAL:
# To estimate the causal effect of reduction in area poverty on political participation,
# with treatment assignment as an instrument.

# Objective:
# Assess MTO treatment impacts on males' voter record matching (matched),
# particularly for adolescents and children.

# Male Adolescents:
# Large negative effect (–6%, p = 0.044),
# showing reduced registration in treated teen boys.

# Male Children:
# Small positive, non-significant effect (+3.5%, p = 0.070).

# Overall:
# Treatment damages boys treated as teens but not younger boys,
# showing age at treatment is significant.


## table 26: voting rate posttreatment
mod2 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(r_postturnout~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)
# Goal:  
# Test whether MTO treatment had an effect on voting turnout (r_postturnout)  
# for male adolescents and male children.

# Male Adolescents:
# - Treatment effect: –1.9 percentage points, p = 0.0504.
# - Just barely on the borderline of significance, consistent with prior evidence:
#   experimental group adolescent boys voted less as adults.

# Male Children:
# - Treatment effect: +0.7 percentage points, p = 0.25.
# - Not statistically significant — no sign of voting effect here.


# Summary:
# Once more, political cost of gender of MTO emerges in the case of teen boys,
# while boys being treated as children seem unaffected.
# Suggests that timing of relocation matters a lot for longer-term civic engagement.

## table 27: predict ever voted posttreatment
mod2 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",] )
mod3 <- lm_robust(evervoted_post~ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",] )
summary(mod2)
summary(mod3)

# These models predict the impact of experimental assignment (ra_group_factor)
# on whether or not they had ever voted after the treatment (evervoted_post),
# for old_kid and young_kid groups. The models control for varying sites (factor(ra_site))
# and apply experimental weights (f_wt_totcore98).
# The results are summarized in Table 27 of the appendix.
# repeat for each site group; comparison models start on page 46

dat <- baltimore
#dat <- boston
#dat <- chicago
#dat <- la 
#dat <- nyc

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
# This analysis takes into account whether the MTO program had an effect on ever voting probability among boys.

# In the case of teen males, the effect is clearly negative and statistically significant:
# those in the treatment group were approximately 5.8 percentage points less likely to vote as adults.

# For younger boys when moved, there's no effect—
# the estimate is a bit positive but not statistically significant.

# Generally, the program is determined to have lowered political involvement mainly among adolescent boys
# who were the age when intervention was conducted.

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# This test confirms if the MTO program treatment effect on voter turnout (r_postturnout)
# differs when looking at one city (e.g., Baltimore) as opposed to the whole sample.

# For adults in this city, the model can't be estimated—presumably small sample—so no conclusions here.

# Within adolescents, the treatment is negative, as with national estimates,
# but only marginally significant, presumably because of fewer numbers.

# For children, there's still no significant effect, confirming earlier studies at city level.


## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Target: test MTO effect on ever voting in one city, no controls.

# Adults: model fails, no data.
# Adolescents: adverse effect (–5.3%), p = 0.084, trend but not significant.
# Children: no effect, verifies null result.

# First filter by city, for example dat <- dat_full %>% filter(ra_site == 1)


dat <- boston
#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Target: confirm MTO effect on matching in Boston.

# Adults: none.
# Adolescents: negative trend, stronger for Section 8 but not quite significant.
# Children: no effect.
# Treatment has little impact overall in Boston, typically a weak negative signal for teens.


## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Goal: estimate MTO treatment effect on turnout (r_postturnout) in Boston.

# Adults: no effect from either treatment.
# Teens: moderate negative effect, more so for Section 8, but only barely significant.
# Children: no effect detected.

# No general treatment impact on participation in Boston, though teenagers have a weak downward trend.

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# Objective: examine MTO effect on ever voting (evervoted_post) in Boston.

# Adults: no significant effect from either treatment.
# Adolescents: Section 8 has a significant negative effect (–8.3 points),
# Experimental group's effect is not significant but negative.
# Children: no significant effect detected.

# Conclusion: In Boston, there exists only one definite effect which is Section 8's negative effect on adolescents' voting.

#My extension:this replication code does not provide the analysis for all the five cities. I did it.  
dat <- chicago

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE: OLS estimate of MTO effect on `matched` in Chicago (ra_site == 3)

# Adults: No effect. Small coefficients and not significant.
# Adolescents: Weakly negative trend (~-5.6 pp), but not significant (p > 0.25).
# Children: Null effects. Coefficients near zero, large p-values.
# CONCLUSION: In Chicago too, MTO had no clear effect on `matched`

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE: Assess MTO impact on Chicago turnout by vote (`r_postturnout`) (ra_site == 3)

# Adults: None. Small and negative coefficients (p > 0.5).
# Adolescents: Negative slope (-2.2 to -2.6 pp), not significant (p > 0.1).
# Consistent direction with more general youth disengagement results.
# Children: Small positive coefficients, not significant.

# CONCLUSION: No discernible MTO impact on Chicago turnout by age in Chicago.
#Youth show a persistent but not meaningful negative trend.

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE: Test MTO effect on `evervoted_post` in Chicago (ra_site == 3)

# Adults: No effect. Coefficients small and insignificant.
# Adolescents: ~5 pp decline in both groups, not significant (p > 0.30).
# Same direction as earlier findings, but with high imprecision.
# Children: Small coefficients, no statistical significance.

# CONCLUSION: No significant MTO effect on ever voting in Chicago.
#Teens have a consistent but imprecise negative trend.


dat <- la

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# Estimate MTO's impact on matching in Los Angeles (ra_site == 4)

# Adults:experimental weak positive trend (+0.043, p = 0.078), not significant
# Section 8 null (-0.004, p = 0.87). Faint, inconclusive signal
# Adolescents:experimental negative (-0.038, p = 0.31), Section 8 null (-0.008, p = 0.84)
# Faint, non-significant negative trend
# Children:No effects (Exp: +0.006, p = 0.84; S8: -0.010, p = 0.71)
# Definite null

## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# Estimate MTO's effect on turnout (r_postturnout) in Los Angeles (ra_site == 4)

# Adults: No significant effects. Experimental: +0.009 (p = 0.24), Section 8: -0.005 (p = 0.44)
# Adolescents: Experimental shows a negative trend (-0.019, p = 0.059), borderline significant
# Section 8: no effect
# Children: Section 8 group shows a weak negative trend (-0.012, p = 0.08)
# Experimental group: null effect

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# MTO effect on ever voted (evervoted_post), Los Angeles only (ra_site == 4)

# Adults: No significant effect. Experimental: +0.029 (p = 0.16), Section 8: -0.010 (p = 0.60)
# Adolescents: Significant negative trend for Experimental (-0.060, p = 0.053), on the boundary of significance
# Section 8: no effect
# Children: No effect. Experimental: +0.004 (p = 0.86), Section 8: -0.010 (p = 0.63)

dat <- nyc

#table 41/45/49/53/57: matching to voter file/registration
mod1 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(matched~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# MTO effect on match (matched) in New York (ra_site == 5)

# Adults: No effect. Experimental: -0.015 (p = 0.49), Section 8: -0.008 (p = 0.72)
# Adolescents:Null effects. Experimental: -0.003 (p = 0.95), Section 8: -0.009 (p = 0.85)
# Children: Weak positive trend for Experimental (+0.030, p = 0.26), Section 8: null (+0.007, p = 0.81)
# No significant effects


## table 42/46/50/54/58: voting rate posttreatment
mod1 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(r_postturnout~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# MTO effect on turnout (r_postturnout) in New York (ra_site == 5)

# Adults: No effect. Experimental: -0.006 (p = 0.45), Section 8: +0.005 (p = 0.58)
# Adolescents: Null effects. Experimental: -0.0005 (p = 0.96), Section 8: -0.0013 (p = 0.91)
# Children:No effect. Experimental: +0.004 (p = 0.62), Section 8: -0.005 (p = 0.52)

## table 43/47/51/55/59: predict ever voted posttreatment
mod1 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- lm_robust(evervoted_post~ra_group_factor, weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# OBJECTIVE:
# MTO effect on ever voted (evervoted_post) in New York (ra_site == 5)

# Adults: No significant effect. Experimental: -0.014 (p = 0.46), Section 8: -0.006 (p = 0.77)
# Adolescents: Null effect. Experimental: -0.004 (p = 0.91), Section 8: -0.006 (p = 0.85)
# Children: No effect. Experimental: +0.019 (p = 0.36), Section 8: ~0 (p = 0.99)


# List of dataset for the cities

list_of_datasets <- list(
  Baltimore = baltimore,
  Boston = boston,
  Chicago = chicago,
  "Los Angeles" = la,
  "New York" = nyc
)

# Execute the model on each city and collate the results into a single table
# This is where the code gets meaty: it accomplishes most of its work in just a few lines.
plot_data <- map_dfr(list_of_datasets, ~ {
  lm_robust(evervoted_post ~ ra_group_factor, data = .x %>% filter(age_group == "old_kid")) %>%
    tidy()
}, .id = "city") %>%  # .id = "city" aggiunge una colonna con il nome della città
  filter(term != "(Intercept)") %>% # Rimuovi le intercette
  mutate(
    treatment_group = if_else(
      term == "ra_group_factorexperimental", 
      "Experimental Voucher", 
      "Section 8 Voucher"
    )
  )

#my chart

ggplot(plot_data, aes(x = estimate, y = fct_rev(city))) + # fct_rev() inverte l'ordine delle città
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ treatment_group) +
  labs(
    title = "MTO Impact on Adolescent Voting Across Cities",
    subtitle = "Outcome: 'Ever Voted Post-Treatment'",
    x = "Estimated Effect on Probability of Voting",
    y = NULL
  ) +
  theme_bw()
# GOAL:
# Compare the impact of MTO on political engagement in five cities.

# Teenagers have the most variability, with children and adults basically unaffected.

# There are effects varying by city:
# - Chicago and Baltimore: small, non-statistically significant adverse trends among teenagers.
# - Boston and Los Angeles: clear negative impacts in some groups, with steep drops in voting among teens.
# - New York: no impact on teens.

# National aggregates conceal large local differences
# especially negative impacts on young people in some cities, with a focus on context.

# recover full data
dat <- dat_all_groups

rm(dat_all_groups, baltimore, boston, boys, chicago, girls, la, nyc)
# Appendix A6 ####

# see previous section for section 8 results by site


# Appendix A3 ####

# note: because the compliance measure is averaged within clusters, divergence may be greater from full-data models
# recall that signs are flipped

mod1 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(matched~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

mod1 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(r_postturnout~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)

mod1 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="adult",])
mod2 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="old_kid",])
mod3 <- iv_robust(evervoted_post~local_poverty_posttreat+factor(ra_site) | ra_group_factor+factor(ra_site), weights=f_wt_totcore98, data=dat[dat$age_group=="young_kid",])
summary(mod1)
summary(mod2)
summary(mod3)
# GOAL:
# Providing a causal effect estimate of reducing poverty in neighborhoods on political participation, with assignment as an instrumental variable.

# NOTE:
# A positive coefficient means that decrease in poverty led to lowered political participation.

# RESULTS:

# Adults: No effects were observed.
# Teenagers: The results are clear and uniform:
# Decreasing neighborhood poverty reduced significantly their likelihood
# of matching to voter records, turnout at the polls, and ever having voted.

# No substantial impacts were observed in young children.
# MAIN CONCLUSION:
# The IV analysis provides strong evidence that reducing neighborhood poverty
# unexpectedly reduced political participation among teens,
# but had no impact on adults or younger kids.


# post-registration turnout models cannot be estimated
# Appendix A2 ####

# results cannot be replicated due to grouping by variables averaged within clusters 

# Appendix A7 ####

# results cannot be replicated: all results presented concern only t-statistics/p-values, which are not maintained in clustered data








