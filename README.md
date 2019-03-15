---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(lme4)
library(prettyR)
library(reshape2)
library(lmerTest)
library(brms)
library(MASS)
library(semTools)
```
Loading data
```{r}
setwd("S:/Indiana Research & Evaluation/FISCH (Jon's Study)/Data")
data_FISCH = read.csv("FISCH_DATA_2019-03-15_0801.csv", header = TRUE)
dat_FISCH = data_FISCH

#### Get rid of anyone who is not eligible
dat_FISCH = subset(dat_FISCH, eligibledebrief == 1)
sum(is.na(dat_FISCH))
describe.factor(dat_FISCH$eligibledebrief)
```
Take a look at the data
Grab all CO_levels and record ID to make long version
```{r}
head(dat_FISCH)
dat_FISCH = dat_FISCH[c("record_id","colevel_1", "colevel_2", "colevel_3", "colevel_4", "colevel_5", "colevel_6")]

head(dat_FISCH)
```
Ok let us try taking the first four sessions for those with full data from that and then getting a t-test
```{r}

dat_FISCH_t_test = dat_FISCH[c("record_id","colevel_1", "colevel_2", "colevel_3", "colevel_4")]
dim(dat_FISCH_t_test)

dat_FISCH_t_test_complete = na.omit(dat_FISCH_t_test)
dim(dat_FISCH_t_test_complete)  

### Get the average for the first three rows
dat_FISCH_t_test_complete_base = dat_FISCH_t_test_complete[c("colevel_1", "colevel_2", "colevel_3")]

dat_FISCH_t_test_complete_base = apply(dat_FISCH_t_test_complete_base, 1, mean)

dat_FISCH_t_test_complete_follow = dat_FISCH_t_test_complete[c("colevel_4")]

dat_FISCH_t_test_complete_follow = data.frame(follow = dat_FISCH_t_test_complete_follow)
names(dat_FISCH_t_test_complete_follow) = "follow"
dat_FISCH_t_test_complete_base = data.frame(base = dat_FISCH_t_test_complete_base)
names(dat_FISCH_t_test_complete_base) = "base"


t.test(dat_FISCH_t_test_complete_follow$follow, dat_FISCH_t_test_complete_base$base, alternative = "less", paired = TRUE)


co_reduction = 15.72727 - 24.18182
co_reduction_percent = (15.72727 - 24.18182)/24.18182

```
Ok let's set up a multilevel model so we only delete data
```{r}
head(dat_FISCH)

dat_FISCH_long = reshape(dat_FISCH, varying = list(c("colevel_1", "colevel_2", "colevel_3", "colevel_4", "colevel_5", "colevel_6")), direction = "long", times = c(1:6))

head(dat_FISCH_long)
dim(dat_FISCH_long)

dat_FISCH_long_complete = na.omit(dat_FISCH_long)


1-(dim(dat_FISCH_long_complete)[1]/dim(dat_FISCH_long)[1])

### Need to add an intervention variable after time point three
### Change this later when some people only have two sessions
dat_FISCH_long_complete$intervention = ifelse(dat_FISCH_long_complete$time > 3, 1, 0)
dat_FISCH_long_complete = dat_FISCH_long_complete[order(dat_FISCH_long_complete$id),]


```
Just try a multilevle model
So for the interaction term, it is showing the average change for one time unit increase within the time points.  So it is comparing the average change for the baseline points relative to the average change for the intervention points.  

So it is positive.  Which does not mean that people are not lower in the pay phase it is showing that there is a higher rate of change for those in the intervention phase (CO levels are going up a faster rate than baseline).

So basically the effect wheres off after one attempt

```{r}
dat_FISCH_long_complete
model_null = lmer(colevel_1 ~ +(1 | id), data = dat_FISCH_long_complete)

model_long_mean = lmer(colevel_1 ~ intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_mean)


model_long_inter = lmer(colevel_1 ~ time*intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_inter)

anova(model_null,model_long_mean,model_long_time_inter, model_long_inter)

```
Ok try Bayesian version
```{r}
dat_FISCH_long_complete$colevel_1_stand = scale(dat_FISCH_long_complete$colevel_1)
summary(dat_FISCH_long_complete)

brms_model = brm(colevel_1_stand ~ time*intervention + (1|id), data = dat_FISCH_long_complete, prior = set_prior("student_t(1,-.25,.05)", class = "b", coef = "intervention"))

summary(brms_model)

brms_model_nointer = brm(colevel_1_stand ~ time+intervention + (1|id), data = dat_FISCH_long_complete, prior = set_prior("student_t(1,-.25,.05)", class = "b", coef = "intervention"))
summary(brms_model_nointer)

brms_model_no_prior = brm(colevel_1_stand ~ time+intervention + (1|id), data = dat_FISCH_long_complete)
summary(brms_model_no_prior)

brms_model_no_prior_ranSlopes = brm(colevel_1_stand ~ time*intervention + (time|id), data = dat_FISCH_long_complete)
summary(brms_model_no_prior_ranSlopes)

brms_model_no_prior_ranInt = brm(colevel_1_stand ~ time*intervention + (1|id), data = dat_FISCH_long_complete)
summary(brms_model_no_prior_ranInt)

```
Try running a simulation with the interaction effect 
Try with both random slopes and intercepts
```{r}
power_bayes_multi_n = function(){
# Generate the blank data sets where we will put the data
n = list(20,30,40,50)
intervention_out = list()
y = list()
time_out = list()
randomEffects_out = list()
subject_out = list()
d = list()

### Effects
intercept = .57
time_effect = -.31
intervention_effect = -1.55
time_intervention_effect = .41

#Set time points
timepoints = 6
time = timepoints-1

for(i in 1:length(n)){
  time_out[[i]] = rep(x = 0:time, times=n[[i]])
  intervention_out[[i]]= c(rep(1,round(n[[i]]*.5,1)), rep(0,round(n[[i]]*.5,0)))
  randomEffectsCorr = matrix(c(1.16,-.85,-.85, .8), ncol = 2)
  randomEffects_out[[i]] = mvrnonnorm(n[[i]], mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
  randomEffects_out[[i]] = data.frame(randomEffects_out[[i]])
  colnames(randomEffects_out[[i]]) = c("Int", "SlopeT")
  subject_out[[i]] = rep(1:n[[i]], each=timepoints)
  ## We are assuming a random intercepts model only
  y[[i]] = (intercept + randomEffects_out[[i]]$Int[subject_out[[i]]]) + (time_effect + randomEffects_out[[i]]$SlopeT[subject_out[[i]]])*time_out[[i]] + intervention_effect*intervention_out[[i]] + time_intervention_effect*time_out[[i]]*intervention_out[[i]] + rnorm(n[[i]]*timepoints, mean = .59, sd = .07)
  d[[i]] = data.frame(y = y[[i]], intervention = intervention_out[[i]], time = time_out[[i]], subject = subject_out[[i]])
}

#Generate empty data sets to fill
brms_model_prior_out = list()
brms_model_prior_summary_out = list()
lower_out = list()
upper_out = list()
power_out = list()

for(i in 1:length(d[[i]])){
brms_model_prior_out[[i]] = brm(y ~ time*intervention + (time|subject), data = d[[i]], prior = set_prior("student_t(10,-1.55,1.05)", class = "b", coef = "intervention"))
brms_model_prior_summary_out[[i]] = summary(brms_model_prior_out[[i]])
lower[[i]] = brms_model_prior_summary_out[[i]]$fixed[3,3]
upper[[i]] = brms_model_prior_summary_out[[i]]$fixed[3,4]
lower[[i]] = ifelse(lower[[i]] < 0,1,0)
upper[[i]] = ifelse(upper[[i]] > 0,1,0)
power[[i]] = sum(lower[[i]], upper[[i]])
### If a value has a lower than zero and an upper greater than zero than not sig everything else is sig
power[[i]] = ifelse(power[[i]] == 2, 0,1)
power[[i]]
}
}
```
Now run this model several times
```{r}
reps = 50
power_rep = replicate(reps, power_bayes_multi_n())
power_rep_df = data.frame(power_rep)
power_rep_df_mean = rowMeans(power_rep_df)
power_rep_df_mean
```
Need to fix lower 
```{r}

```


