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
#install.packages("lmerTest")
library(lmerTest)
#library(brms)
library(MASS)
library(semTools)
library(descr)
library(MuMIn)
library(HLMdiag)
library(psych)
library(sjstats)
```
Loading data
```{r}
#setwd("~/Desktop")
#setwd("S:/Indiana Research & Evaluation/FISCH (Jon's Study)/Data")
#data_FISCH = read.csv("FISCH_DATA_2019-03-15_0801.csv", header = TRUE)
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
Get descriptives
```{r}
describe(dat_FISCH_long_complete)
describe.factor(dat_FISCH_long_complete$time)
describe.factor(dat_FISCH_long_complete$intervention)

### As we get to session six they are going back up!
compmeans(dat_FISCH_long_complete$colevel_1, dat_FISCH_long_complete$time)
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
co_reduction_percent
```
Let's do a power analysis for this results
```{r}
dat_power = data.frame(base = dat_FISCH_t_test_complete_base, follow = dat_FISCH_t_test_complete_follow)

mean_fisch = apply(dat_power, 2, mean)
mean_fisch[1]
sd_fisch = apply(dat_power, 2, sd)

dim(dat_power)[1]

FISCH_power = function(){
  base = rnorm(n = dim(dat_power)[1], mean = mean_fisch[1], sd = sd_fisch[1])
  follow = rnorm(n = dim(dat_power)[1], mean = mean_fisch[2], sd = sd_fisch[2])
  t_results = t.test(follow, base, alternative = "less", paired = TRUE)
  t_results= ifelse(t_results$p.value < .05,1,0)
  t_results
}
```
Now run the results like 500 times (should do like 10,000)
```{r}
reps = 500
power = replicate(reps, FISCH_power())
power = sum(power)/reps
power
```
Now let us do it with differing numbers of n's
```{r}
FISCH_power_n = function(){
  n = c(11,20,30,40)
  base = list()
  follow = list()
  t_results = list()
  for(i in 1:length(n)){
  base[[i]] = rnorm(n[[i]], mean = mean_fisch[1], sd = sd_fisch[1])
  follow[[i]] = rnorm(n[[i]], mean = mean_fisch[2], sd = sd_fisch[2])
  t_results[[i]] = t.test(follow[[i]], base[[i]], alternative = "less", paired = TRUE)
  t_results[[i]]= ifelse(t_results[[i]]$p.value< .05,1,0)
  }
  return(t_results)
}

```
Now run the results like 500 times (should do like 10,000)
Get the data into a fomrat that has 500 columns and rows represent the results for the different n's
```{r}
reps = 500
power_rep = replicate(reps, FISCH_power_n())
power_unlist= unlist(power_rep)
power_matrix = matrix(power_unlist, ncol = reps, nrow = length(n), byrow = FALSE)
power = apply(power_matrix, 1, sum)/reps
power
```
Ok let's set up a multilevel model so we only delete data
```{r}
head(dat_FISCH)

dat_FISCH_long = reshape(dat_FISCH, varying = list(c("colevel_1", "colevel_2", "colevel_3", "colevel_4", "colevel_5", "colevel_6")), direction = "long", times = c(0:5))

head(dat_FISCH_long)
dim(dat_FISCH_long)

dat_FISCH_long_complete = na.omit(dat_FISCH_long)


1-(dim(dat_FISCH_long_complete)[1]/dim(dat_FISCH_long)[1])

#### How many people have certain numbers of time points
describe.factor(dat_FISCH_long_complete$time)


### Need to add an intervention variable after time point three
### Change this later when some people only have two sessions
dat_FISCH_long_complete$intervention = ifelse(dat_FISCH_long_complete$time > 3, 1, 0)
dat_FISCH_long_complete = dat_FISCH_long_complete[order(dat_FISCH_long_complete$id),]
head(dat_FISCH_long_complete)
```
Just try a multilevle model
So for the interaction term, it is showing the average change for one time unit increase within the time points.  So it is comparing the average change for the baseline points relative to the average change for the intervention points.  

So it is positive.  Which does not mean that people are not lower in the pay phase it is showing that there is a higher rate of change for those in the intervention phase (CO levels are going up a faster rate than baseline).

So basically the effect wheres off after one attempt

### Try model with poly term
### Create centered data point
### Positive sign means convex, which indicates a rainbow shape and it is on its way up to the convex point, negative sign indicates a u shape and it is on its way to the point of switching 
## Beta 1 for time is the average change at time zero, which is baseline, this is all relative to the baseline groups
#https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-the-sign-of-the-quadratic-term-in-a-polynomial-regression/
```{r}
model_null = lmer(colevel_1 ~ +(1 | id), data = dat_FISCH_long_complete)

model_long_mean = lmer(colevel_1 ~ intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_mean)


model_long_inter = lmer(colevel_1 ~ time*intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_inter)


model_long_inter_poly = lmer(colevel_1 ~ poly(time, 2)*intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_inter_poly)

model_long_time_poly = lmer(colevel_1 ~ poly(time, 2) + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_time_poly)

### Model comparison
anova(model_null,model_long_mean, model_long_inter, model_long_inter_poly)

```
Get graph of random effects with mean over time and response
```{r}
library(sjPlot)
plot_model(model_long_inter, type = "re")

test = summary(model_long_inter)

ranef(model_long_inter)
ranef(model_long_mean)

```


Get r^2 
check residuals 
ICC to justify the inclusion of random intercepts
```{r}
#R2m is the amount of variation explained by the fixed effects and R2c is the fixed plus random effects
r.squaredGLMM(model_long_inter)
icc(model_long_inter)
summary(model_long_inter)
109.50 / (109.50+79.51)

res_model = summary(model_long_inter)$residuals
res_model_stand = scale(res_model)
hist(res_model_stand)
range(res_model_stand)
qqnorm(res_model_stand)

(184.080+5.067)/(184.080+5.067 +57.799)
```
Ok now look at sensitivty analyses
Need to unintall lme4 first
Thresholds are set where the exact parameter estimate is for signficance at the .05 alpha level.  Then how much data would have to be due to bias to move to not significant based on that threshold
```{r}
library(konfound)
remove.packages("lmerTest")
library(lme4)
model_long_mean = lmer(colevel_1 ~ intervention + (1| id),  data = dat_FISCH_long_complete)
summary(model_long_mean)
konfound(model_long_mean, intervention)
summary(model_long_mean)
```
Ok now need contrasts for more than intervention
just get some simluated data
We don't have 1 versus 2 and 3 and we don't have 2 versus 3

4 total parameter estimates
```{r}
d = data.frame(outcome = rnorm(100), intervention = c(rep(0,25), rep(1,25), rep(2,25), rep(3,25)))
d$intervention = factor(d$intervention)
model_d = lm(outcome~intervention, data = d)
summary(model_d)
K = matrix(c(0, 1,-1,0, 0,1,0,-1,0,0,1,-1), nrow = 3, ncol = 4, byrow = TRUE)
library(multcomp)
t = glht(model_d, linfct = K)
summary(t)
library(descr)
## Check logic
## -0.2067
compmeans(d$outcome, d$intervention)
0.04612403 - 0.25287273
0.25287273 --0.05464364
```


Ok try Bayesian version
```{r}
dat_FISCH_long_complete$colevel_1_stand = scale(dat_FISCH_long_complete$colevel_1)
summary(dat_FISCH_long_complete)

### Just try standard
brms_model = brm(colevel_1_stand ~ time*intervention + (1|id), data = dat_FISCH_long_complete)
summary(brms_model)


brms_model_weak_prior = brm(colevel_1_stand ~ time*intervention + (1|id), data = dat_FISCH_long_complete, prior = set_prior("student_t(1,-.25,.05)", class = "b", coef = "intervention"))


summary(brms_model_weak_prior)

brms_model_nointer = brm(colevel_1_stand ~ time+intervention + (1|id), data = dat_FISCH_long_complete, prior = set_prior("student_t(1,-.25,.05)", class = "b", coef = "intervention"))
summary(brms_model_nointer)


brms_model_no_prior_ranSlopes = brm(colevel_1_stand ~ time*intervention + (time|id), data = dat_FISCH_long_complete)
summary(brms_model_no_prior_ranSlopes)
```
Model diagnostics
```{r}

weak_WAIC = WAIC(brms_model_weak_prior)
reg_WAIC = WAIC(brms_model)
compare_ic(weak_WAIC, reg_WAIC)

### Here are the chains
post = as.mcmc(brms_model)
head(post)


### 
tests= fitted(brms_model)
tests

test_post = posterior_predict(brms_model)
test_post
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
Ok I figured it out.  The code I used "ranef" gives the difference between the average, in FISCH terms, CO_level for the baseline, and average CO_level for each person.  So if you add the overall effect with the unique error part then you get the actual intercept for each person.
```{r}
d_test = d[[4]]
d_test$y_large = d_test$y +100


model_d = lmer(y_large ~ time*intervention + (1 | subject), data = d_test)
summary(model_d)

ran_eff = ranef(model_d)
library(lattice)
head(ran_eff$subject)
dotplot(ran_eff)
coef_ind = coef(model_d)
head(coef_ind$subject)
```
With positive intercept this means on average there is an increase in 

