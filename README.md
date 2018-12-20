---
---
title: "ITS"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
library(caret)
library(ggplot2)
library(pracma)
library(AER)
library(pscl)
library(TSA)
library(TTR)
library(smooth)
library(descr)
```
Load FISCH data
```{r}
dat_FISCH = data_FISCH
```
Just explore the data set
Need to exclude people that are ineligible

In theory there anyone who says a 1 and 0 on the two inclusion criteria questions should be eligible

But there is some missing data for these questions 
```{r}
head(dat_FISCH)
dat_FISCH$ineligible
dim(dat_FISCH)
dat_FISCH =subset(dat_FISCH, is.na(ineligible))
dim(dat_FISCH)
```
If you get rid of the ineligible people, then there should be no cases where there is missing data for the repeat session indicator.  Then you can assume the blank one is the first data session I think (double check)

If you can change NA to 1 for repeat instance variable

Need a time variable

So zeros only include 

This solves the problem.  Figure out which variables are NA and make those true or false and put them into a new variable.  Then into another new variable, state if check variable is true, then 0 for first data collection session, and go back to whatever the original variable had for repeat instance to get the correct time point 

So now in theory, any zeros are meaningful in other data sets and NAs are actual NAs.
```{r}
dat_FISCH_agg = data.frame(record_id = dat_FISCH$record_id, redcap_repeat_instance = dat_FISCH$redcap_repeat_instance, colevel_1 = dat_FISCH$colevel_1, colevel_2 = dat_FISCH$colevel_2, dat_FISCH$gender, dat_FISCH$race___1, dat_FISCH$race___2, dat_FISCH$race___3, dat_FISCH$race___4, dat_FISCH$race___5, dat_FISCH$race___6, dat_FISCH$otherrace, dat_FISCH$income, dat_FISCH$education, dat_FISCH$ethnicity, dat_FISCH$relationshipstatus)

head(dat_FISCH_agg)


dat_FISCH_agg$redcap_repeat_instance_check = is.na(dat_FISCH_agg[,2])
dat_FISCH_agg$redcap_repeat_instance_true = ifelse(dat_FISCH_agg$redcap_repeat_instance_check == TRUE , 0, dat_FISCH_agg$redcap_repeat_instance)

head(dat_FISCH_agg)

## Get rid of extra time variables so there is no confusion
dat_FISCH_agg$redcap_repeat_instance = NULL
dat_FISCH_agg$redcap_repeat_instance_check = NULL
dat_FISCH_agg$redcap_repeat_instance_true
```
Ok next problem is the data is in long form, but data is across different variables
So for session one there is a data point, but for the second line for that person it is blank and then there is a variable number two
Just grab recordID, colevel_1, colevel_2

Make wide, and then grab just the ones you want and subset the data, then make long again

```{r}
dat_FISCH_agg = reshape(dat_FISCH_agg, v.names = c("colevel_1", "colevel_2"), direction = "wide", timevar = "redcap_repeat_instance_true", idvar = "record_id")
head(dat_FISCH_agg)
```
Now we need to drop the extra rows and only keep those with data and go long
```{r}
dat_FISCH_agg_long  = data.frame(record_id = dat_FISCH_agg$record_id,colevel_1.0 = dat_FISCH_agg$colevel_1.0, colevel_2.1 = dat_FISCH_agg$colevel_2.1, gender = dat_FISCH_agg$dat_FISCH.gender, race___1 = dat_FISCH_agg$dat_FISCH.race___1, race___2 = dat_FISCH_agg$dat_FISCH.race___2, race___3 = dat_FISCH_agg$dat_FISCH.race___3, race___4 = dat_FISCH_agg$dat_FISCH.race___4, race___5 = dat_FISCH_agg$dat_FISCH.race___5, race___6 = dat_FISCH_agg$dat_FISCH.race___6, otherrace = dat_FISCH_agg$dat_FISCH.otherrace, income = dat_FISCH_agg$dat_FISCH.income, education = dat_FISCH_agg$dat_FISCH.education, ethnicity = dat_FISCH_agg$dat_FISCH.ethnicity, relationshipstatus = dat_FISCH_agg$dat_FISCH.relationshipstatus) 



dat_FISCH_agg_long_test = reshape(dat_FISCH_agg_long, varying = list(c("colevel_1.0", "colevel_2.1")), direction = "long", times = c(0,1))
dat_FISCH_agg_long_test

dat_FISCH_agg_long = dat_FISCH_agg_long_test[order(dat_FISCH_agg_long_test$record_id),]
head(dat_FISCH_agg_long)

### Need to make other race either ones or zeros if true or false so it doesn't get deleted
dat_FISCH_agg_long$otherrace_check = is.na(dat_FISCH_agg_long[,9])
head(dat_FISCH_agg_long)
dat_FISCH_agg_long$otherrace_true = ifelse(dat_FISCH_agg_long$otherrace_check == TRUE , 0, dat_FISCH_agg_long$otherrace_check)

## Get rid of original other race so you can delete data 
dat_FISCH_agg_long$otherrace = NULL
dat_FISCH_agg_long$otherrace_check = NULL
```
Now bring it back to the original data set

If seems like if colevel is NA then there was no data for that data point ever, so you can delete it.

To find the amount of missing data for the main analytical model we evaluate the number of people in the dat_FISCH_agg_long data set without excluding missing data and then with excluding missing values.

This makes sense, because once we have all the data points if they are missing a time point then, that means we missed that data point.

We should have all the demographics, so when we aggregate the demographics back, we should not lose any data points
```{r}
head(dat_FISCH_agg_long)
dim(dat_FISCH)
dim(dat_FISCH_agg_long)
dat_FISCH_agg_long_complete = na.omit(dat_FISCH_agg_long)
dim(dat_FISCH_agg_long_complete)[1] / dim(dat_FISCH_agg_long)[1]
```

```{r}
head(dat_FISCH_agg_long)
describe(dat_FISCH_agg_long)
```






