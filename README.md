---
---
title: "FISCH Analysis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(reshape2)
library(psych)
library(descr)
library(prettyR)
```
Load FISCH data
Save the loaded data as something else so we don't have to reload the data everytime
```{r}
setwd("~/Desktop")
data_FISCH = read.csv("FISCH_DATA_2018-12-30_0750.csv", header = TRUE)
dat_FISCH = data_FISCH
```
Just explore the data set
Need to exclude people that are ineligible

In theory there anyone who says a 1 and 0 on the two inclusion criteria questions should be eligible

But there is some missing data for these questions 


First thing we want to get is get rid of people who are not eligible for the program.
We can do this by keeping everyone who is NA on the ineligible criteria, because everyone who is eligible has an NA and those who are not ineligible have a 1.  This can be confirmed by the overunder 10 variable.
```{r}
head(dat_FISCH)
dat_FISCH$ineligible
dim(dat_FISCH)
dat_FISCH =subset(dat_FISCH, is.na(ineligible))
dim(dat_FISCH)
```
The next problem we need to solve is that we need a time variable in order to use the reshape function to change the data into long form.

If you include all sessions dates and then create a variable that says whether the variable is for 1,2,3,4,5,6 and NA if NA, then maybe you can transform into wide format.
```{r}
dat_FISCH_agg = data.frame(record_id = dat_FISCH$record_id,session1date= dat_FISCH$session1date, session2date = dat_FISCH$session2date, session3date = dat_FISCH$session3date, session4date = dat_FISCH$session4date, session5date = dat_FISCH$session5date, session6date = dat_FISCH$session6date, colevel_1 = dat_FISCH$colevel_1, colevel_2 = dat_FISCH$colevel_2, colevel_3 = dat_FISCH$colevel_3, colevel_4 = dat_FISCH$colevel_4, colevel_5 = dat_FISCH$colevel_5, colevel_6 = dat_FISCH$colevel_6,  gender = dat_FISCH$gender, race___1 = dat_FISCH$race___1, race___2 = dat_FISCH$race___2, race___3 = dat_FISCH$race___3, race___4 = dat_FISCH$race___4, race___5 = dat_FISCH$race___5, race___6 = dat_FISCH$race___6, otherrace = dat_FISCH$otherrace, income = dat_FISCH$income, education = dat_FISCH$education, ethnicity = dat_FISCH$ethnicity, relationshipstatus = dat_FISCH$relationshipstatus)

write.csv(dat_FISCH_agg, "dat_FISCH_agg.csv", row.names = FALSE)
dat_FISCH_agg = read.csv("dat_FISCH_agg.csv", header = TRUE, na.strings = "")
head(dat_FISCH_agg)

dat_FISCH_agg$session1date  = as.Date(dat_FISCH_agg$session1date, format = "%m/%d/%y")
dat_FISCH_agg$session2date  = as.Date(dat_FISCH_agg$session2date, format = "%m/%d/%y")
dat_FISCH_agg$session3date  = as.Date(dat_FISCH_agg$session3date, format = "%m/%d/%y")
dat_FISCH_agg$session4date  = as.Date(dat_FISCH_agg$session4date, format = "%m/%d/%y")
dat_FISCH_agg$session5date  = as.Date(dat_FISCH_agg$session5date, format = "%m/%d/%y")
dat_FISCH_agg$session6date  = as.Date(dat_FISCH_agg$session6date, format = "%m/%d/%y")

dat_FISCH_agg_long = reshape(dat_FISCH_agg, varying = list(c("colevel_1", "colevel_2", "colevel_3", "colevel_4", "colevel_5", "colevel_6")), direction = "long", times = c(1,2,3,4,5,6))
```

To find the amount of missing data for the main analytical model we evaluate the number of people in the dat_FISCH_agg_long data set without excluding missing data and then with excluding missing values.

This makes sense, because once we have all the data points if they are missing a time point then, that means we missed that data point.

We should have all the demographics, so when we aggregate the demographics back, we should not lose any data points
```{r}
head(dat_FISCH_agg_long)
dim(dat_FISCH)
dim(dat_FISCH_agg_long)
library(psych)
describe(dat_FISCH_agg_long, na.rm = TRUE)
apply(dat_FISCH_agg_long, 2, mean, na.rm = TRUE)

describe.factor(dat_FISCH_agg_long$dat_FISCH.education)
```

```{r}

```






