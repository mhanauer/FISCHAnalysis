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
We want to load the data with na.strings = "", because we want R to treat blanks as NAs.
```{r}
setwd("S:/Indiana Research & Evaluation/FISCH (Jon's Study)/Data")
data_FISCH = read.csv("FISCH_DATA_2019-01-17_1012.csv", header = TRUE, na.strings = "")
dat_FISCH = data_FISCH
```
First thing we want to get is get rid of people who are not eligible for the program.
We can do this by keeping everyone who is NA on the ineligible criteria, because everyone who is eligible has an NA and those who are not ineligible have a 1.  This can be confirmed by the overunder 10 variable.
```{r}
head(dat_FISCH)
dat_FISCH$ineligible
dim(dat_FISCH)
dat_FISCH =subset(dat_FISCH, is.na(ineligible))
dim(dat_FISCH)
```
First I am grabbing just a few variables to make the data set more manageable.  When using a data frame, you need to rename the variable (i.e. name =  variable) otherwise you will get a really long name.   
```{r}
dat_FISCH_agg = data.frame(record_id = dat_FISCH$record_id,session1date= dat_FISCH$session1date, session2date = dat_FISCH$session2date, session3date = dat_FISCH$session3date, session4date = dat_FISCH$session4date, session5date = dat_FISCH$session5date, session6date = dat_FISCH$session6date, colevel_1 = dat_FISCH$colevel_1, colevel_2 = dat_FISCH$colevel_2, colevel_3 = dat_FISCH$colevel_3, colevel_4 = dat_FISCH$colevel_4, colevel_5 = dat_FISCH$colevel_5, colevel_6 = dat_FISCH$colevel_6,  gender = dat_FISCH$gender, race___1 = dat_FISCH$race___1, race___2 = dat_FISCH$race___2, race___3 = dat_FISCH$race___3, race___4 = dat_FISCH$race___4, race___5 = dat_FISCH$race___5, race___6 = dat_FISCH$race___6, otherrace = dat_FISCH$otherrace, income = dat_FISCH$income, education = dat_FISCH$education, ethnicity = dat_FISCH$ethnicity, relationshipstatus = dat_FISCH$relationshipstatus)
```
Although, this step is not necessary at this point, we want R to treat the dates as dates, so I use the formula below to translate the dates into dates that R can recongize.  We are using lowercase y, because the dates are two digits not four.
```{r}
head(dat_FISCH_agg)
dat_FISCH_agg$session1date  = as.Date(dat_FISCH_agg$session1date, format = "%m/%d/%y")
dat_FISCH_agg$session2date  = as.Date(dat_FISCH_agg$session2date, format = "%m/%d/%y")
dat_FISCH_agg$session3date  = as.Date(dat_FISCH_agg$session3date, format = "%m/%d/%y")
dat_FISCH_agg$session4date  = as.Date(dat_FISCH_agg$session4date, format = "%m/%d/%y")
dat_FISCH_agg$session5date  = as.Date(dat_FISCH_agg$session5date, format = "%m/%d/%y")
dat_FISCH_agg$session6date  = as.Date(dat_FISCH_agg$session6date, format = "%m/%d/%y")
```
We want the data in long form, so therefore we need to use the reshape function.  There are four arguments.  First is the data set that we want to reshape.  Second, we need to reshape, which variables are going to be in long form (so which variables to stack).  We created a list and put c in front of the list, so if we wanted to add more vairables (hint) we can do that by adding c("variable1", "variable2"..., "variableX").  Next we tell reshape the direction, which is long.  Finally, we put the number of time points, so it can create a time variable for us. 
```{r}
dat_FISCH_agg_long = reshape(dat_FISCH_agg, varying = list(c("colevel_1", "colevel_2", "colevel_3", "colevel_4", "colevel_5", "colevel_6")), direction = "long", times = c(1,2,3,4,5,6))
```

Get descriptives by time point
Need sd and means for continious variables
Need counts and percentages for categorical and nominal variables 
```{r}
describe(dat_FISCH_agg)
dim(dat_FISCH_agg)
dat_FISCH_agg_cat = dat_FISCH_agg[,14:25]
dat_FISCH_agg_cat = apply(dat_FISCH_agg_cat, 2, function(x){describe.factor(x)})
dat_FISCH_agg_cat
```
Next steps:

Add the other covarying questions (I think there are two more)

Figure out where the missing data is by variable

Starting thinking about multilevel models for longitudnal analysis

Matt generate fake data for analysis as example 

Matt demonstrate equations





