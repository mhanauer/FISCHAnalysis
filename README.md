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
```{r}
dat_FISCH_agg = data.frame(record_id = dat_FISCH$record_id, redcap_repeat_instance = dat_FISCH$redcap_repeat_instance, colevel_1 = dat_FISCH$colevel_1, colevel_2 = dat_FISCH$colevel_2)

head(dat_FISCH_agg)

### Makes every variable have NA's
dat_FISCH_agg[is.na(dat_FISCH_agg)]= 0

head(dat_FISCH_agg)
```



Ok next problem is the data is in long form, but data is across different variables
So for session one there is a data point, but for the second line for that person it is blank and then there is a variable number two
Just grab recordID, colevel_1, colevel_2

Make wide, and then grab just the ones you want and subset the data, then make long again

```{r}
dat_FISCH_agg = reshape(dat_FISCH_agg, v.names = c("colevel_1", "colevel_2"), direction = "wide", timevar = "redcap_repeat_instance", idvar = "record_id")
head(dat_FISCH_agg)
```
Now we need to drop the extra rows and only keep those with data and go long
```{r}
dat_FISCH_agg_long  = data.frame(record_id = dat_FISCH_agg$record_id,colevel_1.0 = dat_FISCH_agg$colevel_1.0, colevel_2.1 = dat_FISCH_agg$colevel_2.1) 
dat_FISCH_agg_long


dat_FISCH_agg_long_test = reshape(dat_FISCH_agg_long, varying = list(c("colevel_1.0", "colevel_2.1")), direction = "long", times = c(0,1))
dat_FISCH_agg_long_test

dat_FISCH_agg_long = dat_FISCH_agg_long_test[order(dat_FISCH_agg_long_test$record_id),]
head(dat_FISCH_agg_long)
```
Now bring it back to the original data set

If seems like if colevel is NA then there was no data for that data point ever, so you can delete it.

Then you can aggregate back the data.  
```{r}
dat_FISCH_agg_long
dim(dat_FISCH)
dim(dat_FISCH_agg_long)
dat_FISCH_agg_long = na.omit(dat_FISCH_agg_long)
dim(dat_FISCH_agg_long)
dat_FISCH_agg_long

dat_FISCH_complete = data.frame(dat_FISCH_agg_long, dat_FISCH)
head(dat_FISCH_complete)
dim(dat_FISCH_complete)
```
Now repeat this process for two other questions that we ask


