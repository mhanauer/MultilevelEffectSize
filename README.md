---
title: "ECLS-K-2011"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Getting effect sizes for multilevel models
```{r}

dep = rnorm(50000)
time = rep(1:10, 5000)
female = as.factor(c(rep(1,25000), rep(0,25000)))
size = abs(rnorm(50000)*10000)
childID = as.factor(rep(1:5000, each = 10))
childID = as.factor(childID)
schoolID = as.factor(rep(1:500, each = 100))
vamData = cbind(dep, time, female, size, childID, schoolID)
names(vamData) = tolower(names(vamData))
head(vamData)

library(nlme)
vamData = data.frame(vamData)
vamData$female = factor(vamData$female)
vamData$childID = factor(vamData$childID)
vamData$schoolID = factor(vamData$schoolID)
vamDataInd = groupedData(dep ~ time | schoolID/childID, data = vamData)
vamDataInd$female = factor(vamDataInd$female)
vamDataInd$childID = factor(vamDataInd$childID)
vamDataInd$schoolID = factor(vamDataInd$schoolID)
head(vamDataInd)

unconVamData = lme(dep ~ time, random =~ time | schoolID/childID, data = vamDataInd, method = "ML")
summary(unconVamData)

install.packages("MuMIn")
library(MuMIn)
r.squaredGLMM(unconVamData)

```
R2m is the pesduo R^2 for fixed effect and R2c includes both fixed and random effects.
