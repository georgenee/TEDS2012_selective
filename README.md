# TEDS2012_selective
# plot in binominal regression.
# Data is from TEDS2012, NCCU, Taiwan
# update R and Rstudio version ASAP!!! 
# import R packages
> library(sjPlot)
> library(sjlabelled)
> library(sjmisc)

# There are 2 methods to deal with estimate coefficient in sjPlot.
# coding in method 1 is much shorter and automatically 1.changing variable property into "factor", 2. turning variable lable into plot chart  
# import data
# atomic.to.fac=TRUE  for turning numeric into factor.
# tag.na=TRUE  for SYSMIS.
>teds2012test<-read_spss("~/Documents/LibertyTimes/LIT08-16/teds2012_0524bㄒ.sav”, atomic.to.fac = TRUE, tag.na = TRUE)

# setting Dummy variables
# 5= value, base5=there are 5 labels in one cariable.
# NNA02a=Newspapers(Liberty Times=5=dummy), NCITY=Regions of Taiwan(Eastern Taiwan=4=duummy), NQ01BC1=Party_ID=Pan-Green Camp(1) & Pan-Blue Camp(2, dummy) 
> contrasts(teds2012test$NNA02a)<-contr.treatment(5, base=5)
> contrasts(teds2012test$NCITY)<-contr.treatment(4, base=4)
> contrasts(teds2012test$NQ01BC1)<-contr.treatment(2, base=2)

# binomial regression
> model01test<-glm(formula=NNM02ab~NQ01BC1+NNA02a+NCITY, data=teds2012test, family=binomial(link="logit"), na.action=na.exclude)
> summary(model01test)

# plot_model
#Figure 1 test ECFA
>plot_model(model01test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.02, 30), title="Figure 1   Is the Economy of Taiwan Getting Better or Worse as ECFA Was Put into Practice?")
pmodel01test<-plot_model(model01test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.02, 30), title="Figure 1   Is the Economy of Taiwan Getting Better or Worse as ECFA Was Put into Practice?")
# for ggplot
>pmodel01test<-plot_model(model01test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.02, 30), title="Figure 1   Is the Economy of Taiwan Getting Better or Worse as ECFA Was Put into Practice?")

#Figure 2  test 1992  Consensus
> model02test<-glm(formula=NN02bb~NQ01BC1+NNA02a+NCITY, data=teds2012test, family=binomial(link="logit"), na.action=na.exclude)
> summary(model02test)
>plot_model(model02test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.05, 100), title="Figure 2  1992 Consensus Is the Prerequisite for Cross-strait Negotiations")
>pmodel02test<-plot_model(model02test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.05, 100), title="Figure 2  1992 Consensus Is the Prerequisite for Cross-strait Negotiations")

#Figure 3 test Evaluationg Taiwan's Economic Performance
> model03test<-glm(formula=NE01~NQ01BC1+NNA02a+NCITY, data=teds2012test, family=binomial(link="logit"), na.action=na.exclude)
> summary(model03test)
>plot_model(model03test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 15), title="Figure 3 Evaluationg Taiwan's Economic Performance: Is It Better than Last Year?")
>pmodel03test<-plot_model(model03test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 15), title="Figure 3 Evaluationg Taiwan's Economic Performance: Is It Better than Last Year?")

#Figure 4 test RNP
> model04test<-glm(formula=NE06b~NQ01BC1+NNA02a+NCITY, data=teds2012test, family=binomial(link="logit"), na.action=na.exclude)
> summary(model04test)
>plot_model(model04test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 100000000), title="Figure 4  The Government Should Take Actions to Ameliorate the Gap Between the Rich and the Poor")
>pmodel04test<-plot_model(model04test, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 100000000), title="Figure 4  The Government Should Take Actions to Ameliorate the Gap Between the Rich and the Poor")


# method 2, writing axis label to plot manually.
#read_spss
> teds2012<-read_spss("~/Documents/LibertyTimes/LIT08-16/teds2012_0524b.sav”

# Changing variables'property manually before setting dummy variables. 
>teds2012$NE06b<-as.factor(teds2012$NE06b)

# dummy-setting
> contrasts(teds2012$NNA02a)<-contr.treatment(5, base=5)
> contrasts(teds2012$NCITY)<-contr.treatment(4, base=4)
> contrasts(teds2012$NQ01BC1)<-contr.treatment(2, base=2)

# binomial regression
> Model04<-glm(formula=NE06b~NQ01BC1+NNA02a+NCITY, data=teds2012, family=binomial(link="logit"), na.action=na.exclude)
> summary(Model04)
# plot_model
# axis.labels = c(""), "Pan-Green Camp" is at the bottom of axis and you have to code in the syntax first. 
# first in,last out.
> plot_model(Model04, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 12000000), title="Figure 4  The Government Should Take Actions to Ameliorate the Gap Between the Rich and the Poor", axis.labels = c("Pan-Green Camp", "China Times", "Economic Daily News", "United Daily News", "Apple Daily", "Southern Taiwan", "Central Taiwan", "Northern Taiwan"))
> pModel04<-plot_model(Model04, vline.color = "#edd840", show.value=TRUE, value.offset = .2, axis.lim=c(0.1, 12000000), title="Figure 4  The Government Should Take Actions to Ameliorate the Gap Between the Rich and the Poor", axis.labels = c("Pan-Green Camp", "China Times", "Economic Daily News", "United Daily News", "Apple Daily", "Southern Taiwan", "Central Taiwan", "Northern Taiwan"))
