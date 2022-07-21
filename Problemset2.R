library(tidyverse) 
pbcData = read_csv("44213-pbctrial.csv")

#Explore the data using descriptive statistics
table(pbcData) 
prop.table(pbcData)
summary(pbcData)
summary(pbcData$drug)
table1<- table(pbcData$drug)
table1
prop.table(table1)

table2<- table(pbcData$drug, pbcData$histo)
table2
prop.table(table2)

table3<- table(pbcData$histo)
table3
prop.table(table3)

table4<- table(pbcData$drug, pbcData$death)
table4
prop.table(table4)

table5<- table(pbcData$ageyr, pbcData$drug)
table5
prop.table(table4)

table5<- table(pbcData$agecat, pbcData$drug)
table5
prop.table(table4)

table5<- table(pbcData$drug, pbcData$agecat)
table5

#Define a survival object, defining the time variable (survyr) and the event (death == 1).  To do this, you must first install and load the “survival” package:
install.packages("survival")  ## only run this the first time
library(survival) 

pbcData$SurvObj = with(pbcData, Surv(survyr, death == 1))

#C Explore differences in time to death by different baseline variables using graphs and complementary log-log plots
# estimate survival curves for entire sample 
km.overall = survfit(SurvObj ~ 1, data = pbcData,
                     type="kaplan-meier", conf.type="log-log") 
km.overall 
summary(km.overall)

# estimate survival curves for drug group
km.drug = survfit(SurvObj ~ drug, data = pbcData,
                  type="kaplan-meier", conf.type="log-log") 
km.drug 
summary(km.drug)

# plot km curves 
plot(km.overall) 
plot(km.drug)
plot(km.drug, ylab="Survival probability",
     xlab="Time", col = c("red", "blue"))

# log rank test for equality of survivor functions 
survdiff(SurvObj ~ drug, data=pbcData) 

# complimentary log-log plot 
plot(km.drug, fun="cloglog", ylab="log(-log(Survival Probability)",
     xlab="Analysis time (shown on log scale)")



#d.Fit several Cox proportional hazards regression models to the ungrouped survival data:
model1 = coxph(SurvObj ~ drug, data =  pbcData) 
summary(model1)

model2 = coxph(SurvObj ~ sex + bil + as.factor(histo), data = pbcData) 
summary(model2)






