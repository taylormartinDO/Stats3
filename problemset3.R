
library(tidyverse) 
binData = read_csv("44213-binlymph.csv")
print(binData) 

qplot(x=mid_days, y=Survival,  
      col=factor(stage, labels=c("Stage 3", "Stage 4")), 
      data=binData) + geom_line() + labs(col="Cancer Stage")

binData = binData %>%  
  mutate(t = mid_days) %>%  
  mutate(N = P_Days)

binData = binData %>%   
  mutate(t60 = t-60) %>%  
  mutate(t60sp = ifelse(t > 60, t-60, 0))

# Model A: stage 
modelA = glm(D ~ stage, offset=log(N),
             family=poisson(link="log"), data=binData) 
summary(modelA) 
modelA$coefficients; confint.default(modelA)  ## coefficients 
exp(modelA$coefficients); exp(confint.default(modelA))  ## IRR

# Model B: stage + t-60 
modelB = glm(D ~ stage + t60, offset=log(N), 
             family=poisson(link="log"), data=binData) 
summary(modelB) 
modelB$coefficients; confint.default(modelB)  ## coefficients 
exp(modelB$coefficients); exp(confint.default(modelB))  ## IRR

# Model C: stage + t-60 + (t-60)^+ 
modelC = glm(D ~ stage + t60 + t60sp, offset=log(N),
             family=poisson(link="log"), data=binData)
summary(modelC) 
modelC$coefficients; confint.default(modelC)  ## coefficients 
exp(modelC$coefficients); exp(confint.default(modelC))  ## IRR


# Model D: stage + t-60 + (t-60)^+ + stage*(t‑60) + stage*(t‑60)^+ 
modelD = glm(D ~ stage + t60 + t60sp + stage:t60 + stage:t60sp,
             offset=log(N), family=poisson(link="log"), data=binData)
summary(modelD) 
modelD$coefficients; confint.default(modelD)  ## coefficients 
exp(modelD$coefficients); exp(confint.default(modelD))  ## IRR


# Use the    AIC = -2 log likelihood + 2(# of parameters) to identify the “best” prediction model from among A-D.  Interpret the model results in a few sentences, as if for a journal article.
AIC(modelA, modelB, modelC, modelD)

#Kaplan Meier
lymphData = read_csv("44213-lymphoma.csv") 
head(lymphData) 

library(survival)

lymphData$SurvObj = with(lymphData, Surv(days, died == 1))

km.stage = survfit(SurvObj ~ stage, data = lymphData,  
                   type="kaplan-meier", conf.type="log-log")
summary(km.stage) 

plot(km.stage, col=c("red","blue"),   
     main="Kaplan-Meier survival estimates by cancer stage",  
     ylab="S(t)", xlab="Days" ) 
legend("topright", c("Stage 3", "Stage 4"),  
     col=c("red", "blue"), lty=1)

# Carry out a log-rank test and determine a p-value for the null hypothesis that the two population survival curves are the same for Stage 4 -vs- Stage 3 patients
survdiff(SurvObj ~ stage, data=lymphData) 

# Fit a Cox proportional hazards model with an arbitrary baseline hazard and a group effect for stage
model1 = coxph(SurvObj ~ stage, data =  lymphData, ties="breslow")
summary(model1)













