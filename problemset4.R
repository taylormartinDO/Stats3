

library(tidyverse) 
framData = read_csv("44213-FraminghamPS4bin.csv")

#Explore the data using descriptive statistics for the 641 time bins (you do not have the individual data in this binned data set): 
table(framData$bmicat, useNA = "always") 
prop.table(framData) 
summary(framData)
framData %>% as.data.frame()

#Explore several Poisson regression models using these grouped survival data and select between models
model1 = glm(D ~ gender, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model1) 

AIC(model1) 

#1a
model1a = glm(D ~ gender + cursmoke, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model1a) 

AIC(model1a) 

exp(model1a$coefficients)

#1b
model1b = glm(D ~ gender + tbin, offset = log(Y), data =  framData,
              family=poisson(link="log")) 
summary(model1b) 

AIC(model1b) 

exp(model1b$coefficients)

#1c
model1c = glm(D ~ tbin, offset = log(Y), data =  framData,
              family=poisson(link="log")) 
summary(model1c) 

AIC(model1c) 

exp(model1c$coefficients)

#model2 dm
model2 = glm(D ~ diabetes, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model2) 
exp(model2$coefficients)
AIC(model2) 

#2a
model2a = glm(D ~ diabetes + tbin, offset = log(Y), data =  framData,
              family=poisson(link="log")) 
summary(model2a) 

AIC(model2a) 

exp(model2a$coefficients)

#model3 smoking
model3 = glm(D ~ cursmoke, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model3) 
AIC(model3) 
exp(model3$coefficients)

#model4 bpmeds
model4 = glm(D ~ bpmeds, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model4) 
AIC(model4)

#model5 bmicat
model5 = glm(D ~ bmicat, offset = log(Y), data =  framData,
             family=poisson(link="log")) 
summary(model5) 

AIC(model1) 
AIC(model2) 
AIC(model3) 
AIC(model4) 
AIC(model5) 

#Check the assumptions of your Poisson models; use other models as appropriate
# Pearson chi-square goodness-of-  fit test (like poisgof in Stata) 
X2 = sum(residuals(model3, type = "pearson")^2); X2 
df = model$df.residual; df
pval = 1-pchisq(X2, df); pval 

# Negative binomial regression 
library(MASS)
model2 = glm.nb(D ~ gender + offset(log(Y)), data=framData)
summary(model2)













