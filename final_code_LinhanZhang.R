data = read.csv(file = 'diabetes.csv')
library(dplyr)
library(tidyverse)
set.seed(1005)
#drop unusable predictor variables
df = subset(data, select = -c(encounter_id, patient_nbr, admission_source_id,
                              payer_code, encounter_num, weight))
#Visualize all variables.
par(mfrow=c(3,4))
hist(as.numeric(df$age), main = 'age')
hist(as.numeric(df$gender), main = 'gender')
hist(as.numeric(df$discharge_disposition_id), main = 'discharge_disposition_id')
hist(as.numeric(df$Length.of.Stay), main = 'Length.of.Stay')
hist(as.numeric(df$num_lab_procedures), main = 'num_lab_procedures')
hist(as.numeric(df$num_procedures), main = 'num_procedures')
hist(as.numeric(df$num_medications), main = 'num_medications')
hist(as.numeric(df$number_outpatient), main = 'number_outpatient')
hist(as.numeric(df$number_emergency), main = 'number_emergency')
hist(as.numeric(df$number_inpatient), main = 'number_inpatient')
hist(as.numeric(df$number_diagnoses), main = 'number_diagnoses')
hist(as.numeric(df$max_glu_serum), main = 'max_glu_serum')
hist(as.numeric(df$metformin), main = 'metformin')
hist(as.numeric(df$repaglinide), main = 'repaglinide')
hist(as.numeric(df$nateglinide), main = 'nateglinide')
hist(as.numeric(df$chlorpropamide), main = 'chlorpropamide')
hist(as.numeric(df$glimepiride), main = 'glimepiride')
hist(as.numeric(df$acetohexamide), main = 'acetohexamide')
hist(as.numeric(df$glipizide), main = 'glipizide')
hist(as.numeric(df$glyburide), main = 'glyburide')
hist(as.numeric(df$tolbutamide), main = 'tolbutamide')
hist(as.numeric(df$insulin), main = 'insulin')
hist(as.numeric(df$glyburide.metformin), main = 'glyburide.metformin')
hist(as.numeric(df$glipizide.metformin), main = 'glipizide.metformin')
hist(as.numeric(df$glimepiride.pioglitazone), main = 'glimepiride.pioglitazone')
hist(as.numeric(df$metformin.rosiglitazone), main = 'metformin.rosiglitazone')
hist(as.numeric(df$change), main = 'change')
hist(as.numeric(df$diabetesMed), main = 'diabetesMed')
hist(as.numeric(df$readmitted), main = 'readmitted')
#Converge predictor variables
#Male = 1 Female = 0
unique(data$gender)
sum(df$gender == 'Unknown/Invalid')
#delete the invalid observations
df = filter(df, gender != 'Unknown/Invalid')
df$gender[df$gender == 'Male'] = 0
df$gender[df$gender == 'Female'] = 1
#readmitted: NO is 0, <30 is 1, >30 is 2
df$readmitted[df$readmitted == 'NO'] = 0
df$readmitted[df$readmitted == '<30'] = 1
df$readmitted[df$readmitted == '>30'] = 1
#age: [0-10):0, [10-20):1, [20-30):2, [30-40):3, [40-50):4, [50-60):5, [60-70):6
# [70-80):7, [80-90):8, [90-100):9
df$age[df$age == '[0-10)'] = 0
df$age[df$age == '[10-20)'] = 1
df$age[df$age == '[20-30)'] = 2
df$age[df$age == '[30-40)'] = 3
df$age[df$age == '[40-50)'] = 4
df$age[df$age == '[50-60)'] = 5
df$age[df$age == '[60-70)'] = 6
df$age[df$age == '[70-80)'] = 7
df$age[df$age == '[80-90)'] = 8
df$age[df$age == '[90-100)'] = 9
#diabetesMed: Yes = 1, No = 0
unique(df$diabetesMed)
df$diabetesMed <- ifelse(df$diabetesMed =="Yes", 1, 0)
#race
unique(data$race)
sum(is.na(data$race))
#there is lots of nas in race delete that variable
df = subset(df, select = -race)
#medical_specifalty
sum(is.na(df$medical_specialty))
#too manny na delete this variable
df = subset(df, select = -medical_specialty)
#max_glu_serum
unique(df$max_glu_serum)
#max_glu_serum: None: 0, Norm: 1, >200: 2, >300: 3
df$max_glu_serum[df$max_glu_serum == 'None'] = 0
df$max_glu_serum[df$max_glu_serum == 'Norm'] = 1
df$max_glu_serum[df$max_glu_serum == '>200'] = 2
df$max_glu_serum[df$max_glu_serum == '>300'] = 3
#A1Cresult: None:0, Norm:1, >7: 2, >8: 3
unique(df$A1Cresult)
df$A1Cresult[df$A1Cresult == 'None'] = 0
df$A1Cresult[df$A1Cresult == 'Norm'] = 1
df$A1Cresult[df$A1Cresult == '>7'] = 2
df$A1Cresult[df$A1Cresult == '>8'] = 3
#metformin: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$metformin)
df$metformin[df$metformin == 'No'] = 0
df$metformin[df$metformin == 'Down'] = 1
df$metformin[df$metformin == 'Steady'] = 2
df$metformin[df$metformin == 'Up'] = 3
#repaglinide: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$repaglinide)
df$repaglinide[df$repaglinide == 'No'] = 0
df$repaglinide[df$repaglinide == 'Down'] = 1
df$repaglinide[df$repaglinide == 'Steady'] = 2
df$repaglinide[df$repaglinide == 'Up'] = 3
#nateglinide: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$nateglinide)
df$nateglinide[df$nateglinide == 'No'] = 0
df$nateglinide[df$nateglinide == 'Down'] = 1
df$nateglinide[df$nateglinide == 'Steady'] = 2
df$nateglinide[df$nateglinide == 'Up'] = 3
#chlorpropamide: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$chlorpropamide)
df$chlorpropamide[df$chlorpropamide == 'No'] = 0
df$chlorpropamide[df$chlorpropamide == 'Down'] = 1
df$chlorpropamide[df$chlorpropamide == 'Steady'] = 2
df$chlorpropamide[df$chlorpropamide == 'Up'] = 3
#glimepiride: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$glimepiride)
df$glimepiride[df$glimepiride== 'No'] = 0
df$glimepiride[df$glimepiride == 'Down'] = 1
df$glimepiride[df$glimepiride == 'Steady'] = 2
df$glimepiride[df$glimepiride == 'Up'] = 3
#acetohexamide: No: 0, Steady: 1
unique(df$acetohexamide)
df$acetohexamide[df$acetohexamide== 'No'] = 0
df$acetohexamide[df$acetohexamide == 'Steady'] = 1
#glipizide: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$glipizide)
df$glipizide[df$glipizide== 'No'] = 0
df$glipizide[df$glipizide == 'Down'] = 1
df$glipizide[df$glipizide == 'Steady'] = 2
df$glipizide[df$glipizide == 'Up'] = 3
#glyburide: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$glyburide)
df$glyburide[df$glyburide== 'No'] = 0
df$glyburide[df$glyburide == 'Down'] = 1
df$glyburide[df$glyburide == 'Steady'] = 2
df$glyburide[df$glyburide == 'Up'] = 3
#tolbutamide: No: 0, Steady: 1
unique(df$tolbutamide)
df$tolbutamide[df$tolbutamide== 'No'] = 0
df$tolbutamide[df$tolbutamide == 'Steady'] = 1
#pioglitazone: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$pioglitazone)
df$pioglitazone[df$pioglitazone== 'No'] = 0
df$pioglitazone[df$pioglitazone == 'Down'] = 1
df$pioglitazone[df$pioglitazone == 'Steady'] = 2
df$pioglitazone[df$pioglitazone == 'Up'] = 3
#rosiglitazone: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$rosiglitazone)
df$rosiglitazone[df$rosiglitazone== 'No'] = 0
df$rosiglitazone[df$rosiglitazone == 'Down'] = 1
df$rosiglitazone[df$rosiglitazone == 'Steady'] = 2
df$rosiglitazone[df$rosiglitazone== 'Up'] = 3
#acarbose: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$acarbose)
df$acarbose[df$acarbose== 'No'] = 0
df$acarbose[df$acarbose == 'Down'] = 1
df$acarbose[df$acarbose == 'Steady'] = 2
df$acarbose[df$acarbose== 'Up'] = 3
#miglitol: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$miglitol)
df$miglitol[df$miglitol== 'No'] = 0
df$miglitol[df$miglitol == 'Down'] = 1
df$miglitol[df$miglitol == 'Steady'] = 2
df$miglitol[df$miglitol== 'Up'] = 3
#troglitazone: No: 0, Steady: 1
unique(df$troglitazone)
df$troglitazone[df$troglitazone== 'No'] = 0
df$troglitazone[df$troglitazone == 'Steady'] = 1
#tolazimide: No: 0, Steady: 1 Up: 2
unique(df$tolazamide)
df$tolazamide[df$tolazamide== 'No'] = 0
df$tolazamide[df$tolazamide == 'Steady'] = 1
df$tolazamide[df$tolazamide == 'Up'] = 2
#examide
unique(df$examide)
#Since there is only one condition for this variable, I will delete this predictor
df = subset(df, select = -examide)
#citoglipton delete
unique(df$citoglipton)
df = subset(df, select = -citoglipton)
#insulin: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$insulin)
df$insulin[df$insulin== 'No'] = 0
df$insulin[df$insulin == 'Down'] = 1
df$insulin[df$insulin == 'Steady'] = 2
df$insulin[df$insulin== 'Up'] = 3
#glyburide.metformin: No: 0, Down: 1, Steady:  2, Up: 3
unique(df$glyburide.metformin)
df$glyburide.metformin[df$glyburide.metformin == 'No'] = 0
df$glyburide.metformin[df$glyburide.metformin == 'Down'] = 1
df$glyburide.metformin[df$glyburide.metformin == 'Steady'] = 2
df$glyburide.metformin[df$glyburide.metformin == 'Up'] = 3
#glipizide.metformin: No: 0, Steady: 1
unique(df$glipizide.metformin)
df$glipizide.metformin[df$glipizide.metformin == 'No'] = 0
df$glipizide.metformin[df$glipizide.metformin == 'Steady'] = 1
#glimepiride.pioglitazone: No: 0, Steady: 1
unique(df$glimepiride.pioglitazone)
df$glimepiride.pioglitazone[df$glimepiride.pioglitazone == 'No'] = 0
df$glimepiride.pioglitazone[df$glimepiride.pioglitazone == 'Steady'] = 1
#metformin.rosiglitazone: No: 0, Steady: 1
unique(df$metformin.rosiglitazone)
df$metformin.rosiglitazone[df$metformin.rosiglitazone == 'No'] = 0
df$metformin.rosiglitazone[df$metformin.rosiglitazone == 'Steady'] = 1
#metformin.pioglitazone: No: 0, Steady: 1
unique(df$metformin.pioglitazone)
df$metformin.pioglitazone[df$metformin.pioglitazone == 'No'] = 0
df$metformin.pioglitazone[df$metformin.pioglitazone == 'Steady'] = 1
#change: No: 0, Ch: 1
unique(df$change)
df$change[df$change == 'No'] = 0
df$change[df$change == 'Ch'] = 1
#delete X
df = subset(df, select = -X)
#boxplot of all variables
boxplot(as.numeric(df$gender), main = 'gender')
#select training data and testing data
library(caTools)
sample = sample.split(df,SplitRatio = 0.75)
train =subset(df,sample ==TRUE)
test =subset(df, sample==FALSE)
#chi-square test delete variables
#H0: independent, Ha: dependent 
chisq.test(train$readmitted,train$age)
chisq.test(train$readmitted,train$admission_type_id)
chisq.test(train$readmitted,train$gender)
chisq.test(train$readmitted,train$discharge_disposition_id)
chisq.test(train$readmitted,train$Length.of.Stay)
chisq.test(train$readmitted,train$num_procedures)
chisq.test(train$readmitted,train$num_medications)
chisq.test(train$readmitted,train$number_outpatient)
chisq.test(train$readmitted,train$number_emergency)
chisq.test(train$readmitted,train$number_inpatient)
chisq.test(train$readmitted,train$number_diagnoses)
chisq.test(train$readmitted,train$max_glu_serum)
chisq.test(train$readmitted,train$AICresult)
chisq.test(train$readmitted,train$metformin)
chisq.test(train$readmitted,train$repaglinide)
chisq.test(train$readmitted,train$nateglinide)
chisq.test(train$readmitted,train$chlorpropamide)
#p value > 0.05
chisq.test(train$readmitted,train$glimepiride)
#p value > 0.05
chisq.test(train$readmitted,train$acetohexamide)
chisq.test(train$readmitted,train$glipizide)
chisq.test(train$readmitted,train$glyburide)
chisq.test(train$readmitted,train$tolbutamide)
chisq.test(train$readmitted,train$pioglitazone)
chisq.test(train$readmitted,train$rosiglitazone)
chisq.test(train$readmitted,train$acarbose)
chisq.test(train$readmitted,train$miglitol)
#p value > 0.05
chisq.test(train$readmitted,train$troglitazone)
#p value > 0.05
chisq.test(train$readmitted,train$tolazamide)
#p value > 0.05
chisq.test(train$readmitted,train$insulin)
chisq.test(train$readmitted,train$glyburide.metformin)
#p value > 0.05
chisq.test(train$readmitted,train$glimepiride.pioglitazone)
#p value > 0.05
chisq.test(train$readmitted,train$metformin.rosiglitazone)
#p value > 0.05
chisq.test(train$readmitted,train$metformin.pioglitazone)
#p value > 0.05
chisq.test(train$readmitted,train$change)
chisq.test(train$readmitted,train$diabetesMed)
#Fit a glm model without chi square p value > 0.05
model <- glm(as.numeric(train$readmitted) ~ age + gender+admission_type_id+
                Length.of.Stay+num_procedures+num_medications+number_outpatient+
                number_emergency+number_inpatient+number_diagnoses+max_glu_serum+
                A1Cresult+ metformin+repaglinide+nateglinide+
                glipizide+glyburide+tolbutamide+pioglitazone+rosiglitazone+
                acarbose+insulin+change+diabetesMed, family=binomial, data=train)
summary(model)
#choose variables using step wise bic
model_bic = step(model1, direction = "backward", k = log(74938))
summary(model_bic)
model_bic
#select variable
model1 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
               Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
               number_inpatient + number_diagnoses + metformin + insulin + 
               diabetesMed, family = binomial, data = train)
summary(model1)


library(MASS)
library(lme4)

model_mix1 = lmer(as.numeric(readmitted)~ age + Length.of.Stay + num_procedures 
                   + number_outpatient + number_emergency + 
                     number_inpatient + number_diagnoses + metformin  + insulin + 
                     diabetesMed+(1|admission_type_id), data = train)
model_mix2 = lmer(as.numeric(train$readmitted) ~ age + admission_type_id + 
                    Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                    number_inpatient + number_diagnoses + metformin + insulin + 
                    diabetesMed+ (1|Length.of.Stay), data = train)
model_mix3 = lmer(as.numeric(train$readmitted) ~ age + admission_type_id + 
                    Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                    number_inpatient + number_diagnoses + metformin + insulin + 
                    diabetesMed+ (1|discharge_disposition_id), data = train)
summary(model_mix1)
summary(model_mix2)
summary(model_mix3)
AIC(model1,model_mix1)
AIC(model1, model_mix2)
AIC(model1,model_mix3)
#Validate all variables for model1
model_v1 = glm(formula = as.numeric(train$readmitted) ~  admission_type_id + 
                 Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v1, model1, test = 'Chisq')
model_v2 = glm(formula = as.numeric(train$readmitted) ~ age  + 
                 Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)

anova(model_v2, model1, test = 'Chisq')
model_v3 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                  num_procedures + number_outpatient + number_emergency + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v3, model1, test = 'Chisq')
model_v4 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay  + number_outpatient + number_emergency + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v4, model1, test = 'Chisq')
model_v5 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay + num_procedures + number_emergency + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v5, model1, test = 'Chisq')
model_v6 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay + num_procedures + number_outpatient  + 
                 number_inpatient + number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v6, model1, test = 'Chisq')
model_v7 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                  number_diagnoses + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v7, model1, test = 'Chisq')
model_v8 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                 number_inpatient  + metformin + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v8, model1, test = 'Chisq')
model_v9 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                 Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                 number_inpatient + number_diagnoses + insulin + 
                 diabetesMed, family = binomial, data = train)
anova(model_v9, model1, test = 'Chisq')
model_v10 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                  Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                  number_inpatient + number_diagnoses + metformin  + 
                  diabetesMed, family = binomial, data = train)
anova(model_v10, model1, test = 'Chisq')
model_v11 = glm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                  Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                  number_inpatient + number_diagnoses + metformin + insulin  
                , family = binomial, data = train)
anova(model_v11, model1, test = 'Chisq')
#Model diagnostic

#Plot residual 
par(mfrow=c(1,2))
plot(model2, 2)
plot(model2, 1)

#Internal Validation
library(rms)
logit.mod <- lrm(formula = as.numeric(train$readmitted) ~ age + admission_type_id + 
                   Length.of.Stay + num_procedures + number_outpatient + number_emergency + 
                   number_inpatient + number_diagnoses + metformin + insulin + 
                   diabetesMed,
                 data = train, x = TRUE, y = TRUE, model = T)
## Cross validation ##
cross.calib <- calibrate(logit.mod, method="crossvalidation", B = 10)
plot(cross.calib, las = 1, xlab = 'Predicted Probability')
pred.prob <- predict(model_mix2, type = "response")
### ROC curve ###
library(pROC)
pred.prob <- predict(model1, type = "response")
## The True Positive Rate ##
p = fitted(model1)
roc_logit = roc(train$readmitted~p)
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, 
     lwd = 2,col = 'red')
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))

### Validating on the test data ##
pred.prob <- predict(logit.mod, newdata = train, type = "fitted")
test$pred.prob <- predict(model1, newdata = test, type = "response") 
deciles <- quantile(test$pred.prob, probs = seq(0,1, by =0.1)) 
test$decile <- findInterval(test$pred.prob, deciles, rightmost.closed = T)
pred.prob <- tapply(test$pred.prob, test$decile, mean)
obs.prob <- tapply(as.numeric(test$readmitted), test$decile, mean)
## The plot ##
par(family = 'serif')
plot(pred.prob, obs.prob, type = "l", ylab = "Observed", xlab = "Predicted", 
     xlim = c(0,1), ylim = c(0,1))
abline(a=0, b=1)
#ok
#validation residuals
#Perform predictions
pred.y = predict(model1, newdata = test, type = 'response')
pred.y
#Prediction error
mean((as.numeric(test$readmitted) - pred.y)^2)
summary(model1)


