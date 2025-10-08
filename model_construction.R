rm(list = ls())
setwd("C:/Users/Gebruiker/Documents/Masters CCS/Year1/Block12/Methods in CCS/algebra/methodsccs-group3/methodsccs-group3/")
dat = read.csv("fulldata_combined.csv", header = TRUE)
head(dat)
library(dplyr)
dat_filtered <- filter(dat, correct == 1)
summary(dat_filtered)
dat_filtered <- select(dat_filtered, -correct)
dat_filtered <- select(dat_filtered, -practice)
summary(dat_filtered)
table(dat_filtered$subject)
table(dat_filtered$problem_type)
table(dat_filtered$Condition)
dat_filtered$subject_nr <- as.factor(dat_filtered$subject_nr)
dat_filtered$problem_type <- as.factor(dat_filtered$problem_type)
dat_filtered$trial <- as.factor(dat_filtered$trial)
dat_filtered$Condition <- as.factor(dat_filtered$Condition)
hist(dat_filtered$RT_response)
#histogram shows non-normal distribution
hist(log(dat_filtered$RT_response))
#histogram of the log is much more normally distributed
dat_filtered$log_RT <- log(dat_filtered$RT_response)
dat_filtered$cLog_RT = scale(dat_filtered$log_RT)
#Including fixed effects for problem type and condition and their interactions, since that's what we're interested in investigating
#Including random intercepts for subject bc we're controlling for subject variation
#Including random slopes for subject:problem_type and subject:condition bc we expect them to interact
#Including random intercepts for trial bc it might have an effect that we want to control for
#We will include random slopes for trial and problem_type/condition and see if they contribute anything to the model

m1 = bam(cLog_RT ~ Condition + problem_type + Condition:problem_type + s(subject_nr, bs='re') + s(trial, bs='re') + s(subject_nr, problem_type, bs='re') + s(subject_nr, Condition, bs='re') + s(trial, problem_type, bs='re') + s(trial, Condition, bs='re'), data=dat_filtered)
summary(m1)
gam.vcomp(m1)
m2 = bam(cLog_RT ~ Condition + problem_type + Condition:problem_type + s(subject_nr, bs='re') + s(trial, bs='re') + s(subject_nr, problem_type, bs='re') + s(subject_nr, Condition, bs='re') + s(trial, problem_type, bs='re'), data=dat_filtered)
summary(m2)
gam.vcomp(m2)
library(itsadug)
compareML(m1, m2)
#Model 2 is better because it uses less degrees of freedom for same fREML score, which we expected based on the significance scores
#Let's try removing the random slopes for subject
m3 = bam(cLog_RT ~ Condition + problem_type + Condition:problem_type + s(subject_nr, bs='re') + s(trial, bs='re') + s(subject_nr, problem_type, bs='re') + s(trial, problem_type, bs='re'), data=dat_filtered)
compareML(m2, m3)
#m2 has lower fREML score, so we are sticking with m2. We are maximalising, so we don't care if it uses more edf
m4 = bam(cLog_RT ~ Condition + problem_type + Condition:problem_type + s(subject_nr, bs='re') + s(trial, bs='re') + s(subject_nr, Condition, bs='re') + s(trial, problem_type, bs='re'), data=dat_filtered)
compareML(m2, m4)
#Once again, model m2 has lower fREML score, so we're sticking with that (even if it uses more edf)
#We won't even try removing the last random slope (trial:problem type) bc the significance scores already tell us it's very significant
#Now we can try removing the random intercepts. Based on significance scores, only trial doesn't seem to be contributing anything
m5 = bam(cLog_RT ~ Condition + problem_type + Condition:problem_type + s(subject_nr, bs='re') + s(subject_nr, problem_type, bs='re') + s(subject_nr, Condition, bs='re') + s(trial, problem_type, bs='re'), data=dat_filtered)
compareML(m2, m5)
#Indeed, the fREML score is the same but model m5 uses less edf, so we are sticking with that
#There is nothing else worth removing, so our final model is model m5
summary(m5)
