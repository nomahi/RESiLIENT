library("mmrm")
library("emmeans")

#########################################################################
##

# ISI, Trial 4; ISI >= 8

##

edat <- read.csv(file="editeddata04_R1.csv")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]
edat <- edat[edat$ISI_0>=8,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C12")		
edat$x1 <- x1

answerIndex <- factor(edat$answerIndex)
answerIndex <- relevel(answerIndex,ref="6")		
edat$answerIndex <- answerIndex

username <- factor(edat$username)	
edat$username <- username

sex <- factor(edat$sex)		
edat$sex <- sex

##

edat6 <- edat[edat$answerIndex==6,]

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm2 <- mmrm(
	formula = PHQ9 ~ x1 + PHQ9_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)	

emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE)									
pairs(emmeans(mmrm2, ~ x1 | answerIndex), reverse = TRUE, adjust=NULL, infer=TRUE)

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:4,1],2)
CL <- round(coef2[2:4,1] - qt(0.975,df=coef2[2:4,3])*coef2[2:4,2],2)
CU <- round(coef2[2:4,1] + qt(0.975,df=coef2[2:4,3])*coef2[2:4,2],2)
P <- round(coef2[2:4,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[2:4,1]/sigma,2)
CL <- round((coef2[2:4,1] - qt(0.975,df=coef2[2:4,3])*coef2[2:4,2])/sigma,2)
CU <- round((coef2[2:4,1] + qt(0.975,df=coef2[2:4,3])*coef2[2:4,2])/sigma,2)
P <- round(coef2[2:4,5],3)

data.frame(Est,CL,CU,P)

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + BI + BA*BI + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + BA*BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	




#########################################################################
##

# ISI, Trial 4; ISI <= 8

##

edat <- read.csv(file="editeddata04_R1.csv")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]
edat <- edat[edat$ISI_0<=7,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C12")		
edat$x1 <- x1

answerIndex <- factor(edat$answerIndex)
answerIndex <- relevel(answerIndex,ref="6")		
edat$answerIndex <- answerIndex

username <- factor(edat$username)	
edat$username <- username

sex <- factor(edat$sex)		
edat$sex <- sex

##

edat6 <- edat[edat$answerIndex==6,]

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm2 <- mmrm(
	formula = PHQ9 ~ x1 + PHQ9_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)	

emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE)						
pairs(emmeans(mmrm2, ~ x1 | answerIndex), reverse = TRUE, adjust=NULL, infer=TRUE)	

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:4,1],2)
CL <- round(coef2[2:4,1] - qt(0.975,df=coef2[2:4,3])*coef2[2:4,2],2)
CU <- round(coef2[2:4,1] + qt(0.975,df=coef2[2:4,3])*coef2[2:4,2],2)
P <- round(coef2[2:4,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[2:4,1]/sigma,2)
CL <- round((coef2[2:4,1] - qt(0.975,df=coef2[2:4,3])*coef2[2:4,2])/sigma,2)
CU <- round((coef2[2:4,1] + qt(0.975,df=coef2[2:4,3])*coef2[2:4,2])/sigma,2)
P <- round(coef2[2:4,5],3)

data.frame(Est,CL,CU,P)

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + BI + BA*BI + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + BA*BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	





#########################################################################
##

# ISI, Trial 4; Interaction test

##

edat <- read.csv(file="editeddata04_R1.csv")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]
edat$z8 <- factor(as.numeric(edat$ISI_0>=8))

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C12")		
edat$x1 <- x1

answerIndex <- factor(edat$answerIndex)
answerIndex <- relevel(answerIndex,ref="6")		
edat$answerIndex <- answerIndex

username <- factor(edat$username)	
edat$username <- username

sex <- factor(edat$sex)		
edat$sex <- sex

##

mmrm4 <- mmrm(
	formula = PHQ9 ~ x1 + PHQ9_0 + age + sex + working + answerIndex + x1*answerIndex + x1*z8 + x1*z8*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm4)	

