library("mmrm")
library("emmeans")

#########################################################################
##

# PHQ-9, Whole Components

##

edat <- read.csv(file="editeddata06_WC26W.csv")
load(file="presurv_edited_WC26W.RData")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C11")		
edat$x1 <- x1

answerIndex <- factor(edat$answerIndex)
answerIndex <- relevel(answerIndex,ref="26")		
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
	formula = PHQ9 ~ nssche + ba + cr + ps + at + bi + PHQ9_0 + age + sex + working + answerIndex + nssche*answerIndex + ba*answerIndex + cr*answerIndex + ps*answerIndex + at*answerIndex + bi*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)	

coef26 <- summary(mmrm2)$coefficients

##

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:7,1],2)
CL <- round(coef2[2:7,1] - qt(0.975,df=coef2[2:7,3])*coef2[2:7,2],2)
CU <- round(coef2[2:7,1] + qt(0.975,df=coef2[2:7,3])*coef2[2:7,2],2)
P <- round(coef2[2:7,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[2:7,1]/sigma,2)
CL <- round((coef2[2:7,1] - qt(0.975,df=coef2[2:7,3])*coef2[2:7,2])/sigma,2)
CU <- round((coef2[2:7,1] + qt(0.975,df=coef2[2:7,3])*coef2[2:7,2])/sigma,2)
P <- round(coef2[2:7,5],3)

data.frame(Est,CL,CU,P)

##
##

mmrm3 <- mmrm(
	formula = PHQ9 ~ nssche + ba + cr + ps + at + bi + ba*cr + ba*ps + ba*at + ba*bi + PHQ9_0 + age + sex + working + answerIndex + nssche*answerIndex + ba*answerIndex + cr*answerIndex + ps*answerIndex + at*answerIndex + bi*answerIndex + ba*cr*answerIndex + ba*ps*answerIndex + ba*at*answerIndex + ba*bi*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	

coef26 <- summary(mmrm3)$coefficients

##

coef2 <- summary(mmrm3)$coefficients

Est <- round(coef2[c(2:7,23:26),1],2)
CL <- round(coef2[c(2:7,23:26),1] - qt(0.975,df=coef2[c(2:7,23:26),3])*coef2[c(2:7,23:26),2],2)
CU <- round(coef2[c(2:7,23:26),1] + qt(0.975,df=coef2[c(2:7,23:26),3])*coef2[c(2:7,23:26),2],2)
P <- round(coef2[c(2:7,23:26),5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[c(2:7,23:26),1]/sigma,2)
CL <- round((coef2[c(2:7,23:26),1] - qt(0.975,df=coef2[c(2:7,23:26),3])*coef2[c(2:7,23:26),2])/sigma,2)
CU <- round((coef2[c(2:7,23:26),1] + qt(0.975,df=coef2[c(2:7,23:26),3])*coef2[c(2:7,23:26),2])/sigma,2)
P <- round(coef2[c(2:7,23:26),5],3)

data.frame(Est,CL,CU,P)

