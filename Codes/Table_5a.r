library("mmrm")
library("emmeans")

#########################################################################
##

# PHQ-9, Whole Components

##

edat <- read.csv(file="editeddata07_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C11")	
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
	formula = PHQ9 ~ nssche + ba + cr + ps + at + bi + wl + PHQ9_0 + age + sex + working + answerIndex + nssche*answerIndex + ba*answerIndex + cr*answerIndex + ps*answerIndex + at*answerIndex + bi*answerIndex + wl*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)		

##

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:8,1],2)
CL <- round(coef2[2:8,1] - qt(0.975,df=coef2[2:8,3])*coef2[2:8,2],2)
CU <- round(coef2[2:8,1] + qt(0.975,df=coef2[2:8,3])*coef2[2:8,2],2)
P <- round(coef2[2:8,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[2:8,1]/sigma,2)
CL <- round((coef2[2:8,1] - qt(0.975,df=coef2[2:8,3])*coef2[2:8,2])/sigma,2)
CU <- round((coef2[2:8,1] + qt(0.975,df=coef2[2:8,3])*coef2[2:8,2])/sigma,2)
P <- round(coef2[2:8,5],3)

data.frame(Est,CL,CU,P)

##
##

mmrm3 <- mmrm(
	formula = PHQ9 ~ nssche + ba + cr + ps + at + bi + ba*cr + ba*ps + ba*at + ba*bi + wl + PHQ9_0 + age + sex + working + answerIndex + nssche*answerIndex + ba*answerIndex + cr*answerIndex + ps*answerIndex + at*answerIndex + bi*answerIndex + ba*cr*answerIndex + ba*ps*answerIndex + ba*at*answerIndex + ba*bi*answerIndex + wl*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	

##

coef2 <- summary(mmrm3)$coefficients

Est <- round(coef2[c(2:7,19:22,8),1],2)
CL <- round(coef2[c(2:7,19:22,8),1] - qt(0.975,df=coef2[c(2:7,19:22,8),3])*coef2[c(2:7,19:22,8),2],2)
CU <- round(coef2[c(2:7,19:22,8),1] + qt(0.975,df=coef2[c(2:7,19:22,8),3])*coef2[c(2:7,19:22,8),2],2)
P <- round(coef2[c(2:7,19:22,8),5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[c(2:7,19:22,8),1]/sigma,2)
CL <- round((coef2[c(2:7,19:22,8),1] - qt(0.975,df=coef2[c(2:7,19:22,8),3])*coef2[c(2:7,19:22,8),2])/sigma,2)
CU <- round((coef2[c(2:7,19:22,8),1] + qt(0.975,df=coef2[c(2:7,19:22,8),3])*coef2[c(2:7,19:22,8),2])/sigma,2)
P <- round(coef2[c(2:7,19:22,8),5],3)

data.frame(Est,CL,CU,P)


