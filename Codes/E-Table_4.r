library("mmrm")
library("emmeans")

#########################################################################
##

# GAD-7, All trials

##

edat <- read.csv(file="editeddata06_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[is.na(edat$GAD7)==FALSE,]

##

x1 <- factor(edat$x1)
x1 <- relevel(x1,ref="C10")		
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

aov6 <- lm(GAD7Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm2 <- mmrm(
	formula = GAD7 ~ x1 + GAD7_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)	

emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE)								
pairs(emmeans(mmrm2, ~ x1 | answerIndex), reverse = TRUE, adjust=NULL, infer=TRUE)

##

coef2 <- summary(mmrm2)$coefficients

Est <- round(coef2[2:12,1],2)
CL <- round(coef2[2:12,1] - qt(0.975,df=coef2[2:12,3])*coef2[2:12,2],2)
CU <- round(coef2[2:12,1] + qt(0.975,df=coef2[2:12,3])*coef2[2:12,2],2)
P <- round(coef2[2:12,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef2[2:12,1]/sigma,2)
CL <- round((coef2[2:12,1] - qt(0.975,df=coef2[2:12,3])*coef2[2:12,2])/sigma,2)
CU <- round((coef2[2:12,1] + qt(0.975,df=coef2[2:12,3])*coef2[2:12,2])/sigma,2)
P <- round(coef2[2:12,5],3)

data.frame(Est,CL,CU,P)

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C12"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C1"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C10"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C11"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[5,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C2"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[6,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C3"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[7,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C4"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[8,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C5"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[9,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C6"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[10,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C7"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[11,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C8"]), 2)

lsm <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[12,]
round( lsm/sd(presurv$GAD7Point[presurv$x0=="C9"]), 2)

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
emm2 <- data.frame(emm2$x1,emm2$answerIndex,round(emm2$emmean,2))
emm2 <- emm2[order(as.numeric(as.character(emm2[,2]))),]
emm2 <- emm2[order(as.numeric(emm2[,1])),]
print(emm2)

