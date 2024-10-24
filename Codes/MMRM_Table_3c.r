library("mmrm")
library("emmeans")

#########################################################################
##

# PHQ-9, Trial 1

##

edat <- read.csv(file="editeddata06_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[edat$x1=="C1"|edat$x1=="C2"|edat$x1=="C6"|edat$x1=="C10",]

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

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

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

presurv01 <- presurv[((presurv$x0=="C1")|(presurv$x0=="C2")|(presurv$x0=="C6")|(presurv$x0=="C10")),]

aov4 <- lm(PHQ9Point ~ factor(x0),data=presurv01)
sigma0 <- summary(aov4)$sigma	

##

mmrm1 <- mmrm(
	formula = PHQ9 ~ BA + CR + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + CR*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm1)	

coef1 <- summary(mmrm1)$coefficients

Est <- round(coef1[2:3,1],2)
CL <- round(coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
CU <- round(coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef1[2:3,1]/sigma,2)
CL <- round((coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
CU <- round((coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)


##

mmrm2 <- mmrm(
	formula = PHQ9 ~ x1 + PHQ9_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm2)	

emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE)						
pairs(emmeans(mmrm2, ~ x1 | answerIndex), reverse = TRUE, adjust=NULL, infer=TRUE)

##

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

emm1 <- summary(emmeans(mmrm1, ~ BA | answerIndex, infer=TRUE))
emm1 <- data.frame(emm1$BA,emm1$answerIndex,round(emm1$emmean,2))
emm1 <- emm1[order(as.numeric(as.character(emm1[,2]))),]
emm1 <- emm1[order(as.numeric(emm1[,1])),]
print(emm1)

emm1 <- summary(emmeans(mmrm1, ~ CR | answerIndex, infer=TRUE))
emm1 <- data.frame(emm1$CR,emm1$answerIndex,round(emm1$emmean,2))
emm1 <- emm1[order(as.numeric(as.character(emm1[,2]))),]
emm1 <- emm1[order(as.numeric(emm1[,1])),]
print(emm1)

##

emm1 <- summary(emmeans(mmrm1, ~ BA | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[1,]
lsm1 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[2,]

M1 <- rbind(M1,round(lsm1,2))
M1 <- rbind(M1,round(lsm0,2))

E1 <- rbind(E1, round( lsm1/sigma0, 2) )
E1 <- rbind(E1, round( lsm0/sigma0, 2) )

##

emm1 <- summary(emmeans(mmrm1, ~ CR | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[1,]
lsm1 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[2,]

M1 <- rbind(M1,round(lsm1,2))
M1 <- rbind(M1,round(lsm0,2))

E1 <- rbind(E1, round( lsm1/sigma0, 2) )
E1 <- rbind(E1, round( lsm0/sigma0, 2) )

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
lsm1 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
lsm2 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
lsm3 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]

M2 <- rbind(M2,round(lsm3,2))
M2 <- rbind(M2,round(lsm1,2))
M2 <- rbind(M2,round(lsm2,2))
M2 <- rbind(M2,round(lsm0,2))

E2 <- rbind(E2, round( lsm3/sigma0, 2) )
E2 <- rbind(E2, round( lsm1/sigma0, 2) )
E2 <- rbind(E2, round( lsm2/sigma0, 2) )
E2 <- rbind(E2, round( lsm0/sigma0, 2) )

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
emm2 <- data.frame(emm2$x1,emm2$answerIndex,round(emm2$emmean,2))
emm2 <- emm2[order(as.numeric(as.character(emm2[,2]))),]
emm2 <- emm2[order(as.numeric(emm2[,1])),]
print(emm2)

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + CR + PHQ9_0 + age + sex + working + answerIndex + BA*CR + BA*answerIndex + CR*answerIndex + BA*CR*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	





#########################################################################
##

# PHQ-9, Trial 2

##

edat <- read.csv(file="editeddata06_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[edat$x1=="C1"|edat$x1=="C3"|edat$x1=="C7"|edat$x1=="C10",]

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

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

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma

##

presurv01 <- presurv[((presurv$x0=="C1")|(presurv$x0=="C3")|(presurv$x0=="C7")|(presurv$x0=="C10")),]

aov4 <- lm(PHQ9Point ~ factor(x0),data=presurv01)
sigma0 <- summary(aov4)$sigma	

##

mmrm1 <- mmrm(
	formula = PHQ9 ~ BA + PS + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + PS*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm1)	

coef1 <- summary(mmrm1)$coefficients

Est <- round(coef1[2:3,1],2)
CL <- round(coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
CU <- round(coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef1[2:3,1]/sigma,2)
CL <- round((coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
CU <- round((coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)


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

##

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

emm1 <- summary(emmeans(mmrm1, ~ PS | answerIndex, infer=TRUE))
emm1 <- data.frame(emm1$PS,emm1$answerIndex,round(emm1$emmean,2))
emm1 <- emm1[order(as.numeric(as.character(emm1[,2]))),]
emm1 <- emm1[order(as.numeric(emm1[,1])),]
print(emm1)

##

emm1 <- summary(emmeans(mmrm1, ~ PS | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[1,]
lsm1 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[2,]

M1 <- rbind(M1,round(lsm1,2))
M1 <- rbind(M1,round(lsm0,2))

E1 <- rbind(E1, round( lsm1/sigma0, 2) )
E1 <- rbind(E1, round( lsm0/sigma0, 2) )

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
emm2 <- data.frame(emm2$x1,emm2$answerIndex,round(emm2$emmean,2))
emm2 <- emm2[order(as.numeric(as.character(emm2[,2]))),]
emm2 <- emm2[order(as.numeric(emm2[,1])),]
print(emm2)

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
lsm1 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
lsm2 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
lsm3 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]

M2 <- rbind(M2,round(lsm3,2))
M2 <- rbind(M2,round(lsm1,2))
M2 <- rbind(M2,round(lsm2,2))
M2 <- rbind(M2,round(lsm0,2))

E2 <- rbind(E2, round( lsm3/sigma0, 2) )
E2 <- rbind(E2, round( lsm1/sigma0, 2) )
E2 <- rbind(E2, round( lsm2/sigma0, 2) )
E2 <- rbind(E2, round( lsm0/sigma0, 2) )

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + PS + PHQ9_0 + age + sex + working + answerIndex + BA*PS + BA*answerIndex + PS*answerIndex + BA*PS*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	





#########################################################################
##

# PHQ-9, Trial 3

##

edat <- read.csv(file="editeddata06_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[edat$x1=="C1"|edat$x1=="C4"|edat$x1=="C8"|edat$x1=="C10",]

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

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

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma

##

presurv01 <- presurv[((presurv$x0=="C1")|(presurv$x0=="C4")|(presurv$x0=="C8")|(presurv$x0=="C10")),]

aov4 <- lm(PHQ9Point ~ factor(x0),data=presurv01)
sigma0 <- summary(aov4)$sigma	

##

mmrm1 <- mmrm(
	formula = PHQ9 ~ BA + AT + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + AT*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm1)	

coef1 <- summary(mmrm1)$coefficients

Est <- round(coef1[2:3,1],2)
CL <- round(coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
CU <- round(coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef1[2:3,1]/sigma,2)
CL <- round((coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
CU <- round((coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)


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

##

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

emm1 <- summary(emmeans(mmrm1, ~ AT | answerIndex, infer=TRUE))
emm1 <- data.frame(emm1$AT,emm1$answerIndex,round(emm1$emmean,2))
emm1 <- emm1[order(as.numeric(as.character(emm1[,2]))),]
emm1 <- emm1[order(as.numeric(emm1[,1])),]
print(emm1)

##

emm1 <- summary(emmeans(mmrm1, ~ AT | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[1,]
lsm1 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[2,]

M1 <- rbind(M1,round(lsm1,2))
M1 <- rbind(M1,round(lsm0,2))

E1 <- rbind(E1, round( lsm1/sigma0, 2) )
E1 <- rbind(E1, round( lsm0/sigma0, 2) )

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
emm2 <- data.frame(emm2$x1,emm2$answerIndex,round(emm2$emmean,2))
emm2 <- emm2[order(as.numeric(as.character(emm2[,2]))),]
emm2 <- emm2[order(as.numeric(emm2[,1])),]
print(emm2)

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
lsm1 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
lsm2 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
lsm3 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]

M2 <- rbind(M2,round(lsm3,2))
M2 <- rbind(M2,round(lsm1,2))
M2 <- rbind(M2,round(lsm2,2))
M2 <- rbind(M2,round(lsm0,2))

E2 <- rbind(E2, round( lsm3/sigma0, 2) )
E2 <- rbind(E2, round( lsm1/sigma0, 2) )
E2 <- rbind(E2, round( lsm2/sigma0, 2) )
E2 <- rbind(E2, round( lsm0/sigma0, 2) )

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + AT + PHQ9_0 + age + sex + working + answerIndex + BA*AT + BA*answerIndex + AT*answerIndex + BA*AT*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)		





#########################################################################
##

# PHQ-9, Trial 4

##

edat <- read.csv(file="editeddata06_R1.csv")
load(file="presurv_edited_R1.RData")

##

edat <- edat[edat$x1=="C1"|edat$x1=="C5"|edat$x1=="C9"|edat$x1=="C10",]

##

edat <- edat[is.na(edat$PHQ9)==FALSE,]

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

aov6 <- lm(PHQ9Point ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

presurv01 <- presurv[((presurv$x0=="C1")|(presurv$x0=="C5")|(presurv$x0=="C9")|(presurv$x0=="C10")),]

aov4 <- lm(PHQ9Point ~ factor(x0),data=presurv01)
sigma0 <- summary(aov4)$sigma	

##

mmrm1 <- mmrm(
	formula = PHQ9 ~ BA + BI + PHQ9_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm1)	

coef1 <- summary(mmrm1)$coefficients

Est <- round(coef1[2:3,1],2)
CL <- round(coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
CU <- round(coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2],2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)

Est <- round(coef1[2:3,1]/sigma,2)
CL <- round((coef1[2:3,1] - qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
CU <- round((coef1[2:3,1] + qt(0.975,df=coef1[2:3,3])*coef1[2:3,2])/sigma,2)
P <- round(coef1[2:3,5],3)

data.frame(Est,CL,CU,P)


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

##

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

emm1 <- summary(emmeans(mmrm1, ~ BI | answerIndex, infer=TRUE))
emm1 <- data.frame(emm1$BI,emm1$answerIndex,round(emm1$emmean,2))
emm1 <- emm1[order(as.numeric(as.character(emm1[,2]))),]
emm1 <- emm1[order(as.numeric(emm1[,1])),]
print(emm1)

emm1 <- summary(emmeans(mmrm1, ~ BI | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[1,]
lsm1 <- data.frame(emm1$emmean,emm1$lower.CL,emm1$upper.CL)[2,]

M1 <- rbind(M1,round(lsm1,2))
M1 <- rbind(M1,round(lsm0,2))

E1 <- rbind(E1, round( lsm1/sigma0, 2) )
E1 <- rbind(E1, round( lsm0/sigma0, 2) )

##

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
emm2 <- data.frame(emm2$x1,emm2$answerIndex,round(emm2$emmean,2))
emm2 <- emm2[order(as.numeric(as.character(emm2[,2]))),]
emm2 <- emm2[order(as.numeric(emm2[,1])),]
print(emm2)

##

emm2 <- summary(emmeans(mmrm2, ~ x1 | answerIndex, infer=TRUE))
lsm0 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[1,]
lsm1 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[2,]
lsm2 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[3,]
lsm3 <- data.frame(emm2$emmean,emm2$lower.CL,emm2$upper.CL)[4,]

M2 <- rbind(M2,round(lsm3,2))
M2 <- rbind(M2,round(lsm1,2))
M2 <- rbind(M2,round(lsm2,2))
M2 <- rbind(M2,round(lsm0,2))

E2 <- rbind(E2, round( lsm3/sigma0, 2) )
E2 <- rbind(E2, round( lsm1/sigma0, 2) )
E2 <- rbind(E2, round( lsm2/sigma0, 2) )
E2 <- rbind(E2, round( lsm0/sigma0, 2) )

##

mmrm3 <- mmrm(
	formula = PHQ9 ~ BA + BI + PHQ9_0 + age + sex + working + answerIndex + BA*BI + BA*answerIndex + BI*answerIndex + BA*BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)		

