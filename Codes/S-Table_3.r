library("mmrm")
library("emmeans")

#########################################################################
##

# SWEMWBS, Trial 1

##

edat <- read.csv(file="editeddata01_R1.csv")

##

edat <- edat[is.na(edat$SWEMWBSc)==FALSE,]

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

aov6 <- lm(SWEMWBS ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = SWEMWBSc ~ BA + CR + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + CR*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ x1 + SWEMWBS_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ BA + CR + BA*CR + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + CR*answerIndex + BA*CR*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	




#########################################################################
##

# SWEMWBS, Trial 2

##

edat <- read.csv(file="editeddata02_R1.csv")

##

edat <- edat[is.na(edat$SWEMWBSc)==FALSE,]

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

aov6 <- lm(SWEMWBS ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = SWEMWBSc ~ BA + PS + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + PS*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ x1 + SWEMWBS_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ BA + PS + BA*PS + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + PS*answerIndex + BA*PS*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	



#########################################################################
##

# SWEMWBS, Trial 3

##

edat <- read.csv(file="editeddata03_R1.csv")

##

edat <- edat[is.na(edat$SWEMWBSc)==FALSE,]

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

aov6 <- lm(SWEMWBS ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = SWEMWBSc ~ BA + AT + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + AT*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ x1 + SWEMWBS_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ BA + AT + BA*AT + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + AT*answerIndex + BA*AT*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	





#########################################################################
##

# SWEMWBS, Trial 4

##

edat <- read.csv(file="editeddata04_R1.csv")

##

edat <- edat[is.na(edat$SWEMWBSc)==FALSE,]

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

aov6 <- lm(SWEMWBS ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = SWEMWBSc ~ BA + BI + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ x1 + SWEMWBS_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = SWEMWBSc ~ BA + BI + BA*BI + SWEMWBS_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + BA*BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)		


