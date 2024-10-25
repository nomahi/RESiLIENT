library("mmrm")
library("emmeans")

#########################################################################
##

# ISI, Trial 1

##

edat <- read.csv(file="editeddata01_R1.csv")

##

edat <- edat[is.na(edat$ISIc)==FALSE,]
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

aov6 <- lm(ISI ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = ISIc ~ BA + CR + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + CR*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ x1 + ISI_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ BA + CR + BA*CR + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + CR*answerIndex + BA*CR*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	




#########################################################################
##

# ISI, Trial 2

##

edat <- read.csv(file="editeddata02_R1.csv")

##

edat <- edat[is.na(edat$ISIc)==FALSE,]
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

aov6 <- lm(ISI ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = ISIc ~ BA + PS + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + PS*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ x1 + ISI_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ BA + PS + BA*PS + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + PS*answerIndex + BA*PS*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	





#########################################################################
##

# ISI, Trial 3

##

edat <- read.csv(file="editeddata03_R1.csv")

##

edat <- edat[is.na(edat$ISIc)==FALSE,]
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

aov6 <- lm(ISI ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = ISIc ~ BA + AT + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + AT*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ x1 + ISI_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ BA + AT + BA*AT + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + AT*answerIndex + BA*AT*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)		





#########################################################################
##

# ISI, Trial 4

##

edat <- read.csv(file="editeddata04_R1.csv")

##

edat <- edat[is.na(edat$ISIc)==FALSE,]
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

aov6 <- lm(ISI ~ x1,data=edat6)
sigma <- summary(aov6)$sigma	

##

mmrm1 <- mmrm(
	formula = ISIc ~ BA + BI + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ x1 + ISI_0 + age + sex + working + answerIndex + x1*answerIndex + us(answerIndex | username),
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
	formula = ISIc ~ BA + BI + BA*BI + ISI_0 + age + sex + working + answerIndex + BA*answerIndex + BI*answerIndex + BA*BI*answerIndex + us(answerIndex | username),
	data = edat,
	control = mmrm_control(method = "Kenward-Roger")
)

summary(mmrm3)	




