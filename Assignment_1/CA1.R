tab1<- as.table(rbind(c(309, 191), c(319, 281)))
dimnames(tab1) <- list(gender = c("women", "men"),opinion = c("favor","against"))
addmargins(tab1)
addmargins(prop.table(tab1,1),2)

###Question 1####
(prob_pf <- 309/500)
prob_nf <- 191/500; prob_nf
prob_pm <- 319/600; prob_pm
prob_nm <- 281/600; prob_nm

####Question 2####

#chi-squared 
#1. Calculate the expected frequencies
e11 <- 628*500/1100
e12 <- 472*500/1100
e21 <- 600*628/1100
e22 <- 472*600/1100

#2. Sum of (observed - expected)^2 / expected
o11 <- (309 - e11)^2/e11
o12 <- (191 - e12)^2/e12 
o21 <- (319 - e21)^2/e21
o22 <- (281 - e22)^2/e22

chisquared <- o11 + o12 + o21 + o22; chisquared

#g^2
#1. sum of observed*log(observed/expected)
l11 <- 309*log(309/e11)
l12 <- 191*log(191/e12)
l21 <- 319*log(319/e21)
l22 <- 281*log(281/e22)

G2 <- 2*(l11+l12+l21+l22); G2


#chisq.test(tab1, correct=FALSE)
#install.packages('MASS')
#library(MASS)
#loglm(~gender+opinion, tab1)

###Question 3####
#1. Calculate odds given males and females and odd ratio
OddsM = 319/281
OddsF = 309/191
(OR=OddsF/OddsM)

#2. Estimate Standard Error
SE <- sqrt((1/309)+(1/191)+(1/319)+(1/281))

#3. Take the logarithm of OR
LOR <- log(OR)

#3. Confidence Intervals 
(LB <- exp(LOR - 1.96*SE))
(UB <- exp(LOR + 1.96*SE))


install.packages('epitools')
library(epitools)
oddsratio(tab1, method='wald', rev='neither')
riskratio(tab1, rev='both')
oddsratio(tab1, method='wald', rev='row')
oddsratio(tab1, method='wald', rev='col')
oddsratio(tab1, method='wald', rev='both')

#####Question 4####
#1. Calculate Risk Ratio
piF <- 309/500
piM <- 319/600
(RR <- piF/piM)

#2. Estimate Standard Error 
SE2 <- sqrt(((1-piF)/309)+((1-piM)/319))

#3. Take the logarithm of RR
LRR <- log(RR)

#4. Confidence Intervals

(UB2 <- exp(LRR + 1.96*SE2))
(LB2 <- exp(LRR - 1.96*SE2))

###Question 2.1####
tab2<- as.table(rbind(c(1198, 1493), c(557, 1278)))
dimnames(tab2) <- list(gender = c("Men", "Women"),admission = c("Admitted","Rejected"))
addmargins(tab2)
addmargins(prop.table(tab2,1),2)
# 1.Chi-squared test 
chisq.test(tab2, correct = FALSE)

# 2. Likelihood Ratio test (G^2)
install.packages('MASS')
library(MASS)
loglm(~ gender + admission, data = tab2)

# 3. Odds Ratio (Men vs. Women for Admission)
library(epitools)
oddsratio(tab2, method = "wald", rev = "neither")

# 4. Risk Ratio (Men vs. Women for Admission)
riskratio(tab2, rev = "neither")

#####Question 2.2####
tab_10 <- round(tab2 / 10, 0)
chisq.test(tab_10, correct = FALSE)
library(MASS); loglm(~ gender + admission, data = tab_10)
library(epitools)
oddsratio(tab_10, method = "wald", rev = "neither")
riskratio(tab_10, rev = "neither")

tab_100 <- round(tab2 / 100, 0)
chisq.test(tab_100, correct = FALSE)
loglm(~ gender + admission, data = tab_100)
oddsratio(tab_100, method = "wald", rev = "neither")
riskratio(tab_100, rev = "neither")

#####Question 2.3####
tab3<- as.table(rbind(c(1010000, 1000000), c(1000000, 1000000)))
dimnames(tab3) <- list(gender = c("Men", "Women"),admission = c("Admitted","Rejected"))
addmargins(tab3)
addmargins(prop.table(tab3,1),2)
loglm(~ gender + admission, data = tab3)
oddsratio(tab3, method = "wald", rev = "neither")
riskratio(tab_100, rev = "neither")


