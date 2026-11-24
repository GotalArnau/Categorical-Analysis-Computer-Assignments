tab1<- as.table(rbind(c(309, 191), c(319, 281)))
dimnames(tab1) <- list(gender = c("women", "men"),opinion = c("favor","against"))
addmargins(tab1)
addmargins(prop.table(tab1,1),2)

###Question 1####
prob_pf <- 309/500; prob_pf
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
o22 <- (281 - e22)^2 / e22

chisquared <- o11 + o12 + o21 + o22; chisquared



#chisq.test(tab1, correct=FALSE)
#install.packages('MASS')
#library(MASS)
#loglm(~gender+opinion, tab1)
