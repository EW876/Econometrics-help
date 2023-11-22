#clear memory 
rm(list =ls())

#Q1
#cross sectional: data on many units for one time period 
# Panel         : data on several units over several time periods 
# time series   : data on one unit for many time periods 

#Q2 
library(wooldridge)

View(attend)

cov(attend$final, attend$termGPA)


#Q3 
reg = lm(final ~ attend + priGPA, data = attend)

reg$coefficients

reg$coefficients[1] + 21 * reg$coefficients[2] + 1.7 * reg$coefficients[3]


#Q4 
# which would you expect to be the largest?? 
  # Answer: E[Final| ACT = 33] 
  # Answer: choose biggest number for largest and smallest for smallest  


#Q5 
7 * reg$coefficients[2] + 0.2 * reg$coefficients[3]


#Q6 
# use this comand and select the "Multiple R-squared" 
summary(reg)
#BUT!!! This most likely the preferred method for getting R-squared 
summary(reg)$r.squared


#Q7 
#The mean of residuals in linear regression is always zero
# as the mean = the sum of the residuals / the number of items. The sum is zero, so 0/n will always equal zero.
#ANSWER: 0 


#Q8 
#This is a properties of OLS question like Q5 
#see property 2 
#property 2 =  sample covariance b/t X & residuals is 0 
#ANSWER: 0 


#Q9 
sum(attend$attend >= 25  | attend$final >= 30) / nrow(attend)


#Q10  
View(attend)
#the brackets tell R to look at that in terms of the prior "entry" # basically it's saying find the mean of (final grade [ of the freshmen == 1 ])
# "== 1" means TRUE /// so Freshmen == TRUE 
mean(attend$final[attend$frosh == 1])  


#Q11 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NEED HELP 
x= c(3,4,5)
xsqr = c(9,16,25)
p = c(1/3, 1/5, 7/15)
sum(xsqr * p)



#Q12 
cov(c(4.0,1.8,1.1), c(3.9,1.3,4.1))


#Q13 
-0.7/ (sqrt(2.7) * sqrt(3))


#Q14 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! need help 
View(discrim)
reg = lm(log(psoda) ~ prpblck + log(income), data = discrim)
reg$coefficients

(reg$coefficients[1] + 0.61 * reg$coefficients[2] + (23879 * reg$coefficients[3]))


#Q15 
reg$coefficients[2] * -0.47  # R will spit out a decimal. make sure to remember to give percentage value not decimal for the answer.
reg$coefficients[2] * -0.47 * 100 # this will give you the % value.


#Q16 
reg$coefficients[3] * -0.63 # R will spit out a decimal. make sure to remember to give percentage value not decimal for the answer.
reg$coefficients[3] * -0.63 * 100 # this will give you the % value. 


#Q17 
#Which of the following can lead to a failure of assumption MLR4? Select all that apply. 
# ANSWER: 
          #Omitted Variables bias 
          #Incorrectly stating the funtional form of the model 


#Q18 
#Andy runs a regression of Y on variables X1, X2, X3, X4. These variables satisfy the relationship: 
#                  8(X1) + 5(X2) + 6(X3) + 5(X4) = 0 
# Andy's regression returns an error, which assumption of MLR 4 does this violate: Enter: 1,2,3, or 4 ?? 

#ANSWER: 3
# MLR 3  = Collinearity == the 5X2 and 5X4 == 5 & 5 = a no no (I think)


#Q19 
#Suppose that we are in SLR Model. Typically we want Var(B1 hat) to be as (small) as possible. Therefore, we want the variance of the error term U to be as (small), and 
#the total sum of squares in variable X to be as (Large) as possible 


#Q20 
reg = lm(log(salary) ~LSAT + GPA, data = lawsch85)
exp(reg$coefficients[1] + (159 * reg$coefficients[2]) + (3.2 * reg$coefficients[3]))


#Q21 
100 * ((-5 * reg$coefficients[2] )+(0.2 * reg$coefficients[3]))


#Q22 
#suppose that squiggly decides to run a similar regression to the one above, but omits variable GPA (instead, using only LSAT) to estimate 
#                                         Log (salary) = B0 + B1 (LSAT)
#let B1 denote the MLR estimate for the ceteris paribus effect of LSAT on log(salary) that you estimated in the previous question

#ANSWER: B1 (Squiggly) > B1 (hat): SLR over predicts the effect of LSAT on log(salary)


#Q23 
#what this is asking is in squiggly SLR whats the difference between his B1 (b1squig) and our MLR B1 (b1hat). these should be different since squiggly has omitted a variable.  
reg = lm(log(salary) ~LSAT + GPA, data = lawsch85) # gives beta hat 
b1hat = reg$coefficients[2]

reg = lm(log(salary) ~LSAT, data = lawsch85) #gives beta squiggly 
b1squig = reg$coefficients[2]

b1squig - b1hat        # !!! make sure to run each regression and run each b1 in order to get answer. if not you'll get 0 or something else that is wrong. 


#Q24
reg = lm(log(price) ~ rooms, data = hprice2)

SSTx = (nrow(hprice2) -1 ) * var(hprice2$rooms)
SSTx


#Q25 
sum(reg$residuals**2)


#Q26 
summary(reg) #look at table and use "Residual Standard error" value 


#Q27 
summary(reg)$coefficients # std. Error of variable (rooms)


#Q28 !!!!!!!!!! NEED HELP 


#Q29 
# this one is just plug & chug algebra

# Y = B0 + B1(X1) + B2(X2) + U 
# 5 = 8 + 9(5) + 4(X2)                   # subtract 8 from both sides and multiply 9*5 to get 45  
# -3 = 45 + 4(X2)                        # subtract 45 from both sides 
# -48 = 4(X2)                            # divide both sides by 4 
# -12 = X2 


#Q30                                     # same thing as previous question but this time we're just finding U 
# Y = B0 + B1(X1) + B2(X2) + U 
# 4 = 8 + 8(7) + 7(8) + U                # subtract the B0 (8) from both sides and multiply B1(X1) &  B2(X2)  ((8(7) + 7(8))
# -4 = 56 + 56 + U                       # Add up results 
# -4 = 112 + U                           # subtract 112 from both sides 
# -116 = U 
















