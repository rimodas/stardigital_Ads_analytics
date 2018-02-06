library(Stat5303libs)
library(lme4)
star <- read.csv("/Users/wuf4/Downloads/star_digital.csv", header = TRUE)
head(star)
attach(star)
summary(star)
str(star)
# t-test for comparing means of control group and treatment group against purchase
fit1 = t.test(purchase ~ test, data = star);fit1
# fail to reject null hypothesis, true difference in means is equal to 0
# conclude that the sales are not systematically different for the control and treatment groups.


#2. Logistic regression is useful when you are predicting a binary outcome from a set of continuous predictor variables. 
# It is frequently preferred over discriminant function analysis because of its less restrictive assumptions.

# Logistic Regression
# where purchase is a binary factor and 
# imp_1~imp_6 are continuous predictors 
f.purchase <- as.factor(purchase) # convert purchase into a binary factor 
fit2 <- glm(f.purchase~imp_1+imp_2+imp_3+imp_4+imp_5+imp_6,data=star,family=binomial())
summary(fit2) # display results
# it seems like the frequency effect depend on the website. 
# Sites 2, 4 and 6 could benefit purchases from increased ads
# while increasing ads could have a negative effects on sites 1,3,6.

# we also consider overall frequency effect
# create a new variable that combines all number of impressions
star$impt = imp_1 + imp_2 + imp_3 + imp_4 + imp_5 + imp_6

#Now we consider the fact that different customers see different number of impressions
# Customers who see more impressions are more likely to purchase than those who see fewer impressions
# Create a new interaction term bwteen test and total impression 

star$imptt = star$test * star$impt
fit3 = glm(f.purchase ~ impt + imptt, data = star, family=binomial())
summary(fit3)
 
# check nonlinearity (imp_t may interact with itself)
star$impt2 = impt^2
star$impt2t = star$impt2 * test

fit4 = glm(f.purchase ~ impt + impt2 + imptt + impt2t, data = star, family=binomial())
summary(fit4)
# from the output of fit4 we can see that nonlinearity effect is not significant
# Overall impression is signficant. Sales increase as total number of ads increases. 

#3. 
# Becasue the question whether the company spends its advertising dollars in Site 6 or in Sites 1 through 5
# we need to compare ROI of sites 1 to 5 with that of site 6

star$imp15 = imp_1 + imp_2 + imp_3 + imp_4 + imp_5 # the sum of site 1-5
star$imp15t = star$imp15 * test # create interaction with two groups (control/treatment)
star$imp6t = star$imp_6 * test # create interaction with two groups (control/treatment)

fit5 = glm(f.purchase ~ imp_6 + imp15 + imp6t + imp15t, data = star, family=binomial())
summary(fit5)

# the result showsthat:
# sites 1 to 5 as whole seems are better at increasing sales than site 6. 

# Since the analysis is conducted on a choice based sample, this biases the estimation of the constant term, while keeping the other coefficients unbiased
# We need to correct this bias in order to compute marginal effects and ROIs.
# We use an offset factor = log[((1-purchaserate_population)/purchaserate_population)/((1-purchaserate_sample)/purchaserate_sample))]

# we are told that the purchase rate in the population is 0.153%, whereas the purchase rate in the sample is 50.29%

pplrate = 0.00153
purchase_rate = mean(purchase);purchase_rate

star$offsetvalue = log(((1 - pplrate)/pplrate)/((1 - purchase_rate)/purchase_rate))
fit5 = glm(f.purchase ~imp_6 + imp15 + imp6t + imp15t, family=binomial(),data = star, offset = offsetvalue)

summary(fit5)


star2 = star
star2$offsetvalue=0
#This is to ensure that predictions correspond to the population purchase rate

pred = predict(fit5,newdata=star2,type="response") #predicted purchase probabilities
star3 = star2
star3$imp15t = star3$imp15t + 0.01*mean(star3$imp15t)

#increase impressions by 1% of mean value

pred_15 = predict(fit5,newdata=star3,type="response")

#predicted purchase probabilities with increased impressions

marginal_15 = (pred_15 - pred)/(star3$imp15t - star2$imp15t)
mu15 = mean(marginal_15[star3$test==1]) #Marginal effect for sites 1-5
mu15
star3 = star2
star3$imp6t = star3$imp6t + 0.01*mean(star3$imp15t)

#increase impressions by 1% of mean value

pred_15 = predict(fit5,newdata=star3,type="response")

#predicted purchase probabilities with increased impressions

marginal_6 = (pred_15 - pred)/(star3$imp6t - star2$imp6t)
mu6 =mean(marginal_6[star3$test==1]) # Marginal effect for site 6
mu6
# ROI comparison
Roi_15=(1200*mu15-0.025)/0.025; Roi_15
Roi_6=(1200*mu6-0.02)/0.02;Roi_6
# Cost per thousand impressions are $25 and $20 for Sites 1-5 and Site 6, respoectively; 
# Cost per impression is $0.025 and $0.020
# Marginal effect of one impression on purchase = 0.000039 and 0.0000361
# Contribution per purchase is $1200
# Incremental contribution due to one impression = Marginal Effect * Contribution per purchase: 0.04680 and 0l.04325
# ROI = (Inc cost - Cost)/Cost: 87.2% for Sites 1-5 and 116.3% for Site 6

