data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(tidyverse) # for tidy format	
library(gridExtra)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabeled)


View(data_sample_1)

str(data_sample_1)

describe(data_sample_1)

describe(data_sample_1$age)
#outlier í row 93
describe(data_sample_1$STAI_trait)
#outlier í row 150
describe(data_sample_1$pain_cat)
describe(data_sample_1$IQ)
describe(data_sample_1$cortisol_serum)
describe(data_sample_1$cortisol_saliva)
#47 er með láa IQ
describe(data_sample_1$household_income)
#taka út 108 er með -3732

data_sample_1 %>%
  ggplot()+
  aes(x = STAI_trait, y = mindfulness)+
  geom_point()

#Búa til modelin
mod1 <-lm(pain ~ age + sex, data = data_sample_1)
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva,  data = data_sample_1)

#residuals vs leverage
mod2 %>%
  plot(which = 5)

#Cooks distance
mod2 %>%
  plot(which = 4)

data_sample_1 %>%
  slice(c(93, 114, 150, 109))

#taka út outliers

data_sample_1 <- data_sample_1[-c(93, 150, 109),]

mod1 <-lm(pain ~ age + sex, data = data_sample_1)
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva,  data = data_sample_1)

#residuals vs leverage
mod2 %>%
  plot(which = 5)

#Cooks distance
mod2 %>%
  plot(which = 4)

anova(mod1, mod2)
#Model 1: pain ~ age + sex
#Model 2: pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + 
#  cortisol_saliva
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1    154 357.94                                 
#149 190.50  5    167.43 26.191 < 2.2e-16 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

lm.beta(mod2.2)
confint(mod2)

#P-value < 0.01

AIC(mod1)
#574.7826
AIC(mod2)
#485.7548

#Normality - okei virðist vera!

#QQ plot
mod2 %>%
  plot(which = 2)
#í röð 100 er einhver sem skráði pain 10

#histogram
# histogram	
residuals_mod2 = enframe(residuals(mod2))	
residuals_mod2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	
#Ekki sérlega skewed

# skew and kurtosis	
describe(residuals(mod2))	
#Within normal range if it is between -1 and 1
# vars   n mean   sd median trimmed  mad   min  max range skew kurtosis   se
#X1    1 154    0 1.11   0.09    0.01 1.06 -2.52 2.33  4.85 -0.1    -0.41 0.09

#Lineaality
mod2 %>% 	
  residualPlots()

#Homoscedasticty

mod2 %>% 	
  plot(which = 3)	
#Evenly distributed, not a error here
#Deviation is not dependent on the predictor values of the fitted predicted data

mod2 %>% 	
  ncvTest() # NCV test
#If the P value is smaller that 0.5 then it indicades signifant heteroscesity
#Chisquare = 2.36531, Df = 1, p = 0.12406

mod2 %>% 	
  bptest() # Breush-Pagan test
#BP = 5.5358, df = 7, p-value = 0.5949


#No multicollinearity

#Variance inflation factur VIF
#Up to 10 are exeptable but in home assignment up to 3 is a threshold
mod2 %>% 	
  vif()	


#If we have high it is less realiable - 

mod2.2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum,  data = data_sample_1)

#residuals vs leverage
mod2.2 %>%
  plot(which = 5)

#Cooks distance
mod2.2 %>%
  plot(which = 4)

anova(mod1, mod2.2)

summary(mod1)

#P-value < 0.01

AIC(mod1)
#577.1297
AIC(mod2.2)
#488.9899

#Normality - okei virðist vera!

#QQ plot
mod2 %>%
  plot(which = 2)
#í röð 100 er einhver sem skráði pain 10

#histogram
# histogram	
residuals_mod2 = enframe(residuals(mod2.2))	
residuals_mod2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	
#Ekki sérlega skewed

# skew and kurtosis	
describe(residuals(mod2.2))	
#Within normal range if it is between -1 and 1
#Skew er -0.11 og kurtosis er -0.43 svo það ætti að vera okei

#Lineaality
mod2.2 %>% 	
  residualPlots()

#Homoscedasticty

mod2.2 %>% 	
  plot(which = 3)	
#Evenly distributed, not a error here
#Deviation is not dependent on the predictor values of the fitted predicted data

mod2.2 %>% 	
  ncvTest() # NCV test
#If the P value is smaller that 0.5 then it indicades signifant heteroscesity

mod2.2 %>% 	
  bptest() # Breush-Pagan test

#No multicollinearity

#Variance inflation factur VIF
#Up to 10 are exeptable but in home assignment up to 3 is a threshold
mod2.2 %>% 	
  vif()	


mod2.2 

summary(mod2.2)

mod2.2

AIC(mod1)
#582.9313
AIC(mod2.2)
#494.78

summary(mod1)$adj.r.squared
summary(mod2.2)$adj.r.squared


anova(mod1, mod2.2)
#significant with p-value <.001

summary(mod1)

predict(mod2.2)

#confidence interval
confint(mod2.2)
  
lm.beta(mod2.2)

#                   b     95%CI lb  95%CI ub  Std.Beta  p-value
# intercept       0.66   -2.42     3.76           0       .671
# age            -0.02   -0.06     0.03       -0.05       .465
# sex             0.25   -0.13     0.63        0.08       .2
# STAI_trait     -0.04   -0.09     0.02       -0.12       .195
# pain_cat        0.13   -0.08     0.18        0.41      <.0
# mindfulness    -0.25   -0.48    -0.01       -0.14      <.01
# cortisol_serum  0.23   -0.21     0.68        0.15       .306
# cortisol_saliva 0.40   -0.08     0.88        0.26       .101



summary(mod2.2)

tab_model(mod2.2, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

tab_model(mod1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

