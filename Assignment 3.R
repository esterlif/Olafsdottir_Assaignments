View(home_sample_4)

View(home_sample_3)

summary(home_sample_3)
summary(home_sample_4)

library(psych) # for describe		
library(tidyverse) # for tidy code and ggplot		
library(cAIC4) # for cAIC		
library(r2glmm) # for r2beta		
library(lme4) # for lmer	
library(lmerTest) # to get singificance test in lmer	
library(MuMIn) # for r.squaredGLMM	
library(optimx) # for optimx optimizer	

home_sample_3 = home_sample_3 %>% 	
  mutate(hospital = recode(hospital,	
                       "hospital_1" = 1,	
                       "hospital_2" = 2,	
                       "hospital_3" = 3,	
                       "hospital_4" = 4,	
                       "hospital_5" = 5,	
                       "hospital_6" = 6,	
                       "hospital_7" = 7,
                       "hospital_8" = 8,
                       "hospital_9" = 9,
                       "hospital_10" = 10,
  ))	
CI(rimodel, ci = 0.95)
rimodel <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital),  data = home_sample_3)
summary(rimodel)
summary(mod2.2)
#model coefficients:
#age: rimodel: -0.08041 , mod: -0.00138, 
#sexmale: rimodel: 0.30647, mod: 0.32611 
#STAI_trait: rimodel:  0.01387, mod: -0.02307
#pain_cat: rimodel: 0.04236, mod: 0.12561
#mindfulness: rimodel: -0.21804, mod: -0.25318  
#cortisol_serum: rimodel; 0.51400 , mod: 0.57434

tab_model(rimodel, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
confint(mod2.2)

#random slope model:
rimodel_slope = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = home_sample_3)	

cAIC(rimodel)$caic
#653.331
cAIC(rimodel_slope)$caic
#652.6149

# Marginal R squared
r2beta(rimodel, method ="nsj", data =home_sample_3)
#Effect   Rsq upper.CL lower.CL
#1          Model 0.371    0.477    0.289
#8 cortisol_serum 0.104    0.192    0.038
#2            age 0.073    0.154    0.020
#7    mindfulness 0.017    0.069    0.000
#3      sexfemlae 0.014    0.063    0.000
#4        sexmale 0.012    0.060    0.000
#6       pain_cat 0.012    0.059    0.000
#5     STAI_trait 0.002    0.032    0.000

r.squaredGLMM(rimodel)
#     R2m       R2c
#[1,] 0.3707512 0.4894758

confint(rimodel)	
#2.5 %       97.5 % 
# .sig01          0.308870251  0.961201081
#.sigma          1.054651923  1.289920428
#(Intercept)     1.793304099  7.300365051
#age            -0.116288755 -0.044688985
#sexfemlae      -0.149186464  4.668021044
#sexmale        -0.040054009  0.652694247
#STAI_trait     -0.028061863  0.056346744
#pain_cat       -0.006100741  0.090615259
#mindfulness    -0.428239795 -0.005506013
#cortisol_serum  0.325032878  0.703313316


var(home_sample_3)


summary(rimodel)$adj.r.squared


model_rnd_slp <- lmer(pain ~ pain_cat + mindfulness + cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = home_sample_3)



ggplot(model_rnd_slp, aes(y =pain, x =cortisol_reym, group =hospital))+
  geom_point(size =3)+
  geom_line(color='red', aes(y=pred_int, x=cortisol_serum))+
  facet_wrap( ~hospital, ncol =5)
