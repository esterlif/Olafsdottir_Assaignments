data_sample_1 <- data_sample_1[-c(93, 150, 109),]
backward_model = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income, data = data_sample_1) 

view(data_sample_1)

#gera diagnostics
summary(backward_model)
lm.beta(backward_model)
describe(backward_model)
AIC(backward_model)
confint(backward_model)

backward_model_2 = step(backward_model, direction = "backward")

backward_model_2

#residual vs leverage
backward_model %>%
  plot(which = 5)

#Cook's distance
backward_model %>%
  plot(which = 4)

describe(residuals(backward_model))	

summary(backward_model)

tab_model(backward_model_2, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#gera nýtt diagnostics

summary(backward_model_2)
lm.beta(backward_model_2)
describe(backward_model_2)
AIC(backward_model_2)
confint(backward_model_2)


anova(backward_model, backward_model_2)

AIC(backward_model)
#491.7883
AIC(backward_model_2)
#488.3757

AIC(mod2.2)
#489.9765
#descriptive difference but not significant
#use the theory one

anova(mod2.2, backward_model_2)
#no anova bc its not nested

summary(backward_model_2)
#rsquared 0.48

summary(mod2.2)
#rsquared 0.47

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

view(data_sample_2)

summary(data_sample_2)

describe(data_sample_2)
#mindfullness



predict_th = predict(mod2.2, data_sample_2)

predict_b = predict(backward_model_2, data_sample_2)

RSS_th = sum((data_sample_2[, "pain"] - predict_th)^2)
RSS_th

RSS_b = sum((data_sample_2[, "pain"] - predict_b)^2)
RSS_b

RSS_b - RSS_th





