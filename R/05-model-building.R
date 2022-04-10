library(here)
source(here("R","00-load-library.r"))
source(here("R","02-functions.r"))
load(here("data", "data-wrangle.rda"))
load(here("data", "training.rda"))

model_full <- lm(log(msrp)~. +
             I(disp^2) +
             I(cyl^2) +
             I(hp^2) +
             I(mpg^2) +
             I(wt^2) +
             I(wb^2), data=training)


# BSS <- ols_step_best_subset(model_full)


# data.frame(n=BSS$n, predictors=BSS$predictors, rsq=BSS$rsquare, adrsq=BSS$adjr, cp=BSS$cp, aic=BSS$aic)

model_7 <- lm(log(msrp)~
                  disp +
                  hp + 
                  mpg +
                  wt +
                  I(disp^2) +
                  I(hp^2) +
                  I(mpg^2), data=training)

model_8 <- lm(log(msrp)~
                   disp +
                   hp + 
                   mpg +
                   wt +
                   wb +
                   I(mpg^2) +
                   I(wt^2) +
                   I(wb^2), data=training)

model_9 <- lm(log(msrp)~
                   disp +
                   hp + 
                   mpg +
                   wt +
                   wb +
                   I(disp^2) +
                   I(mpg^2) +
                   I(wt^2) +
                   I(wb^2), data=training)

model_10 <- lm(log(msrp)~
                   hybrid +
                   disp +
                   hp + 
                   mpg +
                   wt +
                   wb +
                   I(disp^2) +
                   I(mpg^2) +
                   I(wt^2) +
                   I(wb^2), data=training)


model_11 <- lm(log(msrp)~. +
                 I(disp^2) +
                 I(mpg^2) +
                 I(wt^2) +
                 I(wb^2), data=training)


model_12 <- lm(log(msrp)~. +
             I(disp^2) +
             I(hp^2) +
             I(mpg^2) +
             I(wt^2) +
             I(wb^2), data=training)


anova(model_12, model_9)


rmse_9 <- model.metrics(model_9, training, method="loocv", trans="log")$RMSE
rmse_10 <- model.metrics(model_10, training, method="loocv", trans="log")$RMSE

rbind(rmse_9, rmse_10)

# model <- train(formula(model_9), 
#                data=training, 
#                method="lm",
#                trControl=trainControl(method="LOOCV"))
# 
# print(model)
# 
# 
# model.metrics(model_9, training, method="loocv", trans="log")


save(model_9, file=here("data", "best-model.rda"))






