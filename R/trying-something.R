raw_data <- read_csv(here("data","cars.csv"))


data <- raw_data %>% 
    rename(msrp=SuggestedRetailPrice,
           disp=EngineSize,
           cyl=Cylinders,
           hp=Horsepower,
           mpg=HighwayMPG,
           wt=Weight,
           wb=WheelBase) %>% 
    select(msrp, everything(), -`Vehicle Name`) %>% 
    clean_names() %>% 
    arrange(msrp)



model.metrics <- function(model, data, method="none", trans="none"){
    n <- nrow(data)
    p <- length(coef(model))-1
    y <- data %>% pull(msrp)
    ybar <- data %>% pull(msrp) %>% mean() 
    yhat <- NULL
    
    if(method == "none"){
        fit <- lm(formula(model), data=data)
        yhat <- fit$fitted.values
    }
    
    if(method == "loocv"){
        for(i in 1:n){
            train <- data[-i,]
            test <- data[i,]
            fit <- lm(formula(model), data=train)
            yhat[i] <- predict(fit, newdata=test)
        }
    }
    
    if(trans=="log"){
        yhat <- exp(yhat)
        sse <- sum((y - yhat)^2)
        sst <- sum((y - ybar)^2)
    }
    
    
    rsq <- cor(y,yhat)^2
    adjrsq <- 1 - ( ((1-rsq) * (n - 1))/(n - p - 1) )
    rmse <- sqrt(sse/n)
    aic <- AIC(fit)
    
    list(Rsq = rsq, AdjRsq = adjrsq, RMSE = rmse, AIC=aic)
}




m1 <- lm(msrp ~ hp+wt+cyl+I(cyl^2)+wb, data=data)
m2 <- lm(log(msrp)~.+
             I(disp^2) +
             I(cyl^2) +
             I(cyl^3) +
             I(hp^2) +
             I(mpg^2) +
             I(mpg^3) +
             I(wt^2) +
             I(wb^2) + 
             mpg*wt +
             mpg*wt,data=data)

m3 <- lm(log(msrp)~
             hybrid +
             disp +
             cyl +
             hp +
             mpg +
             wt +
             wb +
             I(disp^2) +
             I(hp^2) +
             I(mpg^2) +
             I(wt^2) +
             I(wb^2), data=data)

fss <- ols_step_forward_p(m2)


model.metrics(m3, data, method="loocv",trans="log")

t <- data.frame(hybrid=0, disp=5.5, cyl=12, hp=493, mpg=19, wt=4473, wb=114)
exp(predict(m2, newdata=t))


model <- train(formula(m2), 
               data=data, 
               method="lm",
               trControl=trainControl(method="LOOCV"))

1/(predict(model,newdata = t))


print(model)














