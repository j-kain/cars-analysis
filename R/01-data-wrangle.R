raw_data <- read_csv(here("data","cars.csv"))


data <- raw_data %>% 
    rename(msrp=SuggestedRetailPrice,
           disp=EngineSize,
           cyl=Cylinders,
           hp=Horsepower,
           mpg=HighwayMPG,
           wt=Weight,
           wb=WheelBase) %>% 
    select(msrp, everything(), -`Vehicle Name`)



loocv <- function(model, data, method="none", trans="none"){
    n <- nrow(data)
    p <- length(formula(model))
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
        y <- log(y)
        ybar <- log(ybar)
    }
    
    sse <- sum((y - yhat)^2)
    sst <- sum((y - ybar)^2)
    
    rsq <- cor(y,yhat)^2
    adjrsq <- 1 - sse/sst * (n - 1)/(n - p - 1)
    rmse <- sqrt(sse/n)
    aic <- AIC(fit)

    list(Rsq = rsq, AdjRsq = adjrsq, RMSE = rmse, AIC=aic)
}




m1 <- lm(msrp ~ hp+wt+cyl, data=data); summary(m1)
loocv(m1, data, method="none",trans="none")






model <- train(formula(m1), 
               data=data, 
               method="lm",
               trControl=trainControl(method="LOOCV"))
print(model)
loocv(m1, data, trans="log")













