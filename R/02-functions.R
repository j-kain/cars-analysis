


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