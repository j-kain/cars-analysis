library(here)
source(here("R","00-load-library.r"))
source(here("R","02-functions.r"))
load(here("data", "best-model.rda"))
load(here("data", "testing.rda"))
load(here("data", "data-wrangle.rda"))




model <- model_9


yhat <- exp(predict(model, newdata = testing))
y <- testing$msrp
RMSE_test <- sqrt(mean((y-yhat)^2))
RMSE_test

save(model, RMSE_test, file=here("data", "best-model-and-metrics.rda"))


