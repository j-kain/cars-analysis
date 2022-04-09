library(here)
source(here("R","00-load-library.r"))



set.seed(1337)
train_idx <- createDataPartition(data$msrp, p = 0.8,
                                 list=FALSE,
                                 times=1)

training <- data[ train_idx,]
testing <-  data[-train_idx,]

save(training, file=here("data", "training.rda"))
save(testing, file=here("data", "testing.rda"))
