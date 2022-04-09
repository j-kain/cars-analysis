load(here("data", "best-model-and-metrics.rda"))




x <- data.frame(hybrid=0, disp=3.5, cyl=6, hp=210, mpg=29, wt=4210, wb=114)


exp(predict(model, newdata = x))
