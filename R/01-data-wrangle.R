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


save(data, file=here("data", "data-wrangle.rda"))
