library(here)
source(here("R","00-load-library.r"))
load(here("data", "data-wrangle.rda"))


scatter_predictors <-
    data %>% 
        mutate(msrp=msrp/1000) %>% 
        mutate(hybrid = replace(hybrid, hybrid==0, "no")) %>% 
        mutate(hybrid = replace(hybrid, hybrid==1, "yes")) %>% 
        gather(-msrp, -hybrid, key="var", value="value") %>% 
        
        ggplot(aes(x=value, y=msrp, color=msrp,shape=factor(hybrid))) +
            geom_point(size=2, alpha=.5) +
            facet_wrap(~ var, scales="free", ncol=2) +
            scale_color_viridis_c(option="B") +
            labs(color="msrp in $1000", shape="hybrid") +
            theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
            xlab("") +
            ylab("MSRP (in thousand USD)") +
            ylim(0,150) +
            ggtitle("MSRP vs all possible predictors")
ggsave(filename = here("output","scatter_predictors.png"), width = 10, height = 6)



plot_dt <- data %>% 
    mutate(msrp=msrp/1000) %>% 
    mutate(hybrid = replace(hybrid, hybrid==0, "no")) %>% 
    mutate(hybrid = replace(hybrid, hybrid==1, "yes"))
#------------------------------------------------------------------------------     
hp <- ggplot(plot_dt, aes(x=hp, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("Horsepower") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Horsepower")

p1 <- ggMarginal(hp, type="boxplot", fill="#613659", alpha=.7, margins='x')
#------------------------------------------------------------------------------  
cyl <- ggplot(plot_dt, aes(x=cyl, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("Cylinders") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Cylinders")

p2 <- ggMarginal(cyl, type="boxplot", fill="#613659", alpha=.7, margins='x')
#------------------------------------------------------------------------------  
wt <- ggplot(plot_dt, aes(x=wt, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("Weight (lbs)") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Weight")

p3 <- ggMarginal(wt, type="boxplot", fill="#613659", alpha=.7, margins='x')
#------------------------------------------------------------------------------  
wb <- ggplot(plot_dt, aes(x=wb, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("Wheelbase (in)") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Wheelbase")

p4 <- ggMarginal(wb, type="boxplot", fill="#613659", alpha=.7, margins='x')
#------------------------------------------------------------------------------  
disp <- ggplot(plot_dt, aes(x=disp, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("Engine Size") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Engine Size")

p5 <- ggMarginal(disp, type="boxplot", fill="#613659", alpha=.7, margins='x')
#------------------------------------------------------------------------------  
mpg <- ggplot(plot_dt, aes(x=mpg, y=msrp, color=msrp,shape=factor(hybrid))) +
    geom_point(size=4, alpha=.5) +
    scale_color_viridis_c(option="B") +
    labs(color="msrp in $1000", shape="hybrid") +
    theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
    xlab("MPG") +
    ylab("MSRP (in thousand USD)") +
    ylim(0,150) +
    ggtitle("Highway MPG")

p6 <- ggMarginal(mpg, type="boxplot", fill="#613659", alpha=.7, margins='x')


grid.arrange(p1,p2,p3,p4,p5,p6, ncol=2)
    
corr_plot <- 
    ggcorr(data, 
       legend.position="none",
       size=6,
       color=1,
       label=TRUE,
       label_round=2,
       label_size = 5,
       label_color = "white",
       method=c("complete", "spearman"),
       label_alpha = TRUE,
       low="#b56576", mid="#f8f9fa", high="#355070", 
       nbreaks = 10)
ggsave(filename = here("output","corr_plot.png"), width = 10, height = 6)


data %>% 
    ggplot(aes())


data %>% 
    select(-hybrid) %>% 
    gather(-msrp, key="var", value="value") %>% 
    
    ggplot(aes(x=var, y=value, color=msrp)) +
        geom_boxplot() +
        facet_wrap(~ var, scales="free", ncol=2) +
        scale_color_viridis_c(option="B") +
        labs(color="msrp in $1000", shape="hybrid") +
        theme_ipsum_es(axis_title_size=17, axis_title_just = "center") +
        xlab("") +
        ylab("MSRP (in thousand USD)") +
        ylim(0,150) +
        ggtitle("MSRP vs all possible predictors")





