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





