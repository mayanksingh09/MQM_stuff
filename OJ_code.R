setwd("/Users/mayank/Documents/Duke Documents/Term 5/Pricing/Class3")
library(readxl)
library(dplyr)
library(tidyr)

oj_data <- read_excel("OJData 2004.xls", sheet = "data")

#View(oj_data)
oj_data$X__1 <- NULL

oj_data$sales <- exp(oj_data$lnSALES)
for (i in colnames(oj_data[4:15])){
        oj_data[paste("price", i, sep = "_")] <- exp(oj_data[i])
}


## Ex1 a)
summary_oj <-   oj_data %>% 
                        group_by(Brand) %>% 
                        select(colnames(oj_data[28:length(colnames(oj_data))])) %>% 
                        summarise_all(funs(mean, median, sd, max, min))

summary_oj_new <- summary_oj %>% 
                        gather(summary_col, 
                               summary_val, 
                               colnames(summary_oj[2:length(colnames(summary_oj))])) %>% 
                        group_by(Brand, summary_col) %>% 
                        summarise_all(funs(mean)) %>% 
                        separate(summary_col, 
                                 into = c("Measurement", "Value"), 
                                 sep = "_(?=[^_]+$)") %>% 
                        spread(key = Value, value = summary_val) %>% 
                        separate(Measurement,
                                 into = c("Measure", "Product"),
                                 sep = "_(?=[^_]+$)")
                        
                        # group_by(Brand, Measurement) %>% 
                        # summarise_all(funs(mean))
#View(summary_oj_new)
#write.csv(summary_oj_new, file = "summary_stats.csv", row.names = F)

## Ex1 b)
library(ggplot2)

oj_data_long <- oj_data[c(1,2,4:15)] %>% 
                        gather(Price_Brand, Price, colnames(oj_data[4:15])) %>% 
                        group_by(Brand, Price_Brand, Week) %>% 
                        summarise(Price = mean(Price))



oj_data_sales <- oj_data %>% 
                        group_by(Brand, Week) %>% 
                        summarise(lnSALES = mean(lnSALES))

oj_data_long <-  oj_data_long %>% 
                        left_join(oj_data_sales, by = c("Brand", "Week")) %>% 
                        separate(Price_Brand, 
                                 into = c("n", "Brand_Name"), 
                                 sep="p")

oj_data_long$n <- NULL
oj_data_long$lnPRICE <- oj_data_long$Price

## generating plots for 12 SKUs
for (i in 1:length(unique(oj_data_long$Brand))){
                #print(i)
                print(unique(oj_data_long$Brand_Name)[i])
                #print(oj_data_long$Brand[i])
                assign(paste("plot", unique(oj_data_long$Brand_Name)[i], sep = "_"),
                       ggplot(oj_data_long[oj_data_long$Brand == i,], 
                              aes(x = lnPRICE,
                                  y = lnSALES)) +
                               geom_point() + 
                               geom_smooth() + 
                               ggtitle(label = unique(oj_data_long$Brand_Name)[i]))
}

## Creating grid plots
library(gridExtra)

first64grid <- grid.arrange(plot_CH64, plot_FG64, plot_FN64, plot_MM64, nrow = 2, ncol = 2)
second64grid <- grid.arrange(plot_SB64, plot_TF64, plot_TP64, plot_TR64, nrow = 2, ncol = 2)
othersgrid <- grid.arrange(plot_CH96, plot_MM96, plot_TP96, plot_SB128, nrow = 2, ncol = 2)

lm_eqn <- function(df){
        m <- lm(lnSALES ~ Price, df);
        eq <- substitute(italic(lnSALES) == a + b %.% italic(Price)*","~~italic(r)^2~"="~r2, 
                         list(a = format(coef(m)[1], digits = 2),
                              b = format(coef(m)[2], digits = 2),
                              r2 = format(summary(m)$r.squared, digits = 3)))
        as.character(as.expression(eq));
}
