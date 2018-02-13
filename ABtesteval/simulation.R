library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

customers <- data.frame(banner= rep(0, 834), reven = rep(0, 834)) 
customers$reven[1:22] <- rnorm(mean= 189.07, sd=30, n = 22) #create data for the control group

customersb <- data.frame(banner= rep(1, 1020), reven = rep(0,1020))
customersb$reven[1:30] <- rnorm(mean=191.88, sd=30, n=30) #create data for variant A

customers <- rbind(customers, customersb) #bind data together

model1 <- lm(data=customers, reven ~ banner) #run OLS model
summary(model1) #the coefficient for banner is insignificant
 
##visualisation of the coefficient 
plot.df <- data.frame(y= model1$coefficients["banner"], x= 0, lb= confint(model1, "banner",level=0.95)[1], ub= confint(model1, "banner",level=0.95)[2])
plot.df %>% ggplot(aes(x, y)) + geom_point() + geom_errorbar(aes(ymin= lb, ymax=ub, width=0.1)) + geom_hline(aes(yintercept = 0), linetype="dashed") + scale_x_continuous(limits = c(-1, 1)) + labs(y= "Coef value", title= "OLS coef for banner with 95% CI", x="")+ theme(axis.ticks.y= element_blank(), axis.text.y = element_blank())+coord_flip()

##distoring the data by adding 6 purchases of 250€ for the customers who were not shown the banner
customers_new_purchases <- rbind(customers, rep(c(0, 250), 6))
model2 <- lm(data=customers_new_purchases, reven ~ banner)
summary(model2)
