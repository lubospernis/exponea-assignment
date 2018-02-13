library(magrittr)
library(cluster)
library(dplyr)

set.seed(10) #set.seed to get reproducible results
subscribe <- sample(c(0,1), replace= T, 17000, prob = c(0.4, 0.6)) %>% as.factor() #create a binary vector assigning emailees on a characteristic
nvisit <- sample(0:7, replace = T, 17000) #number of visits on the wbsite
visitsec <- rnorm(mean= 40, sd= 20, n=17000) #generate a vector of the length of stay in seconds on the website
visitsec <- ifelse(nvisit == 0, visitsec == 0, visitsec) #if a person did not make a visit, make sure he stayed 0 secs on the website
visitsec <- ifelse(visitsec <= 0 & nvisit >0, runif(1, min=10, max= 50), visitsec) #if the number of seconds of the website is below 1 and the person visited the site, draw from uniform distribution
avgspend <- ifelse(subscribe==1 | nvisit ==0, 0, sample(c(runif(min=20, max= 200, n=17000), 0), replace = T, 17000)) #if the person did not visit the webpage or is from the did not make a purchase category, assign 0
sex <- sample(c(0,1), replace= T, 17000, prob= c(0.45, 0.55)) %>% as.factor() #1 for male, 0 for female

open <- ifelse(nvisit==0 | avgspend < 10, sample(c(0,1), replace= T, 17000, prob= c(0.95, 0.05)), sample(c(0,1), replace= T, 17000, prob= c(0.80, 0.20))) #if averagespend is below 10 or the number of visits is 0, draw from one distribution; in other case draw from a different distribution
open <- ifelse(sex == 1, sample(c(0,1), replace= T, 17000, prob= c(0.83, 0.17)), open) #if male draw from alter the sample 
open <- ifelse(visitsec > 40, sample(c(0,1), replace= T, 17000, prob= c(0.84, 0.16)), open) #if long visit, alter the sample

mail1 <- data.frame(subscribe= subscribe, nvisit= nvisit, visitsec= visitsec, avgspend=avgspend, sex= sex, open= as.factor(open)) #create a dataset from the generated variables

model1 <- glm(open ~ . ,data= mail1, family= binomial(link= "logit")) 
summary(model1) ###A### there is a clear statistical difference between those who subscribe and those who do not


s_mail1 <- subset(mail1, subscribe== 1)
n_mail1 <- subset(mail1, subscribe== 0) # create two groups 
n_mail1$subscribe <- NULL 
model2 <- glm(open ~ . ,data= n_mail1, family= binomial(link= "logit")) #in this script I am going to only work with customers who have previously made a purchase

###n-subscribe clustering
diss.n_mail <- daisy(n_mail1) #create a dissimilarity object
diss.n_mail_dist <- as.matrix(diss.n_mail) 

sil_width <- c(NA)
for(i in 2:5){
pam_fit <- pam(diss.n_mail,diss = TRUE,k = i)
sil_width[i] <- pam_fit$silinfo$avg.width
}

plot(1:5, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:5, sil_width) #plot how many clusters are optimal 

pam_fit <- pam(diss.n_mail_dist, k= 5, diss=T) ###2###

###medoids- as examples of each cluster
n_mail1[pam_fit$medoids,]   ###3### here we see that we should e.g. focus on visitors who spent around 40 seconds on the website, bought stuff, but did not open emails

#create campaigns for comparision
mail_compare <- data.frame()
set.seed(Sys.time())
for(i in 1:99){
  rand_num <- runif(n=1, min=0.60, max=0.90)
  rand_num2 <- runif(n=1, min=0.82, max=0.88)
  rand_num3 <- runif(min=0.90, max=0.98, n=1)

  
  newsletter <- sample(c(0,1), replace= T, 17000, prob = c(0.4, 0.6))
  nvisit <- sample(sample(0:7, replace = T, 70000), 17000)
  visitsec <- rnorm(mean= runif(1, min=20, max=60), sd= runif(1, min=17, max=23), n=17000)
  visitsec <- ifelse(nvisit == 0, visitsec == 0, visitsec)
  visitsec <- ifelse(visitsec <= 0 & nvisit >0, runif(1, min=20, max= 60), visitsec)
  avgspend <- ifelse(newsletter==1 | nvisit ==0, 0, sample(c(runif(min=100, max= 200, n=17000), runif(min=20, max= 110, n=17000), 0), replace = T, 17000))
  sex <- sample(c(0,1), replace= T, 17000, prob= c(0.45, 0.55)) 
  
  open <- ifelse(nvisit==0 | avgspend < runif(min = 10, max= 30, n=1), sample(c(0,1), replace= T, 17000, prob= c(rand_num3, 1-rand_num3)), sample(c(0,1), replace= T, 17000, prob= c(rand_num, 1-rand_num))) 
  open <- ifelse(sex == 1, sample(c(0,1), replace= T, 17000, prob= c(rand_num2, 1-rand_num2)), open)
  open <- ifelse(visitsec > 40, sample(c(0,1), replace= T, 17000, prob= c(0.84, 0.16)), open)
  
 
  namec <- data.frame(id= as.factor(rep(i, 17000)) ,subscribe= as.factor(newsletter), nvisit= nvisit, visitsec= visitsec, avgspend=avgspend, sex= as.factor(sex), open= as.factor(open))
  mail_compare <- rbind(mail_compare, namec)
  }


###find best campaigns
identified_cluster <- data.frame(nvisit= 4, visitsec= 31.59, avgspend= 104.43, sex= as.factor(0))
mc_work <- mail_compare

prediction_clust <- c(NA)
prop_overall <- c(NA)
for (i in 1:99){
  nmc_work <- mc_work %>% filter(id==i & subscribe==0) %>% select(nvisit:open) 
  model <- glm(open ~ . ,data= nmc_work, family= binomial(link= "logit"))
  prediction_clust <- c(prediction_clust, predict.glm(model, identified_cluster, type="response"))
  prop_overall <- c(prop_overall, prop.table(table(nmc_work$open))[2])
}

comparision_df <- data.frame(id= 1:100, prediction_clust= prediction_clust, prop_overall = prop_overall)
comparision_df[1,] <- c(1, predict(model2, identified_cluster, type="response"), prop.table(table(n_mail1$open))[2])

#plot it
library(ggplot2)

###4###
ggplot(data=comparision_df, aes(0,prediction_clust)) + geom_boxplot() + geom_abline(aes(intercept= comparision_df[1,2], slope= 0, col="red"), linetype="dashed") #we see that the campaign performed poorer for the cluster than the average
ggplot(data=comparision_df, aes(0,prop_overall)) + geom_boxplot() + geom_abline(aes(intercept= comparision_df[1,3], slope= 0, col="red"), linetype="dashed") #we see that the campaign performed poorer in overall than the average



