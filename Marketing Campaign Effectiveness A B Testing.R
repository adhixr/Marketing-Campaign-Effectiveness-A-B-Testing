# Marketing Campaign Effectiveness Testing using response rate and conversion rate

setwd("C:/AR Files/USF/ARS22/SDM/Assignments")
library(readxl)
d <- read_excel("OnlineRetailPromotions.xlsx", sheet="Data")
str(d)                                    

#There are two marketing channels- Phone and Web
#Encoding correctly
# Check for missing values
colSums(is.na(d))        
#We have history, history segment is not needed
#drop non required columns 
d <- d[,-c(2)]
str(d)
# Higher value will indicate more recent purchases, needs below change
d$recency  <- 13 - d$recency              

#Factorizing required chr and binary columns to factors for analysis
d$campaign <- factor(d$campaign)
levels(d$campaign) <- list(None="No E-Mail", Men="Mens E-Mail", Women="Womens E-Mail")
levels(d$campaign)  
d$campaign <- relevel(d$campaign, "None")
d$zipcode  <- factor(d$zipcode)
d$zipcode  <- relevel(d$zipcode, "Urban")
d$channelphone <- ifelse(d$channel=="Phone" | d$channel=="Multichannel", 1, 0)
d$channelweb   <- ifelse(d$channel=="Web"   | d$channel=="Multichannel", 1, 0)
d$channel <- NULL
View(d)
str(d)
#customers who visited the website need to be in a subset for analysis
dvisit <- subset(d, visit==1)             
str(dvisit)   
#9394 website visitors 
dconvert <- subset(d, conversion==1)     
str(dconvert)    
#578 observations of purchases 
View (dconvert)

#EDA and Vizzes
hist(d$spend)
hist(log(d$spend))
#Clearly, we can see that there are a lot of excess zeroes values which need to be accounted for
#Excluding them, rest of the data seems normal. In cases like these, we need to use models
#that can handle excess zeroes, such as zero inflated models or hurdle models.

summary(d$spend)

str(d)
hist(dconvert$spend)
hist(log(dconvert$spend))
summary(dconvert$spend)

library(ggplot2)
ggplot(d, aes(x=history, y=log(spend))) +
  geom_point(color= "steelblue") +
  geom_smooth(method="loess", color="red") 

ggplot(d, aes(x=zipcode, fill=spend)) +
  geom_density(alpha = 0.6) +
  ggtitle("Spend distributions by zipcode")
geom_smooth(color="red")

ggplot(d, aes(x=campaign, fill=spend)) +
  geom_density(alpha = 0.6) +
  ggtitle("Spend distributions by campaign")
geom_smooth(color="red")

ggplot(d, aes(x=recency, fill=spend)) +
  geom_density(alpha = 0.6) +
  ggtitle("Spend distributions by recency")
geom_smooth(color="red")

ggplot(d, aes(x=channelweb, fill=spend)) +
  geom_density(alpha = 0.6) +
  ggtitle("Spend distributions by zipcode")
geom_smooth(color="red")

ggplot(d, aes(x=channelphone, fill=spend)) +
  geom_density(alpha = 0.6) +
  ggtitle("Spend distributions by zipcode")
geom_smooth(color="red")


#Check Correlations through Performance Analytics chart
str(d)
temp2 <- d[, c(2,3,4,8,9,10)]
str(temp2)
library(corrplot)
m <- cbind(temp2)
cor(m)
corrplot(cor(m), method="circle")
corrplot(cor(m), method="number")                
pairs(d) 
library(PerformanceAnalytics)
chart.Correlation(temp2)                   
str(temp2)
# No multicollinearity

#Let's try Poisson Regression GLM Model
#Round value to nearest integer

dconvert$spend <-round(dconvert$spend, 0)
summary(dconvert$spend)

m1 <- glm(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
          campaign*history + recency + zipcode + campaign*channelphone + 
          campaign*channelweb, family=poisson (link=log), data=dconvert)
summary(m1)

library(AER)
dispersiontest(m1)                       
#Dispersion value of lambda is 93, which shows that these estimates do not represent 
#reality too well, we need to account for the excess zeroes.


library(MASS) 
m2 <- glm.nb(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
          campaign*history + recency + zipcode + campaign*channelphone + 
          campaign*channelweb, data=dconvert)
summary(m2)
d$spend <- round(d$spend, 0)

# A hurdle model with or zero inflated is more ideal in this scenario
#to account for the excess zeroes in the data.

#m3 and m4 have interaction effects 

library(pscl)
m3 <- hurdle(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
          campaign*history + recency + zipcode + campaign*channelphone + 
          campaign*channelweb | visit + conversion, 
          data=d, link="logit", dist="negbin")

m4 <- zeroinfl(spend ~ campaign*mens + campaign*womens + campaign*newcustomer + 
          campaign*history + recency + zipcode + campaign*channelphone + 
          campaign*channelweb | visit + conversion, 
          data=d, link="logit", dist="negbin")

#m5 has no interactions and is used for comparison

m5 <- hurdle(spend ~ campaign + history + recency + mens + womens + zipcode +
          newcustomer + channelphone + channelweb | visit + conversion, 
          data=d, link="logit", dist="negbin")

stargazer(m3,m4,m5,type="text", single.row=TRUE)
vif(m5)              
dwtest(m5) 

#Zeroinfl model is used to draw intepretations anc recommendations