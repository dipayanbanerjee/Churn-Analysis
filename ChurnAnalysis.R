# Required Libraries

library(data.table)
library(ggplot2)
library(cowplot)
library(lattice)
library(magrittr)
library(GGally)


## Choosing Important features dataset ##
tel_data <- WA_Fn_UseC_Telco_Customer_Churn
View(tel_data)

telecom_data <- subset(tel_data,select = c('gender','SeniorCitizen','Partner','Dependents',
                                   'tenure','PhoneService','MultipleLines',
                                   'InternetService','OnlineSecurity','OnlineBackup',
                                   'DeviceProtection','TechSupport','StreamingTV','StreamingMovies',
                                   'Contract','PaperlessBilling','PaymentMethod','MonthlyCharges',
                                   'TotalCharges','Churn'))

View(telecom_data)

## Data Cleaning and Preprocessing ##

table(telecom_data$gender)
telecom_data$gender[telecom_data$gender=="Female"]<-"0"
telecom_data$gender[telecom_data$gender=="Male"]<-"1"
telecom_data$gender <-factor(telecom_data$gender,levels = c('0','1'))

table(telecom_data$SeniorCitizen)
telecom_data$SeniorCitizen <- factor(telecom_data$SeniorCitizen,levels = c('0','1'))

table(telecom_data$Partner)
telecom_data$Partner[telecom_data$Partner=="No"]<-"0"
telecom_data$Partner[telecom_data$Partner=="Yes"]<-"1"
telecom_data$Partner <- factor(telecom_data$Partner,levels = c('0','1'))

table(telecom_data$Dependents)
telecom_data$Dependents[telecom_data$Dependents=="No"]<-"0"
telecom_data$Dependents[telecom_data$Dependents=="Yes"]<-"1"
telecom_data$Dependents <- factor(telecom_data$Dependents,levels =c('0','1'))

table(telecom_data$tenure)
telecom_data$tenure <- telecom_data$tenure/12
telecom_data$tenure <- telecom_data$tenure %>% round(3)

table(telecom_data$PhoneService)
telecom_data$PhoneService[telecom_data$PhoneService=="No"]<-"0"
telecom_data$PhoneService[telecom_data$PhoneService=="Yes"]<-"1"
telecom_data$PhoneService <- factor(telecom_data$PhoneService,levels = c('0','1'))

table(telecom_data$MultipleLines)
telecom_data$MultipleLines[telecom_data$MultipleLines=="No phone service"]<-"No"
telecom_data$MultipleLines[telecom_data$MultipleLines=="No"]<-"0"
telecom_data$MultipleLines[telecom_data$MultipleLines=="Yes"]<-"1"
telecom_data$MultipleLines <- factor(telecom_data$MultipleLines,levels = c('0','1'))

table(telecom_data$InternetService)
telecom_data$InternetService[telecom_data$InternetService=="DSL"]<-"Yes"
telecom_data$InternetService[telecom_data$InternetService=="Fiber optic"]<-"Yes"
telecom_data$InternetService[telecom_data$InternetService=="No"]<-"0"
telecom_data$InternetService[telecom_data$InternetService=="Yes"]<-"1"
telecom_data$InternetService<-factor(telecom_data$InternetService,levels = c('0','1'))

table(telecom_data$OnlineSecurity)
telecom_data$OnlineSecurity[telecom_data$OnlineSecurity=="No internet service"] <- "No"
telecom_data$OnlineSecurity[telecom_data$OnlineSecurity=="No"]<-"0"
telecom_data$OnlineSecurity[telecom_data$OnlineSecurity=="Yes"]<-"1"
telecom_data$OnlineSecurity<-factor(telecom_data$OnlineSecurity,levels = c('0','1'))

table(telecom_data$OnlineBackup)
telecom_data$OnlineBackup[telecom_data$OnlineBackup=="No internet service"] <- "No"
telecom_data$OnlineBackup[telecom_data$OnlineBackup=="No"]<-"0"
telecom_data$OnlineBackup[telecom_data$OnlineBackup=="Yes"]<-"1"
telecom_data$OnlineBackup<-factor(telecom_data$OnlineBackup,levels = c('0','1'))

table(telecom_data$DeviceProtection)
telecom_data$DeviceProtection[telecom_data$DeviceProtection=="No internet service"]<-"No"
telecom_data$DeviceProtection[telecom_data$DeviceProtection=="No"]<-"0"
telecom_data$DeviceProtection[telecom_data$DeviceProtection=="Yes"]<-"1"
telecom_data$DeviceProtection<-factor(telecom_data$DeviceProtection,levels = c('0','1'))

table(telecom_data$TechSupport)
telecom_data$TechSupport[telecom_data$TechSupport=="No internet service"]<-"No"
telecom_data$TechSupport[telecom_data$TechSupport=="No"]<-"0"
telecom_data$TechSupport[telecom_data$TechSupport=="Yes"]<-"1"
telecom_data$TechSupport<-factor(telecom_data$TechSupport,levels = c('0','1'))

table(telecom_data$StreamingTV)
telecom_data$StreamingTV[telecom_data$StreamingTV=="No internet service"]<-"No"
telecom_data$StreamingTV[telecom_data$StreamingTV=="No"]<-"0"
telecom_data$StreamingTV[telecom_data$StreamingTV=="Yes"]<-"1"
telecom_data$StreamingTV<-factor(telecom_data$StreamingTV,levels = c('0','1'))

table(telecom_data$StreamingMovies)
telecom_data$StreamingMovies[telecom_data$StreamingMovies=="No internet service"]<-"No"
telecom_data$StreamingMovies[telecom_data$StreamingMovies=="No"]<-"0"
telecom_data$StreamingMovies[telecom_data$StreamingMovies=="Yes"]<-"1"
telecom_data$StreamingMovies<-factor(telecom_data$StreamingMovies,levels = c('0','1'))

table(telecom_data$Contract)
telecom_data$Contract[telecom_data$Contract=="Month-to-month"]<-"1"
telecom_data$Contract[telecom_data$Contract=="One year"]<-"2"
telecom_data$Contract[telecom_data$Contract=="Two year"]<-"3"
telecom_data$Contract<-factor(telecom_data$Contract,levels = c('1','2','3'))

table(telecom_data$PaperlessBilling)
telecom_data$PaperlessBilling[telecom_data$PaperlessBilling=="No"]<-"0"
telecom_data$PaperlessBilling[telecom_data$PaperlessBilling=="Yes"]<-"1"
telecom_data$PaperlessBilling<-factor(telecom_data$PaperlessBilling,levels = c('0','1'))

table(telecom_data$PaymentMethod)
telecom_data$PaymentMethod[telecom_data$PaymentMethod=="Bank transfer (automatic)"]<-"1"
telecom_data$PaymentMethod[telecom_data$PaymentMethod=="Credit card (automatic)"]<-"2"
telecom_data$PaymentMethod[telecom_data$PaymentMethod=="Electronic check"]<-"3"
telecom_data$PaymentMethod[telecom_data$PaymentMethod=="Mailed check"]<-"4"
telecom_data$PaymentMethod<-factor(telecom_data$PaymentMethod,levels = c('1','2','3','4'))

table(telecom_data$Churn)
telecom_data$Churn[telecom_data$Churn=="No"]<-"0"
telecom_data$Churn[telecom_data$Churn=="Yes"]<-"1"
telecom_data$Churn<-factor(telecom_data$Churn,levels = c('0','1'))


## Understanding the Dataset ##

## Univariate Analysis ##

hist_1 <- ggplot(data = telecom_data,aes(x=telecom_data$tenure))+
  geom_histogram(color="black",fill="grey",bins = 20)+
  ggtitle("Customer's tenure")+
  xlab("Tenure in Year")+
  ylab("Frequency")
hist_1

hist_2 <- ggplot(data = telecom_data,aes(x=telecom_data$MonthlyCharges))+
  geom_histogram(color="black",fill="grey",binwidth = 5)+
  ggtitle("Average Monthly Charges")+
  xlab("Charges in $")+
  ylab("Frequency")
hist_2

hist_3 <- ggplot(data = telecom_data,aes(x=telecom_data$TotalCharges))+
  geom_histogram(color="black",fill="grey",bins = 25)+
  ggtitle("Total Charges paid")+
  xlab("Charges in $")+
  ylab("Frequency")
hist_3

grid.arrange(hist_1,hist_2,hist_3,ncol=2)

bar_1 <- ggplot(data = data,aes(x=as.factor(data$gender)))+
  geom_bar(width = 0.75,fill="dodgerblue3",color="black")+facet_wrap(~data$SeniorCitizen)+
  ggtitle("Gender Proportion by Proportion of Senior Citizen")+
  xlab("0: Female | 1: Male")+
  ylab("Proportion")
bar_1

bar_2 <- ggplot(data = data,aes(x=as.factor(data$Dependents)))+
  geom_bar(width = 0.75,fill="dodgerblue3",color="black")+
  ggtitle("Customer with Dependents")+
  xlab ("0: No | 1: Yes")+
  ylab("Proportion")
bar_2

bar_3 <- ggplot(data = data,aes(x=as.factor(data$MultipleLines)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Customers with Multiplelines")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")
bar_3

bar_4 <- ggplot(data = data,aes(x=as.factor(data$InternetService)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Customers having phone service")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")
bar_4

bar_5 <- ggplot(data = data,aes(x=as.factor(data$OnlineBackup)))+
  geom_bar(width = 0.5,color="black",fill="dodgerblue3")+
  ggtitle("Cloud Backup")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")

bar_6 <- ggplot(data = data,aes(x=as.factor(data$DeviceProtection)))+
  geom_bar(width = 0.5,color="black",fill="dodgerblue3")+
  ggtitle("Enabled device protection")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")

bar_7 <- ggplot(data = data,aes(x=as.factor(data$OnlineSecurity)))+
  geom_bar(width = 0.5,color="black",fill="dodgerblue3")+
  ggtitle("Enabled Online Security")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")

grid.arrange(bar_5,bar_6,bar_7,ncol=2)

bar_8 <- ggplot(data = data,aes(x=as.factor(data$StreamingTV)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Streaming Online TV")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")

bar_9 <- ggplot(data = data,aes(x=as.factor(data$StreamingMovies)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Streaming Online Movie")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")

grid.arrange(bar_8,bar_9,ncol=2)

bar_10 <- ggplot(data = data,aes(x=as.factor(data$PaperlessBilling)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Customer opt for Paperless Billing")+
  xlab("0: No | 1: Yes")+
  ylab("Proportion")
bar_10

bar_11 <- ggplot(data = data,aes(x=as.factor(data$PaymentMethod)))+
  geom_bar(width = 0.75,color="black",fill="dodgerblue3")+
  ggtitle("Payment Methods")+
  xlab("1: Auto Bank transfer | 2: Auto debit (Credit Card) 3: Electronic Check | 4: Mailed Check")+
  ylab("Proportion")
bar_11

table(data$Churn) %>% barplot(main="Proportions of target variable",
                              xlab = "0: Current Customer | 1: Churned Customer",
                              ylab = "Proportions")

## Multivariate Analysis ##

t_bar1 <- ggplot(data = telecom_data,aes(x=as.factor(telecom_data$gender)))+
  geom_bar(width = 0.75,color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Gender by Target variable")+
  xlab("0: Female | 1: Male")+
  ylab("Proportions")
t_bar1

chur_hist <- ggplot(data = telecom_data,aes(x=telecom_data$MonthlyCharges))+
  geom_histogram(color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Montly Charges by Churn")+
  xlab("Montly Churges in $")+
  ylab("Frequency")
chur_hist

t_bar2 <- ggplot(data = telecom_data,aes(x=as.factor(telecom_data$MultipleLines)))+
  geom_bar(width = 0.75,color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Multipele Lines by Target variable")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")
t_bar2

t_bar3 <- ggplot(data = telecom_data,aes(x=as.factor(telecom_data$InternetService)))+
  geom_bar(width = 0.75,color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Internet service by Target variable")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")
t_bar3

t_bar4 <- ggplot(data = telecom_data,aes(x=as.factor(telecom_data$PhoneService)))+
  geom_bar(width = 0.75,color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Phone Service by Target variable")+
  xlab("0: No | 1: Yes")+
  ylab("Proportions")
t_bar4

t_bar5 <- ggplot(data = telecom_data,aes(x=as.factor(telecom_data$Contract)))+
  geom_bar(width = 0.75,color="black",fill="grey")+facet_wrap(~telecom_data$Churn)+
  ggtitle("Contract by Target variable")+
  xlab("1: Month-to-Month | 2: One Year | 3: Two Year")+
  ylab("Proportions")
t_bar5


## Model building ##

train.data <- sample(telecom_data,size = 700)

# 1. Model with all complete features

model.full <- glm(telecom_data$Churn ~.,family = binomial(link = logit), data = telecom_data)
summary(model.full)

Anova(model.full) # Anova test on the model


# 2. Reduced feature set based on Anova test

model.2 <- glm(telecom_data$Churn ~ telecom_data$tenure + telecom_data$PhoneService + telecom_data$OnlineSecurity +
                 telecom_data$OnlineBackup + telecom_data$TechSupport + telecom_data$Contract +
                 telecom_data$PaperlessBilling + telecom_data$PaymentMethod + telecom_data$MonthlyCharges +
                 telecom_data$TotalCharges,family = binomial(link = logit))

summary(model.2)
Anova(model.2)

varImp(model.2) # Important features from Model 2 #


# 3. Model fitted with Important features of Model 2 #

model.3 <- glm(formula = telecom_data$Churn ~ telecom_data$tenure + telecom_data$PhoneService + telecom_data$OnlineSecurity +
                 telecom_data$TechSupport + telecom_data$Contract + telecom_data$MonthlyCharges + telecom_data$TotalCharges,
               family = binomial(link = logit),data = telecom_data)

summary(model.3)
coef(model.3)
Anova(model.3)


# Model Prediction #

test.data1 <- data.frame(tenure="4.5",PhoneService="1",OnlineSecurity="0",OnlineBackup = "1",TechSupport = "0",
                         Contract="3",PaperlessBilling = "1",PaymentMethod="2",MonthlyCharges="50",TotalCharges = "200")
View(test.data1)

predict.1 <- predict(model.3,test.data1, type = "response") # Calulate pi.hat directly
predict.1







