setwd("C:/Aashish/Higher_Studies/IIM_Data_Science_2/Capstone_Project/Crime_Detection/R_Code_Analysis")
getwd()

crime_data<- read.csv("02 Oct 2020.csv", header=TRUE)

library(openxlsx)

install.packages("mice")
library(mice)

install.packages("VIM")
library(VIM)
# to check about missing values

mice_plot <- aggr(crime_data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(crime_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern")) 

wrkng_crimedta2 <- mice(crime_data,m=5, maxit = 20,method = "cart", seed = 550)
miceOutput2 <- complete(wrkng_crimedta2,5)

miceOutput2[is.na(miceOutput2$PF_Total_officers_Male),]$PF_Total_officers_Male <- miceOutput2[is.na(miceOutput2$PF_Total_officers_Male),]$PF_Total_Officers - miceOutput2[is.na(miceOutput2$PF_Total_officers_Male),]$PF_Total_officers_Female
anyNA(miceOutput2)

mice_plot <- aggr(miceOutput2, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(miceOutput2), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern")) 


msngvaluetreated2<-as.data.frame(miceOutput2)
write.csv(msngvaluetreated2,"2.Missing_Value_TRTD_FILE.csv")


####################################### Outlier Treatment ##################################################################
#Outlier treatment 1

wrngl_data<-as.data.frame(msngvaluetreated2[,-1:-2])

quantiletreatment<-function(g)
{
  UL<-3*quantile(g, 0.99, na.rm = TRUE)
  g[g>UL]<- UL
  return(g)
}
Outlier99<-as.data.frame(sapply(wrngl_data,quantiletreatment))
write.csv(Outlier99 ,"outlier99.csv")


################################################  Outlier treatment 2 ###################################


d1 <- read.csv("2.Missing_value_treated.csv", header = TRUE)

outlier_trtd<-d1[,-1:-2]
quantiletreatment<-function(a)
{
  qnt<-quantile(a,probs = c(0.25,0.75),na.rm = TRUE)
  c<- quantile(a,probs = c(.05, .095), na.rm = TRUE)
  t<-1.5*IQR(a,na.rm = TRUE)
  a[a > (qnt[2] + t)] <-c[2]
  return(a)
}
Outlier_treatment<-as.data.frame(sapply(outlier_trtd,quantiletreatment))

da<-cbind(Outlier_treatment,d1[,c(1:2)])
db<- da[,c(ncol(da),1:(ncol(da)-1))]
dc<- db[,c(ncol(db),1:(ncol(db)-1))]

library(openxlsx)
write.xlsx(dc ,"3.outliers_rmvd.csv")



#Value Treatment ##
Workingdata_log<-d1[,c(-1:-2)]

logtreatment<-function(p)
{
  logtrtd<-log(1+p)
  p=logtrtd
  return(p)}

xlg<-sapply(Workingdata_log,logtreatment)

dd<-cbind(Outlier_treatment,d1[,c(1:2)])
de<- dd[,c(ncol(dd),1:(ncol(dd)-1))]
df<- de[,c(ncol(de),1:(ncol(de)-1))]

write.xlsx(df ,"4.logtrtd.csv")


###################################### Inference ##################################################
#There is no need of outlier treatment  and log treatment since R^2 going down, better to go with missing value file #









library(dplyr)
d3<- read.csv("2.Missing_value_treated.csv", header = TRUE)
Regression_subcat<-subset(d3,select=-c(Crime_Murder_Slaughter,Crime_Forcible_Rape,Crime_Robbery
                                  ,Crime_Aggrevated_Assault,Crime_Property,
                                  Crime_Burglary,Crime_Larcency,Crime_Motor_Vehicle_Theft,
                               Eco_Total_Civilian_labourforce,Arms_Total_Weapons,
                               PF_Total_law_enforcement_employees,
                                 PF_Total_Officers,PF_Total_civilians,
                                 PF_estimated_population))
write.csv(Regression_subcat ,"Regression_subcat.csv")

#Regression_CATGRY<-subset(d3,select=c(Crime_Total_Offense,
                                      # Eco_Total_Civilian_labourforce,Arms_Total_Weapons,
                                       #PF_Total_law_enforcement_employees,
                                       #PF_Total_Officers,PF_Total_civilians,
                                       #PF_estimated_population))

library(foreign)
library(car)
library(gplots)

#Below plot shows fixed effect for states
plotmeans(d3$Crime_Total_Offense~d3$State, main="Heterogeineity across states", data=Regression_subcat)

#Below plot shows fixed effect for years

plotmeans(d3$Crime_Total_Offense~d3$Year, main="Heterogeineity across years", data=Regression_subcat)


################################# For Total Crime ######################################################################################
install.packages("plm")
library(plm)

d5<-data.frame(Regression_subcat[,-1:-2])

# Finding Highly Correlated Variable
round(cor(d5),2)
cor_val<-round(cor(d5),2)

write.csv(cor_val ,"High_Correlated_Variable.csv")
## Now we run correlation and check which all independent variable have correlation above 0.5 with total crime

# After identifying 25 variables which have high correlation we will weed out those variables which have impact on each other, to discard multicollinearity



library(faraway)
car::vif(random_effect) # Vif value to be less than 5 or 10 - Checks multicollinearity

mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Civilian_non_institutional_population +d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastYear+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Tobacco+d5$Cigarette+d5$Serious_Mental_Illness
            +d5$Any_Mental_Illness+d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

mymodel

summary(mymodel)

fit <-lm(mymodel)

#the linearly dependent variables

ld.vars <- attributes(alias(fit)$Complete)$dimnames[[1]]

g1<-vif(mymodel)
g1
max(g1)
which.max(g1)
which(g1<10.5)

mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastYear+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Tobacco+d5$Cigarette+d5$Serious_Mental_Illness
            +d5$Any_Mental_Illness+d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)
vif(mymodel)
g2<-vif(mymodel)
max(g2)
which.max(g2)
which(g2<10.5)

mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastYear+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette+d5$Serious_Mental_Illness
            +d5$Any_Mental_Illness+d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)
vif(mymodel)
g3<-vif(mymodel)
max(g3)
which.max(g3)
which(g3<10.5)


mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette+d5$Serious_Mental_Illness
            +d5$Any_Mental_Illness+d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g4<-vif(mymodel)
max(g4)
which.max(g4)
which(g4<10.5)

mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette+d5$Serious_Mental_Illness
            +d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g5<-vif(mymodel)
max(g5)
which.max(g5)
which(g5<10.5)

mymodel<-lm(d5$Crime_Total_Offense~d5$Eco_Total_Employment_Civilian_labourforce
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette
            +d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g6<-vif(mymodel)
max(g6)
which.max(g6)
which(g6<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
            +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette
            +d5$Serious_Suicide_Thoughts+d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g7<-vif(mymodel)
max(g7)
which.max(g7)
which(g7<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette
            +d5$Major_Depression
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g8<-vif(mymodel)
max(g8)
which.max(g8)
which(g8<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Alcohol+d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g9<-vif(mymodel)
max(g9)
which.max(g9)
which(g9<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames+d5$Arms_Revolvers
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)

vif(mymodel)
g10<-vif(mymodel)
max(g10)
which.max(g10)
which(g10<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Rifles+d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)
vif(mymodel)
g11<-vif(mymodel)
max(g11)
which.max(g11)
which(g11<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cocaine+d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)
vif(mymodel)
g12<-vif(mymodel)
max(g12)
which.max(g12)
which(g12<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Total_civilians_Female+d5$PF_Number_of_agencies,d5)
vif(mymodel)
g13<-vif(mymodel)
max(g13)
which.max(g13)
which(g13<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Eco_Total_Unemployment_Civilian_labourforce+d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Number_of_agencies,d5)
vif(mymodel)
g14<-vif(mymodel)
max(g14)
which.max(g14)
which(g14<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
             +d5$Arms_Combinations+d5$Arms_Derringers
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Number_of_agencies,d5)
vif(mymodel)
g15<-vif(mymodel)
max(g15)
which.max(g15)
which(g15<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Arms_Combinations
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cigarette
            +d5$PF_Total_officers_Male+d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Number_of_agencies,d5)
vif(mymodel)
g16<-vif(mymodel)
max(g16)
which.max(g16)
which(g16<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Arms_Combinations
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$Cigarette
       +d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Number_of_agencies,d5)
vif(mymodel)
g17<-vif(mymodel)
max(g17)
which.max(g17)
which(g17<10.5)

mymodel<-lm(d5$Crime_Total_Offense~
              +d5$Arms_Combinations
            +d5$Arms_FlareGuns+d5$Arms_Pistols+d5$Arms_Receivers_Frames
            +d5$Arms_Shotguns+d5$Marijuana_Use_PastMonth
            +d5$PF_Total_officers_Female+d5$PF_Total_civilians_Male
            +d5$PF_Number_of_agencies,d5)
vif(mymodel)
g18<-vif(mymodel)
max(g18)
which.max(g18)
which(g18<10.5)

## Now Panel Regression without multicollinearity
d5
d6<-cbind(d5,Regression_subcat[,c(1:2)])
d7<- d6[,c(ncol(d6),1:(ncol(d6)-1))]
d8<- d7[,c(ncol(d7),1:(ncol(d7)-1))]

d9<-as.data.frame(d8)

Y <- cbind(d9$Crime_Total_Offense)
X <- cbind(d9$Arms_Combinations,d9$Arms_FlareGuns,d9$Arms_Pistols,d9$Arms_Receivers_Frames,
           d9$Arms_Shotguns,d9$Marijuana_Use_PastMonth,d9$PF_Total_officers_Female,
           d9$PF_Total_civilians_Male,d9$PF_Number_of_agencies)


# Set data as panel data 
pdata <- pdata.frame(d9, index=c("State","Year")) # basicallly using index we are here declaring that  state & year are individual for which I m looking for regression and time are what different time periods are


# Descriptive statistics
summary(Y)
summary(X)

# Pooled OLS estimator - OLS
pooling <- plm(d9$Crime_Total_Offense ~ d9$Arms_Combinations + d9$Arms_FlareGuns + d9$Arms_Pistols + d9$Arms_Receivers_Frames +
                 d9$Arms_Shotguns + d9$Marijuana_Use_PastMonth + d9$PF_Total_officers_Female +
                 d9$PF_Total_civilians_Male + d9$PF_Number_of_agencies, data=pdata, model= "pooling")
summary(pooling)
#sum_pooling<-summary(pooling)

#pooling_data<-as.data.frame(sum_pooling)
#write.csv(sum_pooling ,"pooling.csv")



# Between estimator - OLS
between <- plm(d9$Crime_Total_Offense ~ d9$Arms_Combinations + d9$Arms_FlareGuns + d9$Arms_Pistols + d9$Arms_Receivers_Frames +
                 d9$Arms_Shotguns + d9$Marijuana_Use_PastMonth + d9$PF_Total_officers_Female +
                 d9$PF_Total_civilians_Male + d9$PF_Number_of_agencies, data=pdata, model= "between")
summary(between)

# First differences estimator - Panel
firstdiff <- plm(d9$Crime_Total_Offense ~ d9$Arms_Combinations + d9$Arms_FlareGuns + d9$Arms_Pistols + d9$Arms_Receivers_Frames +
                   d9$Arms_Shotguns + d9$Marijuana_Use_PastMonth + d9$PF_Total_officers_Female +
                   d9$PF_Total_civilians_Male + d9$PF_Number_of_agencies, data=pdata, model= "fd")
summary(firstdiff)

# Fixed effects or within estimator - For Panel  
fixed <- plm(d9$Crime_Total_Offense ~ d9$Arms_Combinations + d9$Arms_FlareGuns + d9$Arms_Pistols + d9$Arms_Receivers_Frames +
               d9$Arms_Shotguns + d9$Marijuana_Use_PastMonth + d9$PF_Total_officers_Female +
               d9$PF_Total_civilians_Male + d9$PF_Number_of_agencies, data=pdata, model= "within")
summary(fixed)

# Random effects estimator - For Panel
random <- plm(d9$Crime_Total_Offense ~ d9$Arms_Combinations + d9$Arms_FlareGuns + d9$Arms_Pistols + d9$Arms_Receivers_Frames +
                d9$Arms_Shotguns + d9$Marijuana_Use_PastMonth + d9$PF_Total_officers_Female +
                d9$PF_Total_civilians_Male + d9$PF_Number_of_agencies, data=pdata, model= "random")
summary(random)

# LM test for random effects versus OLS
plmtest(pooling)

# LM test for fixed effects -> To check OLS vs Fixed/
pFtest(fixed, pooling)

# Hausman test -> to check fixed vs random effects model (Null hypothesis suggest to go with random)
phtest(random, fixed)



########################################### Robbery Crime ##########################################
#Ignore

Regression_subcat1<-subset(d3,select=-c(Crime_Total_Offense,Crime_Murder_Slaughter,Crime_Forcible_Rape
                                       ,Crime_Aggrevated_Assault,Crime_Property,
                                       Crime_Burglary,Crime_Larcency,Crime_Motor_Vehicle_Theft,
                                       Eco_Total_Civilian_labourforce,Arms_Total_Weapons,
                                       PF_Total_law_enforcement_employees,
                                       PF_Total_Officers,PF_Total_civilians,
                                       PF_estimated_population))

# Finding Highly Correlated Variable
e5<-data.frame(Regression_subcat1[,-1:-2])
round(cor(e5),2)

install.packages("usdm")
library(usdm)
library(faraway)
library(carData)
library(car)
car::vif(e5, random_effect) # Vif value to be less than 5 or 10 - Checks multicollinearity

mymodeljitu<-lm(e5$Crime_Robbery~e5$Eco_Civilian_non_institutional_population+e5$Eco_Total_Employment_Civilian_labourforce
            +e5$Eco_Total_Unemployment_Civilian_labourforce+e5$Arms_AnyOtherWeapons+e5$Arms_Derringers+e5$Arms_FlareGuns
            +e5$Arms_Pistols+e5$Arms_Receivers_Frames+e5$Arms_Revolvers+e5$Arms_Rifles+e5$Arms_Shotguns
            +e5$Marijuana_Use_PastYear+e5$Marijuana_Use_PastMonth+e5$Cocaine+e5$Alcohol+e5$Tobacco+e5$Cigarette
            +e5$Serious_Mental_Illness+e5$Any_Mental_Illness+e5$Serious_Suicide_Thoughts+e5$Major_Depression
            +e5$PF_Total_officers_Male+e5$PF_Total_officers_Female+e5$PF_Total_civilians_Male+e5$PF_Total_civilians_Female)

mymodeljitu

summary(mymodeljitu)

fit <-lm(mymodeljitu)

#the linearly dependent variables

ld.vars <- attributes(alias(fit)$Complete)$dimnames[[1]]

ga<-vif(mymodeljitu)
max(ga)
which.max(ga)
which(ga<10.5)

################################################# Testing ########################################################################

Totalcrime<-d9$Arms_Combinations,d9$Arms_FlareGuns,d9$Arms_Pistols,d9$Arms_Receivers_Frames,
d9$Arms_Shotguns,d9$Marijuana_Use_PastMonth,d9$PF_Total_officers_Female,
d9$PF_Total_civilians_Male,d9$PF_Number_of_agencies


 -19906.26*armsflareguns+30.5*pistols+143.61*shotguns+202.96*marijunapastmonth-25.6*femaleofficers+50.2*civilians male  officer

-19906.26*0+30.5*646+143.61*142+202.96*237-25.6*823+50.2*758

146662-105179.9

(146662-105179.9)/146662




