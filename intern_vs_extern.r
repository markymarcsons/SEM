library(readxl)
# xlsx files
my_data <- read_excel("~/surveyData/survey1.xlsx")

library(psych)
library(car)
#data prep
#recoding amo1 --> willingness to subscribe scale - negation in item
my_data$amo_recoded <- recode(my_data$amo1, "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
#dummy coding platform_usage
my_data$platform_dummy <- ifelse(my_data$platform_usage=="[\"I have never used a crowdfunding platform\"]",0,1)
#extern factor #rew2 + rew3 + con2 + con1 + rec2
my_data$extern <- colMeans(rbind(my_data$rew2,my_data$rew3,my_data$con2, my_data$con1,my_data$rec2 ))
#intern factor 
my_data$intern <- colMeans(rbind(my_data$com1, my_data$com2, my_data$sup1, my_data$sup2, my_data$sup3 ))
# control variables gender, income, education
##recoding gender 
my_data$gender_name <- ifelse(my_data$Gender==0,"Female","Male")
## recoding education
my_data$education_level <- ifelse(my_data$education>=3,"higher edu","lower edu")
## recoding income
my_data$income_level <- ifelse(my_data$income<=2,"below 35k USD","above 35k USD")


#Überprüfung de Zusammenhang amo_recoded und platform_dummy mit lineare regression 
#h0: AV leistet keinen erklärungbeitrag 
#hohe abhängigkeit ( zwischen recodierterer Amotivation und platformnutzung  
AV_test <- lm(platform_dummy ~ amo_recoded , data=my_data ) #p<0.05 #also man kann von platformnutzung auf willingness to subscribe schließen# r2 0.1684# relativ gering daher oultier removal
summary(AV_test)

library(ggplot2)
# plot to check for outliers
ggplot(my_data,aes(platform_dummy, amo_recoded, group=1)) +   # group=1 for grouping
  geom_boxplot()

#identfying outliers #geringer zusammenhang zwischen amo_recoded und platformnutzung# aussagekaraft der AV erhöhen
AV_test_df <- data.frame(x=my_data$platform_dummy,y= my_data$amo_recoded, ext=my_data$extern, int=my_data$intern, amo=my_data$amo1, gender=my_data$gender_name, education=my_data$education_level,income=my_data$income_level)
# create boxplot that includes outliers
p0 = ggplot(AV_test_df, aes(y = y)) + geom_boxplot(aes(x = factor(1), y))
p0
# compute lower and upper 
# plot 75 % quantile without outliers # remove outliers in R, we have to set the outlier.shape argument to be equal to NA
p1 = ggplot(AV_test_df, aes(y = y)) + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = quantile(AV_test_df$y, c(0.25, 0.75)))
p1

# boxplot vorher nachher
bx_outvsin <- boxplot(AV_test_df$y, AV_test_df_rm_out$y, ylab="platform_usage", names=c('with outlier', 'without outlier'), col=c('brown1', 'mistyrose'), main="Comparison Dependant Variable")
bx_outvsin


#AV_test_df_out_rm <- AV_test_df[!AV_test_df %in% ggplot(AV_test_df,aes(y= y))$out] #save with index
outliers <- AV_test_df$y[AV_test_df$y < quantile(AV_test_df$y, 0.25) | AV_test_df$y > quantile(AV_test_df$y, 0.75)]

length(AV_test_df) - length(outliers) # check how many columns removed

AV_test_df_rm_out <- AV_test_df[-which(AV_test_df$y %in% outliers),] #data set with removed outliers


AV_test2 <- lm(x ~ y, data=AV_test_df_rm_out) #x=platform_dummy # y=amo_recoded#improved r2 from 0.1684 to 0.2864
summary(AV_test2)
#Binär-logistische Regression mit outlier
AV <- glm(platform_dummy ~ extern + intern , data=my_data, family = binomial)
summary(AV)
#Binär-logistische Regression ohne outlier
# hypothese 3: extrinsiche faktor dominiert die intrinsische faktoren h0: 
AV_opt <- glm(x ~ ext + int , data=AV_test_df_rm_out, family = binomial)  #ß(int)= 0.2749 >ß(ext)= 0.1387 ##hypothese wiederlegt#
summary(AV_opt)


#graphik t.test vizualisieren 
#ggboxplot(AV_test_df, aes(x,y)) +
  #geom_boxplot()
library(psych)
# check if welchs or shapiro 
describe(AV_test_df,na.rm = TRUE,data= aes(ext,int)) #sd(ext)=1,15 #sd(int)=1,62 # eventuell shapiro

#WELCHS TWO SAMPLE T-TEST
#t-test
#H0:Es gibt keinen Mittelwertsunterschied zwischen den beiden Gruppen in der Grundgesamtheit.
#H1: Motivation von intrinsisch und extrinsch unterscheidet zwischen den Gruppen signifikant
#dummycoding factors  ######tbd: rm_out Datenset verwenden
AV_test_df$int_dummy <- ifelse(AV_test_df$int<=3, 0, 1)
AV_test_df$ext_dummy <- ifelse(AV_test_df$ext<=3, 0, 1)
t.model <- t.test(AV_test_df$int_dummy,AV_test_df$ext_dummy)# p=0.008013<0.05 # h1 wiederlegt # faktoren unterscheiden sich nicht signifikant im Mittelwert 
t.model
## p = 0.008013 < 0.005 h0: wiederlegt ## 
# welchs  vizualisation 
library(ggpubr)
bxp2 <- ggboxplot(t.model,aes_(x=AV_test_df_rm_out$x, y=AV_test_df_rm_out$y), 
)
bxp2


#boxplot vergleich intern extern 
bx_extvsint <- boxplot(AV_test_df_rm_out$ext, AV_test_df_rm_out$int, names=c('extern','intern'), col=c('blue3', 'brown1'), ylab="Motivation Scale", main="Comparison of Factors")
bx_extvsint


#correlationsmatrix intern vs. extern
corr_df= data.frame(extern=AV_test_df$ext, intern=AV_test_df$int, amotivation=AV_test_df$amo, platform_usage=AV_test_df$x)
library(ggcorrplot)
ggcorrplot(cor(corr_df), method= c('circle')) # interpretation: https://www.youtube.com/watch?v=dxx6azcXvRs


### 3-way anova with optimized dependendant variable#####
#H0: Zwischen den Mittelwerten der einzelnen Gruppen bestehen keine Unterschiede.
model <- lm(x ~ gender*income*education, data = AV_test_df_rm_out)
summary(model)
AOV_3w <- aov(model, data=AV_test_df_rm_out)# p value >0.05 # no significant differences between groups# H0 wiederlegt
summary(AOV_3w)


