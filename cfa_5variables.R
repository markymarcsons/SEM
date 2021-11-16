library(readxl)
# xlsx files
my_data <- read_excel("~/surveyData/survey1.xlsx")

data.frame<-df

##tbd manipulate dataframe

library("lavaan")
#orginal model
model<-'
reward =~ rew1 + rew2 + rew3
recognition=~ rec1 + rec2 + rec3
content =~ con1+ con2
support =~ sup1+ sup2 + sup3
community =~ com1 + com2
' 
fit <-cfa(model, data=df, std.lv=TRUE)

#covariance matrix
#something wrong here ##negative variance ##cov not postive definit
lavInspect(fit, "cov.lv")

summary(fit, fit.measures=TRUE)
summary(fit, standardized=TRUE)

#standardized loadings
inspect(fit, what="std")

#r square
inspect(fit,'r2')


#model fitted rew3, amo1, discarded 
# 1*con1 for negative variance
model<-'
rewrec =~ rew1 + rew2 + rec1 + rec2 + rec3
content =~ 1*con1 +con2
support =~ sup1+ sup2 + sup3
community =~ com1 + com2
' 


fit <-cfa(model, data=df, std.lv=TRUE)
#covariance matrix
lavInspect(fit, "cov.lv")
#looks better 

summary(fit, fit.measures=TRUE)
summary(fit, standardized=TRUE)


#standardized loadings
inspect(fit, what="std")

#r square
inspect(fit,'r2')




