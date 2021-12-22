library(readxl)
# xlsx files
my_data <- read_excel("~/surveyData/survey1.xlsx")
data.frame<-df
##tbd manipulate dataframe

library("lavaan")
# verworfene Items: rew1, rew3, rec1, rec3,sup1
model<-'
ext =~ rew2 + con2 + con1 + rec2
int =~ com1 + com2 + sup2 + sup3

' 
fit <-cfa(model, data=my_data, std.lv=TRUE)
summary(fit, fit.measures=TRUE)
summary(fit, standardized=TRUE)
#SRMR = 0.099#RMSEA = 0.081 # TLI=0.936 #CFI=0.956 # chi2=0.132
library(psych)

### cronbach validity internal consistency
alpha(subset(my_data, select=c(rew2,con2,con1,rec2)))#0.61 acceptable 
alpha(subset(my_data, select=c(com1,com2,sup2,sup3)))# 0.87 very good


#covariance matrix
#lavInspect(fit, "cov.lv")
#sandardized loadings
inspect(fit, what="std")

#r square
inspect(fit,'r2')


#Sem path Diagramm
##install.packages("semPlot")
library(semPlot)


semPaths(fit,
         what = "std", # this argument controls what the color of edges represent. In this case, standardized parameters
         whatLabels = "est", # This argument controls what the edge labels represent. In this case, parameter estimates
         style = "lisrel", # residuals as arrows
         residScale = 8, # This makes the residuals larger
         theme = "colorblind", # qgraph colorblind friendly theme
         nCharNodes = 0, # Setting 0 disables abbreviation of nodes.
         reorder = FALSE, # This disables the default reordering
         rotation = 2, # Rotates the plot
         layout = "tree2", # tree layout options are "tree", "tree2", and "tree3"
         cardinal = "lat cov", # This makes the latent covariances connet at a cardinal center point
         curvePivot = TRUE, # Changes curve into rounded straight lines
         sizeMan = 4, # Size of manifest variables
         sizeLat = 10, # Size of latent variables
         mar = c(2,5,2,5.5), # Figure margins
)


