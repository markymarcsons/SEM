
AV_test_df2 <- data.frame(x=my_data$platform_dummy,y= my_data$amo_recoded)
summary(AV_test_df2)
AV_test_df2_num <- as.numeric(unlist(AV_test_df2$y))
#create benchmark
bench <- 1*1.5*IQR(AV_test_df2_num)
#windsoring replace outliers with benchmark
AV_test_df2_num[AV_test_df2_num>bench] <- bench
#check if outliers replaced with bench
