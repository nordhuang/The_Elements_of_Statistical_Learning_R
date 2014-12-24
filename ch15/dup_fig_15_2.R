# 
# Written by:
# -- 
# John L. Weatherwax                2007-07-05
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
# 
#-----

library(gbm)
library(randomForest)
library(DAAG)

set.seed(0)

source('../Chapter10/gen_eq_10_2_data.R') # code to generate the nested spheres data

n_sims = 50 # the number of simulations to run
p = 10 # the dimension the nested sphere is in

# initialize storage to hold the test error according to each run: 
gbm_1_test_error = matrix( 0, nrow=n_sims, ncol=1 )
gbm_6_test_error = matrix( 0, nrow=n_sims, ncol=1 )
rf_1_test_error = matrix( 0, nrow=n_sims, ncol=1 )
rf_3_test_error = matrix( 0, nrow=n_sims, ncol=1 )

for( nsi in seq(1,n_sims) ){
  if( nsi %% 5 == 0 ){ print(sprintf("Performing Simulation Number %5d; %5.2f %% done ... ", nsi, nsi/n_sims)) }

  # Get data for this simulation:
  # 
  XTraining = gen_eq_10_2_data(N=2000,p=p); XTraining[ XTraining$Y==-1, p+1 ] = 0 # Map the response "-1" to the value of "0" (required format for the call to gbm): 
  XTesting = gen_eq_10_2_data(N=10000,p=p); XTesting[ XTesting$Y==-1, p+1 ] = 0

  # Fit the gradient boosting models:
  # 
  gb_m = gbm( Y ~ ., data=XTraining, distribution='bernoulli', n.trees=2000, shrinkage=0.05, interaction.depth=1, verbose=FALSE )
  Yhat = predict( gb_m, XTesting[,1:p], n.trees=2000 )
  pcc = mean( ( ( Yhat <= 0 ) & ( XTesting[,p+1] == 0 ) ) | ( ( Yhat > 0 ) & ( XTesting[,p+1] == 1 ) ) )
  gbm_1_test_error[nsi] = 1 - pcc
  
  gb_m = gbm( Y ~ ., data=XTraining, distribution='bernoulli', n.trees=2000, shrinkage=0.05, interaction.depth=6, verbose=FALSE )
  Yhat = predict( gb_m, XTesting[,1:p], n.trees=2000 )
  pcc = mean( ( ( Yhat <= 0 ) & ( XTesting[,p+1] == 0 ) ) | ( ( Yhat > 0 ) & ( XTesting[,p+1] == 1 ) ) )
  gbm_6_test_error[nsi] = 1 - pcc 

  # Fit the random forest models:
  #
  XTraining[,p+1] = factor(XTraining[,p+1])
  XTesting[,p+1] = factor(XTesting[,p+1])
  rf_m = randomForest( Y ~ ., data=XTraining, ntree=500, mtry=1 ) # nodesize?
  rf_1_test_error[nsi] = 1 - mean( predict( rf_m, XTesting[,1:p] ) == XTesting[,p+1] )

  rf_m = randomForest( Y ~ ., data=XTraining, ntree=500, mtry=3 ) # nodesize?
  rf_3_test_error[nsi] = 1 - mean( predict( rf_m, XTesting[,1:p] ) == XTesting[,p+1] )
  
}

# Group everything in a data frame for plotting:
# 
DF = rbind( cbind( rf_1_test_error, rep( "RF-1", n_sims ) ), cbind( rf_3_test_error, rep( "RF-3", n_sims ) ), cbind( gbm_1_test_error, rep( "GBM-1", n_sims ) ), cbind( gbm_6_test_error, rep( "GBM-6", n_sims ) ) )
DF = as.data.frame(DF, stringsAsFactors=FALSE)
colnames( DF ) = c("error_rate","method")
DF$error_rate = as.double(DF$error_rate)
DF$method = as.factor( DF$method )

#postscript("../../WriteUp/Graphics/Chapter15/dup_fig_15_2.eps", onefile=FALSE, horizontal=FALSE)
bwplot( method ~ error_rate, data=DF, xlab="test error rate", ylab="learning method" )
#dev.off()



