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

set.seed(0)

source('../Chapter3/load_spam_data.R')

PD         = load_spam_data(trainingScale=FALSE,responseScale=FALSE) # read in unscaled data
p          = dim(PD[[1]])[2]-1 # the last column is the response 1=>Spam; 0=>Ham
XTraining  = PD[[1]]; colnames(XTraining)[p+1] = "Y"
XTesting   = PD[[2]]; colnames(XTesting)[p+1] = "Y"
spam_words = PD[[3]]

# Generate the formula used to fit our model with: 
#
terms = paste( colnames(XTraining)[1:p], collapse="+" ) # dont consider the last column (the response variable)
formula = formula( paste( colnames(XTraining)[p+1], " ~ ", terms ) )

n_trees = 2500

#
# Fit a gradient BOOSTING MODEL to the spam data:
#
if( FALSE ){ 
  library(gbm)

  K = 5 # 5 number of interactions
  cv_folds = 10 # 10 
  m = gbm( formula, data=XTraining, distribution='bernoulli', n.trees=n_trees, shrinkage=0.05, interaction.depth=K, verbose=FALSE, cv.folds=cv_folds )
 
  #best.iter = gbm.perf(m,method="cv")
  
  # Compute the training error as a function of number of trees: 
  #
  gbm_training_error = matrix( 0, nrow=n_trees, ncol=1 )
  for( nti in seq(1,n_trees) ){
    Fhat = predict( m, XTraining[,1:p], n.trees=nti )
    pcc = mean( ( ( Fhat <= 0 ) & ( XTraining[,p+1] == 0 ) ) | ( ( Fhat > 0 ) & ( XTraining[,p+1] == 1 ) ) )
    gbm_training_error[nti] = 1 - pcc
  }

  # Compute the testing error as a function of the number of trees:
  #
  gbm_test_error = matrix( 0, nrow=n_trees, ncol=1 )
  for( nti in seq(1,n_trees) ){
    Fhat = predict( m, XTesting[,1:p], n.trees=nti )
    pcc = mean( ( ( Fhat <= 0 ) & ( XTesting[,p+1] == 0 ) ) | ( ( Fhat > 0 ) & ( XTesting[,p+1] == 1 ) ) )
    gbm_test_error[nti] = 1 - pcc 
  }
  save(gbm_training_error, gbm_test_error, file="chap_15_gbm_spam.RData")
}else{
  load("chap_15_gbm_spam.RData") 
}

#
# Fit a RANDOM FOREST MODEL to the spam data:
#
if( TRUE ){ 
    library(randomForest)

    XTraining[,58] = factor(XTraining[,58])
    XTesting[,58] = factor(XTesting[,58])

    rf_test_error = matrix( 0, nrow=n_trees, ncol=1 ) 
    for( nti in seq(1,n_trees) ){
      if( nti %% 25 == 0 ){
        print(sprintf("Building Random Forest with %5d Trees %5.2f %% done ... ", nti, nti/n_trees))
      }
      rf = randomForest( formula, data=XTraining, ntree=nti ) #mtry, nodesize
      rf_test_error[nti] = 1 - mean( predict( rf, XTesting[,1:p] ) == XTesting[,p+1] )
    }
    save(rf_test_error, file="chap_15_rf_spam.RData")
}else{
  load("chap_15_rf_spam.RData")
}

#
# Plot both results here:
# 
#postscript("../../WriteUp/Graphics/Chapter15/dup_fig_15_1.eps", onefile=FALSE, horizontal=FALSE)

plot( seq(1,n_trees), rf_test_error, ylim=c(0.04,0.07), type="l", col="red" )
lines( seq(1,n_trees), gbm_test_error, ylim=c(0.04,0.07), type="l", col="green", xlab="Number of Trees", ylab="Test Error", main="Spam Data" )
legend( 1250, 0.07, c("Random Forest", "Gradient Boosting (5 Node)"), col=c("red", "green"), lty=c(1,1) )

#dev.off()



