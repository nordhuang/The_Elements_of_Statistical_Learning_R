#
# Duplicate Figure 3.5 (all possible subset models) from the book 
# 

X  = read.table("../../Data/prostate.data")
XTraining = subset( X, train )
XTesting  = subset( X, train==FALSE )

D = XTraining[,1:8] # get the predictor data
nrow = dim( D )[1]
p = dim( D )[2] 

# Standardize the data.
#
# Note that one can get the centering value (the mean) with the command attr(D,'scaled:center')
#                   and the scaling value (the sample standard deviation) with the command attr(D,'scaled:scale')
# 
D = scale( D )

lpsa = XTraining[,9]
Db = cbind( D, lpsa ) 
Df = data.frame( Db ) # a data frame containing all variable of interest 

# do the k=0 (no features) subset first:
#
mk = lm( formula = "lpsa ~ +1", data=Df )
rss = sum( mk$residuals^2 )
xPlot = c(0); yPlot = c(rss)

# do all remaining k>0 subsets next:
# 
for( k in 1:p ){ # k=0 needs to be done outside of this loop 
  allPosSubsetsSizeK = combn(p,k) # get all possible subsets of size k 
  numOfSubsets = dim(allPosSubsetsSizeK)[2]
  
  for( si in 1:numOfSubsets ){ 
    featIndices = allPosSubsetsSizeK[,si]
    featNames   = as.vector(names(Df))[featIndices]

    # construct a formula needed for the linear regression:
    # 
    form = "lpsa ~ "
    for ( ki in 1:k ){
      if( ki==1 ){ 
        form = paste( form, featNames[ki], sep=" " )
      }else{
        form = paste( form, featNames[ki], sep="+" )
      }
    }
    
    # fit this linear model and compute the RSS using these features:
    # 
    mk   = lm( formula = form, data=Df )
    rss = sum( mk$residuals^2 )
    xPlot = c(xPlot,k)
    yPlot = c(yPlot,rss)
  }
}

postscript("../../WriteUp/Graphics/Chapter3/fig_3_5_dup.eps", onefile=FALSE, horizontal=FALSE)

plot(xPlot,yPlot,xlab="Subset Size k",ylab="Residual Sum-of-Squares",ylim=c(0,100),xlim=c(0,8))

xMinPlot = xPlot[1]; yMinPlot = yPlot[1]
for ( ki in 1:p ){
  inds = xPlot==ki
  rmin = min(yPlot[inds])
  xMinPlot = c(xMinPlot,ki); yMinPlot = c(yMinPlot,rmin)
}
lines(xMinPlot,yMinPlot)

dev.off()



