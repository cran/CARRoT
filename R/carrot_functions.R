#'Turning a non-numeric variable into a numeric one
#'
#'Function which turns a single categorical (non-numeric) variable into a numeric one (or several) by introducing dummy '0'/'1' variables.
#'
#'@param vari array of values to be transformed
#'@param outcome TRUE/FALSE indicates whether the variable \code{vari} is an outcome (TRUE) or a predictor (FALSE)
#'@param ra indices of the input array \code{vari} which indicate which values will be transformed
#'@usage make_numeric(vari, outcome, ra)
#'@return Returned value is an M x N matrix where M is the length of the input array of indices \code{ra} and N is \code{length(vari)-1}.
#'@details This function is essentially a standard way to turn categorical non-numeric variables into numeric ones in order to run a regression
#'@export make_numeric
#'@examples
#'#creating a non-numeric set
#'
#'a<-t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'a[a==1]='red'
#'a[a==2]='green'
#'a[a==3]='blue'
#'
#'#running the function
#'
#'make_numeric(a,FALSE,sample(1:100,50))
#'
#'make_numeric(a,TRUE,sample(1:100,50))





make_numeric<-function(vari,outcome,ra){
  
  
  if (outcome==TRUE){#turning non-numeric outcome into a numeric one
    
    un=unique(vari); #finding unique elements of the array of outcomes
    
    lun=length(un); #number of unique elements
    
    lunu<-array(NA,lun) #an empty array of the corresponding length
    
    #counting the number of occurrences of each outcome and writing it into the array initialised above
    
    for (i in 1:lun){
      
      lunu[i]=length(vari[vari==un[i]]);
      
    }
    
    #creating an order array corresponding to the array of counts
    
    o<-order(lunu);
    
    #creating a numeric array for output
    
    vari1<-as.numeric(as.factor(vari));
    
    #assigning values from 0 to lun-1 to the values of the array above in the descending order where 0 corresponds to the most frequent one and lun-1 to the least frequent one 
    
    for (j in 1:lun){
      
      vari1[vari==un[o[j]]]=lun-j;
      
      
    }
    
    #numeric output of the function
    
    as.numeric(vari1);
    
  }
  else{ #turning non-numeric variable into a numeric one
    
    un=unique(vari); #similar to the outcome case
    
    lun=length(un); #similar to the outcome case
    
    lunu<-array(NA,lun) #similar to the outcome case
    
    for (i in 1:lun){
      
      lunu[i]=length(vari[vari==un[i]]); #similar to the outcome case
      
    }
    
    o<-order(lunu); #similar to the outcome case
    
    vari1<-matrix(0,length(vari),lun-1) #creating a matrix of zeros to be turned into lun-1 dummy variables
    
    for (i in 1:lun-1){
      
      vari1[which(vari==un[o[i]]),i]=1; #assigning values 1 to different columns of the matrix depending on the value of the variable
      
    }
    
    #lun-1 dummy variables as an output
    
    vari1[ra,];
    
  }
  
  
}

#'Transforming the set of predictors into a numeric set
#'
#'Function which turns a set of predictors containing non-numeric variables into a fully numeric set
#'
#'@param a An M x N matrix, containing all possible subsets (N overall) of the size M of predictors' indices; therefore each column of \code{a} defines a unique subset of the predictors
#'@param ai array of indices of the array \code{a}
#'@param k index of the array \code{ai}
#'@param vari set of all predictors
#'@param ra array of sample indices of \code{vari}
#'@param l size of the sample
#'@usage make_numeric_sets(a,ai,k,vari,ra,l)
#'@return Returns a list containing two objects: \code{tr} and \code{test}\cr
#'\item{tr}{training set transformed into a numeric one}
#'\item{test}{test set transformed into a numeric one}
#'@details Function transforms the whole set of predictors into a numeric set by consecutively calling function \code{make_numeric} for each predictor
#'@seealso \code{\link{make_numeric}}
#'@export make_numeric_sets
#'@examples
#'#creating a categorical numeric variable
#'
#'a<-t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'#creating an analogous non-numeric variable
#'
#'c<-array(NA,100)
#'c[a==1]='red'
#'c[a==2]='green'
#'c[a==3]='blue'
#'
#'#creating a data-set
#'
#'b<-data.frame(matrix(c(a,rbinom(100,1,0.3),runif(100,0,1)),ncol=3))
#'
#'#making the first column of the data-set non-numeric
#'
#'b[,1]=data.frame(c)
#'
#'#running the function
#'
#'make_numeric_sets(combn(3,2),1:3,1,b,sample(1:100,60),100)



make_numeric_sets<-function(a,ai,k,vari,ra,l){
  
  #initialialising arrays of test and training sets
  
  testset1<-array(NA,0)
  trset1<-array(NA,0)
  
  #going through the indices of the corresponding set of predictors
  
  for (m in 1:length(a[,ai[k]])){
    
    #turning the non-numeric variable into numeric
    
    if (is.numeric(vari[,a[m,ai[k]]])==FALSE){ #turning a non-numeric variable into numeric
      
      #performing this operation for the training and test set
      
      anum1<-make_numeric(vari[,a[m,ai[k]]],FALSE,ra)
      anum<-make_numeric(vari[,a[m,ai[k]]],FALSE,setdiff(1:l,ra))
      
      #adding the transformed variable to the existing test and training sets on the left
      
      testset1<-cbind(testset1,anum)
      trset1<-cbind(trset1,anum1)
      
      
    }
    else{
      
      #if the variable is already numeric we simply add in on the left of the existing test and training set
      
      testset1<-cbind(testset1,vari[setdiff(1:l,ra),a[m,ai[k]]])
      trset1<-cbind(trset1,vari[ra,a[m,ai[k]]])
      
      
    }
    
  }
  
  #output the transformed test and training sets
  
  list("test" = testset1, "tr" = trset1)
  
  
}

#'Weights of predictors
#'
#'Function which computes the weight of each predictor according to the rules of thumb and outputs it into corresponding array
#'
#'@param vari_col number of predictors
#'@param vari set of predictors
#'@details Continuous or categorical numerical variable with more then 5 categories has weight 1, otherwise it has weight \code{n-1} where \code{n} is the number of categories
#'@return Returns an array of weights of the size \code{vari_col}
#'@usage compute_weights(vari_col, vari)
#'@export compute_weights
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@references{
#'\insertRef{ref2012-18631-001}{CARRoT}
#'}
#'@importFrom Rdpack reprompt
#'@examples
#'#creating data-set with for variables
#'
#'a<-matrix(NA,nrow=100,ncol=4)
#'
#'#binary variable
#'
#'a[,1]=rbinom(100,1,0.3)
#'
#'#continuous variable
#'
#'a[,2]=runif(100,0,1)
#'
#'#categorical numeric with les than 5 categories
#'
#'a[,3]=t(rmultinom(100,1,c(0.2,0.3,0.5)))%*%c(1,2,3)
#'
#'#categorical numeric with 5 categories
#'
#'a[,4]=t(rmultinom(100,1,c(0.2,0.3,0.3,0.1,0.1)))%*%c(1,2,3,4,5)
#'
#'#running the function
#'
#'compute_weights(4,a)


compute_weights<-function(vari_col,vari){
  
  #initialising and empty array of weights
  
  we<-matrix(nrow=0,ncol=1);
  
  #going through all the predictive variables
  
  for (i in 1:vari_col){
    
    #if the variable is numeric it has a weight 1
    
    if (is.numeric(vari[,i])==TRUE) {
      
      we<-array(c(we,1))
      
    } else{
      
      #otherwise it has a weight of the number of categories minus one
      
      we<-array(c(we,length(unique(vari[,i]))-1));
    }
    
  }
  
  #outputting the array of weights
  
  we
  
}

#'Maximum feasible weight of the predictors
#'
#'Function which computes maximal weight (multiplied by 10) of a regression according to the rule of thumb applied to the outcome variable. Weight of a regression equals the sum of weights of its predictors.
#'
#'@details For continuous outcomes it equals sample size divided by 10, for multinomial it equals the size of the smallest category divided by 10
#'@param outi set of outcomes
#'@param mode indicates the mode: 'linear' (linear regression), 'binary' (logistic regression), 'multin' (multinomial regression)
#'@usage compute_max_weight(outi,mode)
#'@return returns an integer value of maximum allowed weight multiplied by 10
#'@export compute_max_weight
#'@examples
#'#continuous outcomes
#'
#' compute_max_weight(runif(100,0,1),'linear')
#'
#' #binary outcomes
#'
#' compute_max_weight(rbinom(100,1,0.4),'binary')
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@importFrom Rdpack reprompt




compute_max_weight<-function(outi,mode){
  
  if (mode=='linear') { #if the mode is linear maximal weight multiplied by the corresponding EPV rule is defined by the sample size
    
    numr=length(outi)
    
  } else{ #otherwise
    
    unio<-unique(outi); #find all unique elements of the outcome
    
    lo<-array(NA,length(unio)) #initialise an array of the corresponding length
    
    for (i in 1:length(unio)) lo[i]=length(which(outi==unio[i])); #count the occurrences of the each type of outcome
    
    numr=min(lo); #choose the smallest one to defined the maximal weight multiplied by the corresponding EPV rule
  }
  
  numr #output the obtained value
  
}

#'Cumulative weights of the predictors' subsets
#'
#'Function which computes the sum of predictors' weights for each subset containing a fixed number of predictors
#'
#'@param a an \code{m} x N matrix, containing all possible subsets (N overall) of the size \code{m} of predictors' indices; therefore each column of \code{a} defines a unique subset of the predictors
#'@param m number of elements in each subset of indices
#'@param we array of weights of the predictors
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@usage sum_weights_sub(a,m,we,st)
#'@return Returns an array of weights for predictors defined by each colun of the matrix \code{a}
#'@export sum_weights_sub
#'@examples
#'#all two-element subsets of the set 1:3
#'
#'a<-combn(3,2)
#'
#'sum_weights_sub(a,2,c(1,2,1))



sum_weights_sub<-function(a,m,we,st=NULL){
  
  
  s<-array(NA,ncol(a)); #the array corresponding to the number of the feasible subsets
  
  if ((m>1)|(is.null(st)==FALSE)){ #if the size of the subset is greater than 1 or if it consists only of stationary part
    
    for (h in 1:ncol(a))
      
      s[h]=sum(we[a[((h-1)*m+1):(h*m)]]); #sum up the corresponding weights
    
  } else s=we; #otherwise the target value is exactly the array of weights
  
  s  #output the value
  
}


#'Finds certain subsets of predictors
#'
#'Reorders the columns of matrix \code{a} according to the ordered elements of array \code{s}
#'@param a A \code{j} x N matrix, containing all possible subsets (N overall) of the size \code{j} of predictors' indices.
#'@param s array of numbers of the size N
#'@param j number of rows in \code{a}
#'@param c array of all indices of the predictors
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@usage find_sub(a,s,j,c,st)
#'@return Returns a submatrix of matrix \code{a} which consits of columns determined by the input array \code{s}
#'@export find_sub
#'@examples
#'#all two-element subsets of 1:3
#'
#'a<-combn(3,2)
#'s<-c(3,2,3)
#'
#'find_sub(a,s,2,1:3)



find_sub<-function(a,s,j,c,st){#
  
  
  if (j==1){ #if all the subsets are of the size 1
    
    a=t(matrix(a[,order(s)])); #transforming array of subsets according to the order of the array s
    
  } else{
    
    if (dim(a)[2]==1){ #if there is only one subset
      
      a=matrix(a[,order(s)]);
      
    } else a=a[,order(s)]; #if there is more than one subset of size larger than 1
    
  }
  
  a #outputting the transformed subset
  
}

#'Maximum number of the regressions
#'
#'Function which computes the maximum number of regressions with fixed number of variables based on the rule of thumb
#'
#'@param vari_col number of predictors
#'@param k maximum weight of the predictors
#'@param c array of all indices of the predictors
#'@param we array of weights of the predictors. Continuous or categorical numerical variable with more then 5 categories has weight 1, otherwise it has weight \code{n-1} where \code{n} is the number of categories
#'@param minx minimum number of predictors in a single model, 1 variable by default
#'@param maxx maximum number of predictors in a single model, total number of variables by default
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@import utils
#'@return Integer correponding to maximum number of regressions of the same size
#'@usage compute_max_length(vari_col,k,c,we,minx,maxx,st)
#'@seealso Function uses \code{\link[utils]{combn}}
#'@export compute_max_length
#'@references{
#'\insertRef{ref1}{CARRoT}
#'}
#'@references{
#'\insertRef{ref2012-18631-001}{CARRoT}
#'}
#'@importFrom Rdpack reprompt
#'@examples
#'compute_max_length(4,40,1:4,c(1,1,2,1))


compute_max_length<-function(vari_col,k,c,we,minx=1,maxx=NULL,st=NULL){ #
  
  #initialising an array of length of the number of types of regressions subject to parameter k, number of variables,minimal number of variables and maximal number of variables
  
  le<-array(NA,min(min(vari_col,k)-minx+1,maxx-minx+1))
  
  #going through regressions with the number of variables we are willing to consider
  
  for (m in max(minx,1):min(min(vari_col,k),maxx)){
    
    a<-combn(c,m); #all subsets of variables of the size m
    
    s<-sum_weights_sub(a,m,we,st); #computing the corresponding weights
    
    le[m-minx+1]=length(which(s<=k)); #number of regression of the given size satisfying the weight constraint
    
  }
  
  max(le) #outputting the size of the largest one
  
  
}


#'Probabilities for multinomial regression
#'
#'Function which computes probabilities of outcomes on the test set by applying regression parameters inferred by a run on the training set. Works for logistic or multinomial regression
#'@param trset values of predictors on the training set
#'@param testset values of predictors on the test set
#'@param outc values of outcomes on the training set
#'@param mode \code{'binary'} (logistic regression) or \code{'multin'} (multinomial regression)
#'@usage get_probabilities(trset,testset,outc,mode)
#'@return Probabilities of the outcomes. In \code{'binary'} mode returns an array of the size of the number of observations in a testset. In \code{'multin'} returns an M x N matrix where M is the size of the number of observations in a testset
#'and N is the number of unique outcomes minus 1.
#'@details In binary mode this function computes the probabilities of the event '0'. In multinomial mode computes the probabilities of the events '0','1',...,'N-1'.
#'@seealso Function uses \code{\link[nnet]{multinom}}  and \code{\link[stats]{coef}}
#'@import stats
#'@import nnet
#'@export get_probabilities
#'@examples
#'trset<-matrix(c(rbinom(70,1,0.5),runif(70,0.1)),ncol=2)
#'
#'testset<-matrix(c(rbinom(10,1,0.5),runif(10,0.1)),ncol=2)
#'
#'get_probabilities(trset,testset,rbinom(70,1,0.6),'binary')


get_probabilities<-function(trset,testset,outc,mode){
  
  #dimensions of the test set
  
  d<-dim(data.matrix(testset));
  
  
  if (mode=='binary'){
    
    #for binary mode compute the coefficients of the regression
    
    regr<-data.matrix(coef(multinom(-outc~.,data=data.frame(trset),trace=FALSE)));
    
    #applying coefficients to the test set and immediately transforming the result in order to get probabilities later
    
    ps=exp(matrix(rep(1,d[1]))%*%regr[1,]+data.matrix(testset)%*%regr[2:length(regr),]);
    
    
  } else {
    
    #for multinomial mode compute the coefficients of the regression
    
    regr<-apply(data.matrix(coef(multinom(-outc~.,data=data.frame(trset),trace=FALSE))),2,rev);
    
    #applying coefficients to the test set and immediately transforming the result in order to get probabilities later
    
    ps=rowSums(exp(matrix(rep(1,d[1]))%*%regr[,1]+data.matrix(testset)%*%t(regr[,2:dim(regr)[2]])));
    
  }
  
  #getting rid of infinite values of ps
  
  coe=8;
  
  ps[ps==Inf]=max(ps[ps<Inf])*coe;
  
  while(length(ps[ps==Inf])>0){
    coe=coe/2;
    ps[ps==Inf]=max(ps[ps<Inf])*coe;
  }
  
  #computing the first term in the product in order to get probabilities
  
  p1=t(1/(1+ps));
  
  #computing the second term in the product in order to get probabilities for binary and multinomial probabilities
  
  if (mode=='binary') { 
    
    p0=exp(matrix(rep(1,d[1]))%*%regr[1,]+data.matrix(testset)%*%regr[2:length(regr),]);
    
  } else p0=exp(matrix(rep(1,d[1]))%*%regr[,1]+data.matrix(testset)%*%t(regr[,2:dim(regr)[2]]));
  
  if (mode=='binary') { #computing the probabilities for binary mode
    
    p=t(p1)*p0;
  } else { #computing the probabilities for multinomial mode and combining them into a matrix
    
    p=t(p1)*p0[,1]
    
    for (i in 2:dim(p0)[2]) p=cbind(p,t(p1)*p0[,i])
    
  }
  
  
  if (sum(is.na(p))>0) { #output an error message in case there are undefined values of p
    
    stop('undefined prediction probabilities')
    
  }
  
  p #output the probabilities
  
  
}

#'Predictions for linear regression
#'
#'Function which runs a linear regression on a training set, computes predictions for the test set
#'
#'@param trset values of predictors on the training set
#'@param testset values of predictors on the test set
#'@param outc values of predictors on the training set
#'@param k length of the test set
#'@usage get_predictions_lin(trset,testset,outc,k)
#'@return An array of continous variables of the length equal to the size of a \code{testset}
#'@seealso Function uses function \code{\link[stats]{lsfit}} and \code{\link[stats]{coef}}
#'@export get_predictions_lin
#'@examples
#'trset<-matrix(c(rnorm(90,2,4),runif(90,0,0.5),rbinom(90,1,0.5)),ncol=3)
#'
#'testset<-matrix(c(rnorm(10,2,4),runif(10,0,0.5),rbinom(10,1,0.5)),ncol=3)
#'
#'get_predictions_lin(trset,testset,runif(90,0,1),10)

get_predictions_lin<-function(trset,testset,outc,k){
  
  #write the coefficients of the linear regression fitted to the corresponding training set into an array
  
  regr<-data.matrix(coef(lsfit(as.matrix(trset),outc)));
  
  #initialise an array of predictions
  
  pred<-array(NA,c(1,k));
  
  if (sum(outc%%1)==0){ # in case the outcomes are integers round the predictions
    
    pred<-round(data.matrix(testset)%*%matrix(regr[2:length(regr)])+regr[1]);
    
  } else{ #otherwise do not round them
    
    pred<-data.matrix(testset)%*%matrix(regr[2:length(regr)])+regr[1];
    
    
    
  }
  
  #in case all outcomes are positive assign value zero to all negatuve predictions
  
  if (length(outc[outc<0]==0)) pred[pred<0]=0;
  
  #output the array of predictions
  
  pred
  
  
}

#'Predictions for multinomial regression
#'
#'Function which makes a prediction for multinomial/logistic regression based on the given cut-off value and probabilities.
#'@param p probabilities of the outcomes for the test set given either by an array (logistic regression) or by a matrix (multinomial regression)
#'@param k size of the test set
#'@param cutoff cut-off value of the probability
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@usage get_predictions(p,k,cutoff,mode)
#'@seealso Uses \code{\link[stats]{rbinom}}, \code{\link[stats]{rmultinom}}
#'@return Outputs the array of the predictions of the size of \code{p}.
#'@export get_predictions
#'@examples
#'#binary mode
#'
#'get_predictions(runif(20,0.4,0.6),20,0.5,'binary')
#'
#'#creating a data-set for multinomial mode
#'
#'p1<-runif(20,0.4,0.6)
#'p2<-runif(20,0.1,0.2)
#'p3<-1-p1-p2
#'
#'#running the function
#'
#'get_predictions(matrix(c(p1,p2,p3),ncol=3),20,0.5,'multin')


get_predictions<-function(p,k,cutoff,mode){
  
  #initialise the array of predictions
  
  pred<-array(NA,c(1,k));
  
  #going through all objects in the testset
  
  for (m in 1:k){
    
    
    if (mode=='binary'){
      
      #making a prediction based on the cut-off value
      
      pred[m]=ifelse(p[m]>cutoff,0,1)
      
      
    } else{
      
      #for multinomial mode prediction based on maximal probability
      
      maxpr=which(c(p[m,],1-sum(p[m,]))==max(c(p[m,],1-sum(p[m,]))));
      
      pred[m]=maxpr-1;
      
      
    }
  }
  
  #outputting the array of predictions
  
  pred
  
}


#'Combining in a list
#'
#'Function for combining outputs in a list
#'@param ... an argument of \code{mapply} used by this function
#'@seealso Function \code{\link[base]{mapply}}
#'@export comb
#'@examples
#'#array of numbers to be separated in a list
#'
#'a<-1:4
#'
#'#running the function
#'
#'comb(a)

comb <- function(...) {
  mapply('cbind', ..., SIMPLIFY=FALSE)
}

#'Area Under the Curve
#'
#'Function enables efficient computation of area under receiver operating curve (AUC). Source: \url{https://stat.ethz.ch/pipermail/r-help/2005-September/079872.html}
#'@param probs probabilities
#'@param class outcomes
#'@usage AUC(probs, class)
#'@return A value for AUC
#'@export AUC
#'@examples
#'AUC(runif(100,0,1),rbinom(100,1,0.3))

AUC <- function(probs, class) {
  x <- probs
  y <- class
  x1 = x[y==1]; n1 = length(x1);
  x2 = x[y==0]; n2 = length(x2);
  r = rank(c(x1,x2))
  auc = (sum(r[1:n1]) - n1*(n1+1)/2) / n1 / n2
  return(auc)
}

#'Cross-validation run
#'
#'Function running a single cross-validation by partitioning the data into training and test set
#'
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param c set of all indices of the predictors
#'@param part indicates partition of the original data-set into training and test set in a proportion \code{(part-1):1}
#'@param l number of observations
#'@param we weights of the predictors
#'@param vari_col overall number of predictors
#'@param preds array to write predictions into, intially empty
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param cutoff cut-off value for logistic regression
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@param rule an Events per Variable (EPV) rule, defaults to 10
#'@param corr maximum correlation allowed between the variables in a single model, defaults to 1
#'
#@usage cross_val(vari,outi,c,rule,part,l,we,vari_col,preds,mode,predm,cutoff,objfun,minx,maxx,nr,maxw)
#'@return
#'\item{regr}{An M x N matrix of sums of the absolute errors for each element of the test set for each feasible regression. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression. Therefore each row corresponds to results obtained from running regressions with the same number of variables and columns correspond to different subsets of predictors used.}
#'\item{regrr}{An M x N matrix of sums of the relative errors for each element of the test set (only for \code{mode = 'linear'}) for each feasible regression. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression. Therefore each row corresponds to results obtained from running regressions with the same number of variables and columns correspond to different subsets of predictors used.}
#'\item{nvar}{Maximum feasible number of variables in the regression}
#'\item{emp}{An accuracy of always predicting the more likely outcome as suggested by the training set (only for \code{mode = 'binary'} and \code{objfun = 'acc'})}
#'In \code{regr} and \code{regrr} \code{NA} values are possible since for some numbers of variables there are fewer feasible regressions than for the others.
#'@seealso Uses \code{\link{compute_max_weight}}, \code{\link{sum_weights_sub}}, \code{\link{make_numeric_sets}}, \code{\link{get_predictions_lin}}, \code{\link{get_predictions}}, \code{\link{get_probabilities}}, \code{\link{AUC}}, \code{\link[utils]{combn}}
#'@export cross_val
#'@examples
#'#creating variables
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes
#'
#'out<-rbinom(100,1,0.3)
#'
#'#creating array for predictions
#'
#'preds<-array(NA,c(2,2))
#'
#'#running the function
#'
#'cross_val(vari,out,1:2,10,10,100,c(1,1),2,preds,'binary','exact',0.5,'acc',nr=c(1,4))

cross_val<-function(vari,outi,c,rule,part,l,we,vari_col,preds,mode,predm,cutoff,objfun,minx=1,maxx=NULL,nr=NULL,maxw=NULL,st=NULL,corr=1){
  
  if ((objfun!='acc')&(objfun!='roc')) stop('Unknown value of the parameter "objfun"')
  
  #for linear mode initialising the array of relative errors
  
  if (mode=='linear') predsr<-preds
  
  #creating the partition into training and test set given that no subset is specified to be necessarily present both in the training and test set
  
  if (is.null(nr)==TRUE){
    
    ra=sample(1:l,floor((1-(1/part))*l));
    
  } else { #creatinng the partition when parameter nr is specified
    
    lnr=length(nr);
    
    ranr=sample(nr,floor((1-(1/part))*lnr)) #partitioning nr itself
    
    #partitioning the remaining datapoints
    
    rar=sample(setdiff(1:l,nr),floor((1-(1/part))*l)-floor((1-(1/part))*lnr));
    
    #combining the two
    
    ra=c(ranr,rar);
    
  }
  
  #defining the training set
  
  trset<-vari[ra,];
  
  #outcomes corresponding to the training set
  
  outc<-outi[ra];
  
  #testset
  
  testset<-vari[setdiff(c(1:l),ra),];
  
  #computing the maximum allowed weight given the outcomes in the training set and the mode of the prediction
  
  mw<-compute_max_weight(outc,mode)
  
  #maximal weight given the input parameter maxw (if specified)
  
  mw=min(mw,maxw*rule);
  
  #maximal number of variables allowed in a single regression
  
  nvar=floor((1/rule)*mw);
  
  #error message if this number is lower than the minimum allowed weight
  
  if (nvar<min(min(we),maxw)) stop('not enough events to build a regression model');
  
  #maximal number of variables taking into account restriction by the parameter maxx (if any)
  
  if (is.null(maxx)==FALSE){
    
    maxj=min(maxx,min(nvar,vari_col))
    
  } else {
    
    maxj=min(nvar,vari_col)
  }
  
  #minimal number of variables taking into account restriction by the parameter minx (if any)
  
  
  minj=max(1,minx)
  
  #subset of indices of the regression without the "fixed subset" defined by parameter st
  
  c=setdiff(c,st);
  
  #length of the "fixed subset" defined by parameter st
  
  lest=length(st)
  
  #going through all possible sizes of the regression model
  
  for (j in max(minj,lest):maxj){
    
    #creating a matrix of all possible regressions of a given size j, taking into account the "fixed subset" defined by st
    
    a<-combn(c,(j-lest));
    
    #in case c contains only 1 element and st is empty
    
    if ((length(c)<2)&(j>lest)) a=matrix(c)
    
    #in case st is non-empty to each column of a add a column of elements of st
    
    if (is.null(st)==FALSE) a<-rbind(matrix(st,ncol=dim(a)[2],nrow=lest),a)
    
    #compute the weights of each column of a
    
    s<-sum_weights_sub(a,j,we,st)
    
    #reoders columns of a in ascending order corrresponding to the order of the array of weights s
    
    a<-find_sub(a,s,j,c(st,c),st)
    
    #sort the array of weights
    
    s=sort(s);
    
    #find those weights which satisfy the weight constraint (aka maximal number of variables in the regression)
    
    ai=which(s<=max(nvar));
    
    if (length(ai)>0){
      
      for (k in 1:length(ai)){    #going through all elements of ai
        
        #transform the corresponding subset of variables into numeric one
        
        set_num<-make_numeric_sets(a,ai,k,vari,ra,l)
        
        #numeric test set
        
        testset1<-set_num$test
        
        #numeric training set
        
        trset1<-set_num$tr
        
        #initialise correlation parameter to 0
        
        corr1<-0
        
        if (corr<1){ #if correlation parameter corr is smaller than 1 compute absolute correlations between variables on the training set
          
          corr1<-abs(cor(trset1))
        }
        
        #if there is no restriction on highly correlated predictors or if there are no highly correlated predictors in the kth subset
        
        if ((length(corr1[corr1>corr])<=length(a[,ai[k]]))|(corr==1)){
          
          
          if (mode=='linear'){ #linear mode
            
            #get the prediction for the training set
            
            pred<-get_predictions_lin(trset1,testset1,outc,l-floor((1-1/part)*l))
            
            #difference between the actual value and the predicted one on the test set
            
            diff<-outi[setdiff(1:l,ra)]-pred;
            
            #difference between predicted value and the actual one divided by the actual one, aka relative difference
            
            diffr<-(pred/outi[setdiff(1:l,ra)])-1;
            
            #sum of all absolute values of differences defined above
            
            preds[j-minx+1,k]=sum(abs(diff));
            
            #sum of all absolute values of relative differences defined above
            
            predsr[j-minx+1,k]=sum(abs(diffr));
            
            
          } else{
            
            #computing the probabilities for each outcome on the test set by fitting multinomial/logistic regression to the training set
            
            p<-get_probabilities(trset1,testset1,outc,mode);
            
            
            if (objfun=='acc'){ #case of accuracy maximisation
              
              #transforming probabilities into predictions
              
              pred<-get_predictions(p,l-floor((1-1/part)*l),cutoff,mode)
              
              #difference between the actual values and the predicted ones on the test set
              
              diff<-outi[setdiff(1:l,ra)]-array(pred);
              
              
              
              if (predm=='exact')  { #in case of the exact prediction, aka predicting the exact class
                
                diff[abs(diff)>0]=1;
              } else{
                
                #in case of "up to a class" prediction, aka consider correct if the correct class is the one neighboring to the predicted one
                
                diff[abs(diff)<2]=0;
                diff[abs(diff)>1]=1;
              }
              
              #computing the number of times prediction was correct (non-averaged out accuracy)
              
              #rows correspond to the number of variables in a regression, column is determined by k
              
              
              preds[j-minx+1,k]=l-floor((1-1/part)*l)-sum(abs(diff));
              
              
              
            } else{
              
              #computing the AUROC of the prediction
              
              preds[j-minx+1,k]<-AUC(1-p,outi[setdiff(1:l,ra)])
            }
          }
        } 
      }
    }
  }
  
  
  
  if ((mode=='binary')&(objfun=='acc')){
    
    #empirical prediction based on always choosing the most frequent category
    
    cpred=sum(outi[setdiff(1:l,ra)])/(l-floor((1-1/part)*l));
    
    #output is a list of (non-averaged out) accuracies of all feasible regression models, the corredponding maximal nimber of variables, the corredsponding empirical prediction
    
    list("regr" = preds, "nvar"=nvar, "emp" = cpred)
    
    
  } else{
    
    if (objfun=='roc') {
      
      #list of AUROCs for all feasible model and the corrresponding maximal number of variables
      
      list("regr" = preds, "nvar"=nvar)
      
    } else{
      
      if (mode=='multin'){
        
        #unique elements of the training set outcomes
        
        uo<-unique(outc);
        
        #empirical prediction based on always choosing the most frequent category
        
        cpred=1-length(which(outi[setdiff(1:l,ra)]==uo[which.max(tabulate(match(outc,uo)))]))/(l-floor((1-1/part)*l));
        
        #output is a list of (non-averaged out) accuracies of all feasible regression models, the corredponding maximal nimber of variables, the corredsponding empirical prediction
        
        list("regr" = preds, "nvar"=nvar, "emp" = cpred)
        
      } else{ #linear mode
        
        #empirical predictions based on always choosing the mean of the training set (empirical absolute and relative errors respectively)
        
        cpred=sum(abs(outi[setdiff(1:l,ra)]-mean(outi[ra])))/(l-floor((1-1/part)*l));
        
        cpredr=sum(abs((outi[setdiff(1:l,ra)]-mean(outi[ra]))/outi[setdiff(1:l,ra)]))/(l-floor((1-1/part)*l));
        
        #output is a list of non-averaged out absolute errors of all feasible regression models
        #the corredponding maximal nimber of variables, non-averaged out relative errors of all feasible regression models
        #absolute error of the empirical prediction, relative error of the empirical prediction
        
        
        list("regr" = preds, "nvar"=nvar, "regrr"=predsr, "emp"=cpred,"empr"=cpredr)
        
      }
    }
    
  }
  
  
}


#'Averaging out the predictive power
#'
#'Function which averages out the predictive power over all cross-validations
#'@param preds An M x \code{crv}N matrix consisting of \code{crv} horizontally concatenated M x N matrices. These M x N matrices are the matrices of predictive powers for all feasible regressions (M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression)
#'@param crv number of cross-validations
#'@param k size of the test set for which the predictions are made
#'@usage av_out(preds,crv,k)
#'@return Returns an M x N matrix of average predictive powers where M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression
#'@export av_out
#'@examples
#'#creating a matrix of predictive powers
#'
#'preds<-cbind(matrix(runif(40,1,4),ncol=10),matrix(runif(40,1.5,4),ncol=10))
#'preds<-cbind(preds,matrix(runif(40,1,3.5),ncol=10))
#'
#'#running the function
#'
#'av_out(preds,3,5)


av_out<-function(preds,crv,k){
  
  #writing the dimensions of the matrix of preditive powers into the array
  
  si<-dim(preds);
  
  #dividing the number of columns in preds by the number of cross-validations, since the results from each next cross-validation are always concatenated with the previous one
  
  si[2]=si[2]/crv;
  
  #initialising the array of averaged out predictive powers 
  
  predsp<-array(NA,c(si[1],si[2])) #predictive power corresponding to the same model from all cross-validations
  
  for (i in 1:si[1]){
    for (j in 1:si[2]){
      
      pr<-preds[i,seq(j,si[2]*crv,si[2])]; #predictive power corresponding to the same model from all cross-validations
      
      #the mean value of the corresponding predictive power divided by the size of the test set
      
      predsp[i,j]=mean(pr[is.finite(pr)],na.rm=TRUE)/k;
      
    }
  }
  
  #output is the matrix of the averaged out predictive powers
  
  predsp
  
}

#'Best regression
#'
#'Function which identifies regressions with the highest predictive power
#'
#'@param predsp An M x N matrix of averaged out predictive power values. M is maximum feasible number of variables included in a regression, N is the maximum feasible number of regressions of the fixed size; the row index indicates the number of variables included in a regression.
#'@param nvar array of maximal number of variables for each cross-validation
#'@param c array of all indices of the prediction variables
#'@param we array of all weights of the prediction variables
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@param minx minimum number of predictors to be included in a single regression, defaults to 1
#'@usage get_indices(predsp,nvar,c,we,st,minx)
#'@return A list of arrays which contain indices of the predictors corresponfing to the best regressions
#'@seealso Uses \code{\link{sum_weights_sub}}, \code{\link{find_sub}}, \code{\link[utils]{combn}}
#'@export get_indices
#'@examples
#'#creating a set of averaged out predictive powers
#'
#'predsp<-matrix(NA,ncol=3,nrow=3)
#'
#'predsp[1,]=runif(3,0.7,0.8)
#'predsp[2,]=runif(3,0.65,0.85)
#'predsp[3,1]=runif(1,0.4,0.5)
#'
#'#running the function
#'
#'get_indices(predsp,c(3,3,3),1:3,c(1,1,1))

get_indices<-function(predsp,nvar,c,we,st=NULL,minx=1){
  
  #creating a list
  
  ll=list(0)
  
  #finding the index of the arry of the averaged out predictive powers which corresponds to the highest predictive power
  
  nums<-which(predsp==max(predsp[predsp!=0],na.rm=TRUE))
  
  #dimensions of the array of the averaged out predictive powers
  
  si=dim(predsp)
  
  #computing the number of variables of the best predictive model based on the row it corresponds to
  
  numv=nums%%si[1]
  
  #finding the column which corresponds to the model with the best predictive power
  
  numss=ceiling(nums/si[1])
  
  #array of predictive variables without the "fixed subset" defined by parameter st
  
  c=setdiff(c,st)
  
  #length of the "fixed subset"
  
  lest=length(st)
  
  #going through all models which exhibited the highest predictive power
  
  for (i in 1:length(numv)){
    
    #in case the value of the number of variables is 0 reassign the maximal number of variables to it
    
    if (numv[i]==0) numv[i]=si[1]
    
    #add the minimal number of variables defined by parameter minx to the number of variables
    
    numv1=numv[i]+minx-1
    
    #all subsets of size numv1 minus the size of the "fixed subset" defined by st
    
    af<-combn(c,(numv1-lest))
    
    #in case there is only one predictor and st is empty
    
    if ((length(c)<2)&(numv1>lest)) af<-matrix(c)
    
    #in case the "fixed subset" is not empty add to each column of af a column with elements of st
    
    if (is.null(st)==FALSE) af<-rbind(matrix(st,ncol=dim(af)[2],nrow=lest),af)
    
    #compute the weights of models corresponding to columns of af
    
    s<-sum_weights_sub(af,numv1,we,st)
    
    #reorder the columns of sf based on the order of the array of weights s
    
    af<-find_sub(af,s,numv1,c,st)
    
    #sort the array of weights
    
    s=sort(s)
    
    #find the models with the weights satisfying the aximal weight constraint
    
    aif=which(s<=max(nvar));
    
    #the column of af exhibiting the best predictive power is written as an ith element of the list
    
    ll[[i]]<-af[,aif[numss[i]]]
    
  }
  
  
  ll
}

#'Indices of the best regressions
#'
#'One of the two main functions of the package. Identifies the predictors included into regressions with the highest average predictive power
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param crv number of cross-validations
#'@param cutoff cut-off value for mode \code{'binary'}
#'@param part for each cross-validation partitions the dataset into training and test set in a proportion \code{(part-1):part}
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, available only for \code{mode='binary'}; \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param parallel TRUE if using parallel toolbox, FALSE if not. Defaults to FALSE
#'@param cores number of cores to use in case of parallel=TRUE
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set. This is to ensure that elements of this subset are included both in the training and in the test set.
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@param rule an Events per Variable (EPV) rule, defaults to 10
#'@param corr maximum correlation allowed between the variables in a single model, defaults to 1
#'@return Prints the best predictive power provided by a regression, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic and linear regression). Returns indices of the predictors included into regressions with the highest predictive power written in a list. For \code{mode='linear'} outputs a list of two lists. First list corresponds to the smallest absolute error, second corresponds to the smallest relative error
#'@export regr_ind
#'@import doParallel
#'@import parallel
#'@import foreach
#'@seealso Uses \code{\link{compute_weights}}, \code{\link{make_numeric}}, \code{\link{compute_max_weight}}, \code{\link{compute_weights}}, \code{\link{compute_max_length}}, \code{\link{cross_val}},\code{\link{av_out}}, \code{\link{get_indices}}
#'@examples
#'#creating variables for linear regression mode
#'
#'variables_lin<-matrix(c(rnorm(56,0,1),rnorm(56,1,2)),ncol=2)
#'
#'#creating outcomes for linear regression mode
#'
#'outcomes_lin<-rnorm(56,2,1)
#'
#'#running the function
#'
#'regr_ind(variables_lin,outcomes_lin,100,mode='linear',parallel=TRUE,cores=2)
#'
#'#creating variables for binary mode
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes for binary mode
#'
#'out<-rbinom(100,1,0.3)
#'
#'#running the function
#'
#'regr_ind(vari,out,20,cutoff=0.5,part=10,mode='binary',parallel=TRUE,cores=2,nr=c(1,10,20),maxx=1)




regr_ind<-function(vari,outi,crv,cutoff=NULL,part=10,mode,predm='exact',objfun='acc',parallel=FALSE,cores,minx=1,maxx=NULL,nr=NULL,maxw=NULL,st=NULL,rule=10,corr=1){
  
  #in case of an error close all the connections
  
  
  on.exit(closeAllConnections());
  
  #error message in case of incompatible moe and objfun parameters
  
  
  if ((objfun=='roc')&(mode!='binary')) {
    
    stop('function "roc" is available for binary mode only')
    
  }
  
  #if there is only one predictive variable written in an array
  
  
  if (is.null(dim(vari))){
    
    vari=matrix(vari);
  }
  
  #overall number of predictive variables
  
  vari_col=ncol(vari);
  
  #compute weights of all predictive variables and write them into an array
  
  we<-compute_weights(vari_col,vari)
  
  #the sample size
  
  l=nrow(vari);
  
  #the array of all indices of predictive variables
  
  c<-c(1:vari_col);
  
  if (is.numeric(outi)==FALSE){ #turning non-numeric outcomes in numeric ones
    
    outi<-make_numeric(outi,TRUE)
    
  } 
  
  
  #compute maximum weight defined by the outcomes
  
  numr<-compute_max_weight(outi,mode);
  
  #compute maximal length of a regression which can be fitted to a training set
  
  le<-compute_max_length(vari_col,floor((1.0/(rule*min(we)))*numr),c,we,minx,maxx+1,st)
  
  #initialising the array of predictive powers of all feasible regression
  
  preds<-array(NA,c(min(vari_col,min(floor((1/(rule*min(we)))*numr),maxx+1))-minx+1,le));
  
  #defining a %fun% function
  
  `%fun%` <- `%do%`
  
  if (parallel==TRUE){ #in case the parallel mode is on
    
    `%fun%` <- `%dopar%`
    
    #creating a cluster of the corresponding size
    
    cl <- makeCluster(cores)
    
    #exporting the corresponding libraries to all cores
    
    registerDoParallel(cl)
    
    clusterEvalQ(cl,rm(list=ls()))
    clusterEvalQ(cl, library(nnet))
    clusterEvalQ(cl, library(foreach))
    clusterEvalQ(cl, library(doParallel))
    clusterEvalQ(cl, library(stats))
    
    #exporting all necessary functions to all cores
    
    clusterExport(cl,"cross_val")
    clusterExport(cl,"compute_max_weight")
    clusterExport(cl,"sum_weights_sub")
    clusterExport(cl,"get_probabilities")
    clusterExport(cl,"get_predictions")
    clusterExport(cl,"get_predictions_lin")
    clusterExport(cl,"find_sub")
    clusterExport(cl,"make_numeric")
    clusterExport(cl,"make_numeric_sets")
    clusterExport(cl,"AUC")
    
    
  }
  
  #running the given number of cross-validations
  
  result <- foreach(i=1:crv, .combine='comb', .multicombine=TRUE,.packages="CARRoT") %fun% {
    
    cross_val(vari,outi,c,rule,part,l,we,vari_col,preds,mode,predm,cutoff,objfun,minx,maxx,nr,maxw,st,corr);
    
  }
  
  #writing predictive powers of the regressions in an array
  
  preds<-result[[1]];
  
  #writing the maximal number of variables in an array
  
  nvar<-result[[2]];
  
  
  
  if (mode=='binary') {
    
    if (objfun=='acc') {  #write the array of empirical predictions
      
      cpred<-result[[3]];
      
    }
  } else {
    
    if (mode=='linear'){
      
      #write the array of relative errors
      
      predsr<-result[[3]];
      
      #write the array of absolute errors of empirical predictions
      
      cpred<-result[[4]];
      
      #write the array of relative errors of empirical predictions
      
      cpredr<-result[[5]];
    } else{
      
      #an array of empirical predictions
      
      cpred<-result[[3]]
    }
    
  }
  
  
  #stop the cluster
  
  if (parallel==TRUE) stopCluster(cl)
  
  closeAllConnections()
  
  #average out the predictive powers over all cross-validations
  
  if (objfun=='roc'){
    predsp<-av_out(preds,crv,1)
  } else{
    
    predsp<-av_out(preds,crv,l-floor((1-(1/part))*l))
  }
  
  if (mode=='linear')  {
    
    predspr<-av_out(predsr,crv,l-floor((1-(1/part))*l))
    
    
  }
  
  
  if (((mode=='binary')&(objfun=='acc'))|(mode=='multin')) {
    
    #print the average accuracy attained by the best predictive model and the empirical accuracy
    
    print(c(max(predsp[predsp>0],na.rm=TRUE),1-sum(cpred)/crv));
    
  } else{
    
    if (objfun=='roc') {
      
      #print the average AUROC of the best predictive model
      
      print(max(predsp[predsp>0],na.rm=TRUE))
      
    } else{
      
      
      if (sum(is.finite(predspr[predspr>0]))>0){
        
        #in case all average relative errors are finite 
        
        print(c(min(predsp[predsp>0],na.rm=TRUE),min(predspr[predspr>0],na.rm=TRUE),sum(cpred)/crv,sum(cpredr[cpredr<Inf])/(crv-length(cpredr[cpredr==Inf]))))
      } else {
        
        #printing NaN values for average relative error in case it is not finite
        
        print(c(min(predsp[predsp>0],na.rm=TRUE),NaN,sum(cpred)/crv,NaN))
        
        
      }
      
      
    }
  }
  
  
  
  if (mode=='linear') { #find the indices of the variables included into the regression with the best predictive power
    
    if (sum(is.finite(predspr))>0) {
      
      #find indices of the variables included in the models corresponding to the lowest absolute and lowest relative error
      
      list(get_indices(-predsp,nvar,c,we,st,minx),get_indices(-predspr,nvar,c,we,st,minx))
      
    } else {
      
      #in case relative error is infinite output NaN
      
      list(get_indices(-predsp,nvar,c,we,st,minx),NaN)
      
    }
    
  } else{
    
    #find indices of the variables included in the models corresponding to the highest accuracy/AUROC
    
    get_indices(predsp,nvar,c,we,st,minx)
    
  }
  
  
  
  
  
  
}

#' Best regressions
#'
#' Function which prints the highest predictive power, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic regression), outputs the regression objects corresponding to the highest average predictive power and the indices of the variables included into regressions with the best predictive power.
#' In the case of linear regression it outputs the best regressions with respect to both absolute and relative errors
#'@param vari set of predictors
#'@param outi array of outcomes
#'@param crv number of cross-validations
#'@param cutoff cut-off value for mode \code{'binary'}
#'@param part for each cross-validation partitions the dataset into training and test set in a proportion \code{(part-1):part}
#'@param mode \code{'binary'} (logistic regression), \code{'multin'} (multinomial regression)
#'@param predm \code{'exact'} or \code{''}; for logistic and multinomial regression; \code{'exact'} computes how many times the exact outcome category was predicted, \code{''} computes how many times either the exact outcome category or its nearest neighbour was predicted
#'@param objfun \code{'roc'} for maximising the predictive power with respect to AUC, available only for \code{mode='binary'}; \code{'acc'} for maximising predictive power with respect to accuracy.
#'@param parallel TRUE if using parallel toolbox, FALSE if not. Defaults to FALSE
#'@param cores number of cores to use in case of parallel=TRUE
#'@param minx minimum number of predictors to be included in a regression, defaults to 1
#'@param maxx maximum number of predictors to be included in a regression, defaults to maximum feasible number according to one in ten rule
#'@param maxw maximum weight of predictors to be included in a regression, defaults to maximum weight according to one in ten rule
#'@param nr a subset of the data-set, such that \code{1/part} of it lies in the test set and \code{1-1/part} is in the training set, defaults to empty set. This is to ensure that elements of this subset are included both in the training and in the test set.
#'@param st a subset of predictors to be always included into a predictive model,defaults to empty set
#'@param rule an Events per Variable (EPV) rule, defaults to 10
#'@param corr maximum correlation allowed between the variables in a single model, defaults to 1
#'@return Prints the highest predictive power provided by a regression, predictive accuracy of the empirical prediction (value of \code{emp} computed by \code{cross_val} for logistic regression).
#'\item{ind}{Indices of the predictors included into regressions with the best predictive power written in a list. For \code{mode='linear'} a list of two lists. First list corresponds to the smallest absolute error, second corresponds to the smallest relative error. This output is identical to the one from \code{regr_ind}}
#'\item{regr}{List of regression objects providing the best predictions. For \code{mode='multin'} and \code{mode='binary'}}
#'\item{regr_a}{List of regression objects providing the best predictions with respect to absolute error.For \code{mode='linear'}}
#'\item{regr_r}{List of regression objects providing the best predictions with respect to relative error.For \code{mode='linear'}}
#'@seealso Uses \code{\link{regr_ind}},\code{\link[stats]{lm}}, \code{\link[nnet]{multinom}}
#'@export regr_whole
#'@import doParallel
#'@import parallel
#'@import foreach
#'@examples
#'#creating variables for linear regression mode
#'
#'variables_lin<-matrix(c(rnorm(56,0,1),rnorm(56,1,2)),ncol=2)
#'
#'#creating outcomes for linear regression mode
#'
#'outcomes_lin<-rnorm(56,2,1)
#'
#'#running the function
#'
#'regr_whole(variables_lin,outcomes_lin,20,mode='linear',parallel=TRUE,cores=2)
#'
#'#creating variables for binary mode
#'
#'vari<-matrix(c(1:100,seq(1,300,3)),ncol=2)
#'
#'#creating outcomes for binary mode
#'
#'out<-rbinom(100,1,0.3)
#'
#'#running the function
#'
#'regr_whole(vari,out,20,cutoff=0.5,part=10,mode='binary',parallel=TRUE,cores=2)



regr_whole<-function(vari,outi,crv,cutoff=NULL,part=10,mode,predm='exact',objfun='acc',parallel=FALSE,cores,minx=1,maxx=NULL,nr=NULL,maxw=NULL,st=NULL,rule=10,corr=1){
  
  #error message in case of incompatible moe and objfun parameters
  
  if ((objfun=='roc')&(mode!='binary')) {
    
    stop('function "roc" is available for binary mode only')
    
  }
  
  #writing lists of indices of the variables included into the best regressions into the array
  
  ind<-regr_ind(vari,outi,crv,cutoff,part,mode,predm,objfun,parallel,cores,minx,maxx,nr,maxw,st,rule)
  
  if (mode=='linear'){ #linear mode
    
    regr_a=list(0)
    
    if (identical(ind[[1]][[1]],ind[[2]][[1]])){ #if the same models minimise both relative and absolute error
      
      
      
      for (i in 1:length(ind[[1]])){ #fit linear regression with the variables corresponding to the best models to the whole dataset
        
        regr_a[[i]]<-lsfit(outi,vari[,ind[[1]][[i]]]);
        
        
      }
      
      
      #output the list of corresponding regression objects and the indices of variables corresponding to them
      
      list("regr_a"=regr_a,"regr_r"=regr_a,"ind"=ind)
      
      
    } else { #if absolute and relative errors are minimised by different models
      
      
      
      regr_a=list(0)
      
      
      for (i in 1:length(ind[[1]])){ #fitting to the whole dataset regression which minimises absolute error
        
        regr_a[[i]]<-lsfit(outi,vari[,ind[[1]][[i]]]);
        
      }
      
      
      regr_r=list(0)
      
      
      for (i in 1:length(ind[[2]])){ #fitting to the whole dataset regression which minimises relative error
        
        regr_r[[i]]<-lsfit(outi,vari[,unlist(ind[[2]][[i]])]);
        
      }
      
      #output the list of corresponding regression objects and the indices of variables corresponding to them
      
      list("regr_a"=regr_a,"regr_r"=regr_r,"ind"=ind)
      
      
      
    }
    
  } else {  #multinomial or binary mode
    
    
    regr=list(0)
    
    for (i in 1:length(ind)){ #fitting to the whole dataset regression which mmaximises the predictive power
      
      regr[[i]]<-multinom(-outi~.,data=data.frame(vari[,ind[[i]]]),trace=FALSE)
      
    }
    
    #output the list of corresponding regression objects and the indices of variables corresponding to them
    
    list("regr"=regr,"ind"=ind)
    
    
    
  }
  
  
}


#' Pairwise interactions and squares
#'
#' Function transforms a set of predictors into a set of predictors, their squares and pairwise interactions
#'@param A set of predictors
#'@param n first \code{n} predictors, whose interactions with the rest should be taken into account, defaults to all of the predictors
#'@return Returns the predictors including their squares and pairwise interactions
#'@usage quadr(A,n)
#'@export quadr
#'@examples
#'quadr(cbind(1:100,rnorm(100),runif(100),rnorm(100,0,2)))


quadr<-function(A,n=1000){
  
  #copying the array of variables into array B
  
  B<-A
  if (n==1000){ #if the n parameter is set to default
    
    n=dim(A)[2] #the number of variables whose interactions are to be considers is the number of all variables
    
  }
  
  
  for (i in 1:n){
    
    B=cbind(B,A[,i:dim(A)[2]]*A[,i]) #multiplying variables in order to obtain pairwise interactions
    
  }
  
  B
}

#' Three-way interactions and squares
#'
#' Function transforms a set of predictors into a set of predictors, their squares, pairwise interactions, cubes and three-way interactions
#'@param A set of predictors
#'@param n first \code{n} predictors, whose interactions with the rest should be taken into account, defaults to all of the predictors
#'@return Returns the predictors including their squares, pairwise interactions, cubes and three-way interactions
#'@usage cub(A,n)
#'@export cub
#'@examples
#'cub(cbind(1:100,rnorm(100),runif(100),rnorm(100,0,2)))


cub<-function(A,n=1000){
  
  B<-quadr(A,n)  #creating an array of all pairwise interactions
  
  if (n==1000){#if the n parameter is set to default
    
    n<-dim(A)[2] #the number of variables whose interactions are to be considers is the number of all variables
    
  }
  
  m<-dim(B)[2] #the number of variables and their pairwise interactions
  
  for (i in 1:n){
    
    B=cbind(B,B[,(n+0.5*(n+n-i+2)*(i-1)+1):m]*A[,i]) #multiplying variables from B (pairwise interactions) and A in order to obtain three-way interactions
    
  }
  
  B
  
}

#' Finding the interacting terms based on the index
#'
#' Function transforms an index of an array of two- or three-way interactions into two or three indices corresponding to the interacting variables
#'@param ind index to transform
#'@param N number of interacting variables
#'@return Returns two or three indices corredsponding to a combination of variables written under the given index
#'@export find_int
#'@usage find_int(ind, N)
#'@examples find_int(28,9)


find_int<-function(ind,N){
  
  if (ind<=N){ #if the index is lower than the number of variables
    
    ind #it is just a single variable
    
  } else{
    
    if (ind<=(N+0.5*(N+1)*N)){ #if the index is smaller than the number of all variables, their squares and two-way interactions
      
      a<-2*N #starting point is interactions with the first variables
      
      i<-1
      
      while (ind>a){  #locating the index by adding the number of possible interacting variables
        
        a=a+N-i
        
        i=i+1
      }
      
      c(i,ind-a+N) #output two interacting variables
      
    } else{ #in case the interaction goes beyond a pair of variables
      
      
      a<-N+0.5*(N+1)*N #starting point two-way interactions
      
      i<-0 
      
      while (ind>a){ #locating the index by adding the number of possible interacting variables taking into account three-way interactions
        
        a=a+0.5*(N-i+1)*(N-i)
        
        i=i+1
      }
      
      ind1<-i #the index of the first interacting variable
      
      ind2<-ind-a+0.5*(N-i+2)*(N-i+1) #reducing the problem to finding two interacting variables
      
      a<-N-i+1 #similar to the two-way interaction case but taking into account the number of the first interacting variable
      
      
      while (ind2>a){ #similarly to two-way case locating two variables interacting with each other
        
        a=a+N-i
        
        i=i+1
      }
      
      c(ind1,i,ind2-a+N) #outputting three interacting variables
      
    }
    
    
  }
  
}


