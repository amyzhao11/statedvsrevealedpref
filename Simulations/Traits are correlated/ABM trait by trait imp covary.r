library(lme4)
library(lmerTest)
library(parallel)
library(foreach)
library(doParallel)
library(stringr)

setwd(".../Data and code/Requested by reviewers/Traits are correlated")
#171 speed-dating sessions from real data

rm(list=ls(all=TRUE)) #remove all saved variables in workspace

packages <- c("parallel", "foreach", "doParallel","stringr","lme4","lmerTest")
num_sims <- 1000 #This is the number of simulations I want, I’m going to ask foreach to run through 1:num_sims in parallel

registerDoParallel(min(detectCores()-1,num_sims)) #This establishes how many cores to use (it will use the minimum of either the total number of cores for this computer – 1, or the total number of sims)



noisetested=c(0,5,10,20,30,40,50)
#num=1000 #number of simulations
load(".../Data and code/Simulations/ABM core data 171 traits 25 .RData")


#preprocess data for storage reasons
for (i in 1:1000){
  data[[i]]<-subset(data[[i]],select=c("participantID",paste0("importance ", 1:9),
                                       "partnerID","sessionID","interact",paste0("rating ", 1:9)))
}

?chol

impcor=read.csv(".../Data and code/Requested by reviewers/Traits are correlated/Importance correlation matrix.csv")
ratcor=read.csv(".../Data and code/Requested by reviewers/Traits are correlated/Rating correlation matrix.csv")

rownames(impcor)=impcor[,1]
impcor=impcor[,2:10]
impcor[lower.tri(impcor)]

rownames(ratcor)=ratcor[,1]
ratcor=ratcor[,2:10]
ratcor[lower.tri(ratcor)]



head(data[[1]])
impcor_chol=chol(impcor)
t(impcor_chol) %*% impcor_chol

ratcor_chol=chol(ratcor)
t(ratcor_chol) %*% ratcor_chol
#check that it is equal to original matrix

impcor_chol

#Just do this for 9 traits - because this is specific to the traits we have. 
#These traits aren't independent so can't just remove any trait
library(scales)
for (i in 1:1000){
  data[[i]][,paste0("importance ", 1:9)] = as.matrix(data[[i]][,paste0("importance ", 1:9)]) %*% impcor_chol
  data[[i]][,paste0("importance ", 1:9)] =round(apply(data[[i]][,paste0("importance ", 1:9)],2,rescale, to=c(1,7)))
  data[[i]][,paste0("rating ", 1:9)] = as.matrix(data[[i]][,paste0("rating ", 1:9)]) %*% ratcor_chol
  data[[i]][,paste0("rating ", 1:9)] = round(apply(data[[i]][,paste0("rating ", 1:9)],2,rescale, to=c(1,7)))
}
impcor
data[[1]][,paste0("importance ", 1:9)] = as.matrix(data[[1]][,paste0("importance ", 1:9)]) %*% impcor_chol
cor(data[[1]][,paste0("importance ", 1:9)])
cor(data[[]][,paste0("rating ", 1:9)])





calc <- function(a,noise,test=num_sims) {
  options(digits=10)

  saved=list()
  #for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
  
  #for 9 traits
  k=9
  
    # Pre-allocate the data frame to store the estimates and p-values
    saved[[paste0(k)]]<- data.frame(estimate = numeric(test), p = numeric(test))
  #}
  
  # Calculate trait appeal and attractiveness for each test case
  for (i in 1:test) {
    set.seed(i)
    a_i <- a[[i]]
    nint <- nrow(a_i)
    #variable names have spaces, remove them
    names(a_i)<-str_replace_all(names(a_i), c(" " = ""))
    

    
    #for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
      # Pre-allocate the data frame to store the estimates and p-values
      
      
      # Attractiveness calculated by trait appeal
      a_i[[paste0("attr", k)]] <- rowMeans(a_i[,paste0("importance", 1:k)]*(a_i[,paste0("rating", 1:k)]-4)+noise*matrix( rnorm(k*nint,mean=0,sd=1), nint, k))
      
      # Fit the linear mixed-effects model for each trait parameter
      model <- lmer(scale(get(paste0("attr", k))) ~ 1 +
                      scale(importance1)*scale(rating1) +
                      #check real analyses
                      (1 | sessionID) +
                      (1 | participantID) +
                      (1 | partnerID)+
                      (rating1| participantID),
                    data = a_i)
      
      saved[[paste0(k)]]$estimate[i] <- summary(model)$coefficients[4, 1]
      saved[[paste0(k)]]$p[i]<-summary(model)$coefficients[4, 5]
    #}
    
    
    a[[i]] <- a_i
  }
  #for (k in c(2, 3, 5, 9, 10, 15, 20, 25)) {
    write.csv(saved[[paste0(k)]], file = paste0("Traitbytrait imp 171 traits covary", deparse(k), " noise ", deparse(noise), ".csv"))
  #}
  return(saved)
}

##################################################################################################
#run simulations

foreach (noisy = noisetested, .packages = packages) %dopar% {
  #data=vector(mode = "list", length = num)
  calc(data,noisy)
}


