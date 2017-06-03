#functions to access coefficient estimate and SD
avg_est <- function(x,i){
  return (x[i,1])
}
avg_sd <- function(x,i){
  return (x[i,2])
}

#create lists for storage
estimate = rep(NA, 20)
c_list = list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20)

sdcoef = rep(NA,20)
sdlist = rep(NA,12)
sd_coefs = rep(NA,12)

#find avg of coefficient estimates (loop for 12 coefficients)
for (i in 1:12){
  estimate <- sapply(c_list, avg_est, i)
  mean_est[i] <- mean(estimate)
}

#find SD of coefficient estimates
for (i in 1:12){
  estimate <- sapply(c_list, avg_est, i)
  sdlist[i] <- sd(estimate)
}

#find avg of coefficient SDs
for (i in 1:12){
  sdcoef <- sapply(c_list, avg_sd, i)
  sd_coefs[i] <- mean(sdcoef)
  
}

#find final adjusted SD
adj_sd_coefs = sqrt(sd_coefs^2+sdlist^2)

#find t-value
tval = mean_est / adj_sd_coefs

#find p-value with df = 155
pval <- function(x){
  if (x>0){
    return (2*(1-pt(x, 155)))
  }
  else{
    return(2*pt(x,155))
  }
}
pvalues <- sapply(tval, pval)
pvalues

cbind(mean_est, adj_sd_coefs, tval, pvalues)
