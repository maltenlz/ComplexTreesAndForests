#set.seed(171122)
library(dplyr)
source("functionsRefactored.R")
library(sn)
library(sampling)

truncate_target = function(y, ymax = 1000000){
  y[y > ymax] = ymax
  return(y)
}

safeapply = function(x){
  if(!is.vector(x)){
    return(t(apply(x, 1, sum)))
  } else {
    return(x)
  }
}

simulate_data_outcome  = function(N =10000, p = 1, p_0 = 5, epsilon = 1, skewness = 1){
  x = matrix(0, ncol =p+p_0, nrow = N)
  for(i in 1:(p+p_0)){
    x[,i] = rnorm(N, 0, 1)
  }
  lin_pred = as.numeric(safeapply(x[,1:p]))

  y = rlnorm(N, lin_pred, skewness)
  #y = truncate_target(y)
  
  u = y + rnorm(N, 0, epsilon)
  u[u<=0] = 0.01
  #u = y
  pi = u/sum(u)
  data.frame(y, x, pi = u)
}

simulate_data_skewed  = function(N =10000, p = 5, p_0 = 5, epsilon = 1, skewness = 1){
  x = matrix(0, ncol =p+p_0, nrow = N)
  for(i in 1:(p+p_0)){
    x[,i] = rlnorm(N, 1, skewness)
  }
  lin_pred = as.numeric(safeapply(x[,1:p]))
  y = rnorm(N, lin_pred, 5)
  #y = truncate_target(y)
  u = y + rnorm(N, 0, epsilon)
  u[u<=0] = 0.01
  pi = u/sum(u)
  data.frame(y, x, pi = pi)
}

run_simulation = function(train_size, runs = 10, epsilon = 1, skewness = 0){

  pb = txtProgressBar(min = 1, max = runs, initial = 1)
  
  res_rf_PPS = c()
  res_rf_weights = c()
  res_rf_hajek = c()
  res_rf_hajek_van = c()
  res_rf_SRS = c()
  
  for (run in 1:runs) {
    
    ### Make the population size dynamic in order to lower finite population effects
    N = train_size*1000
    
    ### Generate train and test data under PPS sampling (without replacement)
    train_population = simulate_data_outcome(N = N, p = 5, p_0 = 5, epsilon = epsilon, skewness = skewness)
    train_sample = takePPSsample(train_population, train_size)
    test_data = simulate_data_outcome(N = 1000, p = 5, p_0 = 5, epsilon = epsilon, skewness = skewness)
    test_data = select(test_data, -pi)
    
    ### Run different approaches on the PPS sampled data
    res_rf_weights[run] = errorRFweighted(train_sample, test_data)
    res_rf_PPS[run] = errorRFplain(train_sample, test_data)
    res_rf_hajek[run] = errorHajekApproach(train_sample, test_data, minobs = 0.02)
    res_rf_hajek_van[run] = errorHajekApproach(train_sample, test_data, minobs = 0)
    
    ### Run plain random forest on a SRS
    train_sample_SRS = takeSRSsample(train_population, train_size)
    res_rf_SRS[run] = errorRFplain(train_sample_SRS, test_data)
    
    setTxtProgressBar(pb,run)
  }
  outframe = rbind(data.frame(method = "Weighted Bootstrap", n = train_size, skewness = skewness, fold = 1:runs, error = res_rf_weights),
                   data.frame(method = "Vanilla PPS", n = train_size, skewness = skewness, fold = 1:runs, error = res_rf_PPS),
                   data.frame(method = "Hajek Forest", n = train_size,skewness = skewness, fold = 1:runs, error = res_rf_hajek),
                   data.frame(method = "Hajek Forest vanilla", n = train_size,skewness = skewness, fold = 1:runs, error = res_rf_hajek_van),
                   data.frame(method = "Vanilla SRS", n = train_size, skewness = skewness, fold = 1:runs, error = res_rf_SRS))
  write.csv2(outframe, paste0("simulation_rf_outcome/simulation n =", train_size, ", skewness = ", skewness, ".csv"))
}

