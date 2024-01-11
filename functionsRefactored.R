#####
library(rpart)
library(xgboost)


mse = function(predictions, ground_truth){
  checkmate::assert_vector(predictions)
  checkmate::assert_vector(ground_truth)
  assertthat::assert_that(length(predictions) == length(ground_truth))
  mean((predictions - ground_truth)^2)
}

errorRFweighted = function(train_data, test_data){
  weights = train_data$pi
  train_data = select(train_data, -pi)
  fitted_rf = ranger::ranger(formula = y ~ ., dependent.variable.name = y, 
                             data = train_data, case.weights = 1/weights)
  pred = predict(fitted_rf, data = test_data)
  predictions = pred$predictions
  return(mse(predictions, test_data$y))
}

errorRFplain = function(train_data, test_data){
  train_data = select(train_data, -pi)
  fitted_rf = ranger::ranger(formula = y ~ ., dependent.variable.name = y, 
                             data = train_data)
  pred = predict(fitted_rf, data = test_data)
  predictions = pred$predictions
  return(mse(predictions, test_data$y))
}

errorHajekApproach = function(train_data, test_data, minobs = 0){
  X = select(train_data, -c(pi, y))
  y = train_data$y
  Xtest = select(test_data, -y)
  weights_to_pass = nrow(train_data)*(1/train_data$pi)/sum(1/train_data$pi)
  mod = hajek_forest(X = X, y = y, w = weights_to_pass, ntree = 500, mtry = floor(sqrt(ncol(X))), minobs = minobs)
  pred = predict(mod, Xtest)
  mse(pred, test_data$y)
}


hajek_forest = function(X,
                        y, 
                        w, 
                        ntree = 500,
                        mtry = 2,
                        minobs,
                        maxdepth= 128
                        ){
  n = nrow(X)
  p = ncol(X)
  param <- list(max_depth = maxdepth,
                eta = 1,
                objective = "reg:squarederror",
                colsample_bynode = mtry/p,
                min_child_weight = max(1, n*minobs),
                lambda = 0
  )
  
  forest = list()
  for (tree in 1:ntree){
    ### take bootstrap sample
    ids    = sample(1:n, replace = T, size= nrow(X))
    dtrain = xgb.DMatrix(as.matrix(X[ids,]),
                         label = y[ids], 
                         weight = w[ids])
    
    ### build de-biased tree on bootstrapped sample
    forest[[tree]] =   mod = xgb.train(
                                       params = param,
                                       data= dtrain,
                                       nrounds = 1,
                                       verbose = F
                                       )
  }
  out = list(forest = forest, X=X)
  class(out) = "hajek_forest"
  out
}


predict.hajek_forest = function(mod,
                                Xnew = NULL){
  if (is.null(Xnew)){
    Xnew  = as.matrix(mod$X)
  } else {
    Xnew  = as.matrix(Xnew)
  }
  forest = mod$forest
  ntree  = length(forest)
  ytree  = matrix(0, nrow = nrow(Xnew), ncol = ntree)
  for (tree in 1:ntree){
    ytree[,tree] = predict(forest[[tree]], Xnew)
  }
  as.numeric(apply(ytree, 1, mean))
}

takePPSsample = function(data, train_size){
  ind = 1:nrow(data)
  train_ind = sample(ind,train_size, prob = data$pi)
  return(data[train_ind,]) 
}

takeSRSsample = function(data, train_size){
  ind = 1:nrow(data)
  train_ind = sample(ind,train_size)
  return(data[train_ind,]) 
}