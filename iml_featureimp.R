library(dplyr)
library(sampling)
library(iml)
library(ggplot2)
library(tidyr)
source("functionsRefactored.R")
#data <- read.csv("D:/my papers/Weighted-Tree-Forest/Daegu_Real_Estate_data.csv")
data <- read.csv("D:/my papers/Weighted-Tree-Forest/housing_data.csv")
dataused = data[,c(11, 12, 13, 14, 18)]
colnames(dataused) = c("YrSold", "SalePrice", "sqm", "lotsqm", "YearBuilt")
dataused$YearBuilt = as.numeric(dataused$YearBuilt)
dataused$YrSold = as.numeric(substr(dataused$YrSold, 1,4))

dataused = dataused %>% filter(YearBuilt > 0) %>% drop_na() %>% distinct()

pred_fun <- function(X.model, newdata) {
  predict(X.model, newdata)$predictions
}

pred_fun_hj <- function(X.model, newdata) {
  predict(X.model, as.matrix(newdata))
}

calculate_PermutationIMP = function(dataused, n){
  dataused$u = rnorm(nrow(dataused), dataused$SalePrice, 10000)
  
  test_ids = sample(1:nrow(dataused), n)
  test_data = dataused[test_ids,] %>% dplyr::select(-c("u"))
  
  dataused = dataused[-test_ids,]
  pi = inclusionprobabilities(dataused$u, n)
  sampleind <- UPrandompivotal(pik = pi)
  train_data = dataused[sampleind == 1,] %>% dplyr::select(-c("u"))
  pop_data = dataused %>% dplyr::select(-c("u"))
  weights = pi[sampleind == 1]
  
  train_data_srs = dataused[sample(1:nrow(dataused), n),] %>% dplyr::select(-c("u"))
  
  wb_ranger = ranger::ranger(formula = SalePrice ~ .,
                             dependent.variable.name = "SalePrice", 
                             data = train_data,
                             case.weights = 1/weights
                             #max.depth = 6
  )
  van_ranger = ranger::ranger(formula = SalePrice ~ .,
                              dependent.variable.name = "SalePrice",
                              data = train_data
                              #max.depth = 6
  )
  
  
  van_ranger_srs = ranger::ranger(formula = SalePrice ~ .,
                                  dependent.variable.name = "SalePrice",
                                  data = train_data_srs
                                  #max.depth = 6
  )
  
  
  hj = hajek_forest(X = as.matrix(train_data %>% dplyr::select(-c("SalePrice"))),
                    y = train_data[,"SalePrice"],
                    w = n*(1/weights)/sum(1/weights),
                    minobs = 0.001,
                    mtry = floor(sqrt(ncol(train_data-1))),
                    #maxdepth = 12
  )
  hj_reg = hajek_forest(X = as.matrix(train_data %>% dplyr::select(-c("SalePrice"))),
                    y = train_data[,"SalePrice"],
                    w = n*(1/weights)/sum(1/weights),
                    minobs = 0.02,
                    mtry = floor(sqrt(ncol(train_data-1))),
                    #maxdepth = 12
  )
  
  mse_van = mse(predict(van_ranger, test_data)$predictions, test_data$SalePrice)
  mse_srs = mse(predict(van_ranger_srs, test_data)$predictions, test_data$SalePrice)
  mse_wb = mse(predict(wb_ranger, test_data)$predictions, test_data$SalePrice)
  mse_hj = mse(predict(hj, test_data %>% dplyr::select(-c("SalePrice"))), test_data$SalePrice)
  mse_hj_reg = mse(predict(hj_reg, test_data %>% dplyr::select(-c("SalePrice"))), test_data$SalePrice)
  
  #pop_ranger = ranger::ranger(formula = SalePrice ~ ., dependent.variable.name = "SalePrice",
  #                            data = pop_data)
  
  predictor <- Predictor$new(wb_ranger, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  imp_wb <- FeatureImp$new(predictor,
                           loss = "mse",
                           compare = "ratio",
                           n.repetitions = 20)
  
  predictor <- Predictor$new(van_ranger, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  imp_van <- FeatureImp$new(predictor,
                            loss = "mse",
                            compare = "ratio",
                            n.repetitions = 20)
  
  predictor <- Predictor$new(van_ranger_srs, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  imp_srs <- FeatureImp$new(predictor,
                            loss = "mse",
                            compare = "ratio",
                            n.repetitions = 20)
  
  predictor <- Predictor$new(hj, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun_hj)
  imp_hj <- FeatureImp$new(predictor,
                           loss = "mse",
                           compare = "ratio",
                           n.repetitions = 20)
  
  predictor <- Predictor$new(hj_reg, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun_hj)
  imp_hj_reg <- FeatureImp$new(predictor,
                           loss = "mse",
                           compare = "ratio",
                           n.repetitions = 20)
  
  # combine plots
  return(list(rbind(cbind("method" = rep("wb", nrow(imp_wb$results)), imp_wb$results), 
               cbind("method" = rep("van", nrow(imp_van$results)), imp_van$results),
               cbind("method" = rep("van_srs", nrow(imp_srs$results)), imp_srs$results),
               cbind("method" = rep("hj", nrow(imp_hj$results)), imp_hj$results),
               cbind("method" = rep("hj_reg", nrow(imp_hj_reg$results)), imp_hj$results)
               
               ),
              rbind(data.frame(method = "wb", feature = "mserel", importance = mse_wb/mse_srs), 
                    data.frame(method = "van", feature = "mserel",importance =  mse_van/mse_srs),
                    data.frame(method = "van_srs", feature = "mserel" , importance = mse_srs/mse_srs),
                    data.frame(method = "hj", feature = "mserel", importance = mse_hj/mse_srs),
                    data.frame(method = "hj_reg", feature = "mserel", importance = mse_hj_reg/mse_srs)
                    
              )
  )
  )
}

calculate_PermutationIMP_rep = function(dataused,  n, nreps){
  outls = list()
  for(i in 1:nreps){
    
    imp_out = calculate_PermutationIMP(dataused,  n)
    
    outls[[i]] = rbind(imp_out[[1]][,c('method', 'feature', 'importance')],
                       imp_out[[2]]
                       )
  }
  return(do.call("rbind", outls))
}

ggframeIMP =calculate_PermutationIMP_rep(dataused = dataused, n = 100, 100)
write.csv2(ggframeIMP, "importance_sim.csv")

ggframeIMP_barplot = ggframeIMP %>% filter(feature != "mserel") %>% group_by(method, feature) %>% summarise(sd_importance = sd(importance),
                                                                                                            importance = mean(importance)
                                                                                                            )

ggframeIMP %>% filter(feature == "mserel") %>% group_by(method, feature) %>% summarise(importance = mean(importance))

ggplot(ggframeIMP_barplot) +
  geom_bar( aes(x=feature,  y=importance, fill=method),
            position =position_dodge() ,
            stat="identity",
            alpha=0.7) +
  geom_errorbar(aes(x=feature, fill=method, ymin=importance-sd_importance, ymax=importance+sd_importance),
                position = position_dodge(.9), 
                width = 0.4, 
                colour = "orange",
                alpha=0.9,
                size=1.3
                )

