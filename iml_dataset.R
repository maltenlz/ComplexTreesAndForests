library(dplyr)
library(sampling)
library(iml)
library(ggplot2)
library(tidyr)
source("functionsRefactored.R")

# Calculates PDPs (Partial Dependence Plots) for different methods given a dataset
# Housing data downloaded from https://www.data.go.kr/en/data/15052419/fileData.do
# can also be used on other datasets: However the y variable needs to be adjusted in the code accordingly

# sample size to be used
n = 100

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

calculate_PDP = function(dataused, feature, n){
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
  
  van_ranger = ranger::ranger(formula = SalePrice ~ ., dependent.variable.name = "SalePrice",
                              data = train_data
                              #max.depth = 6
                              )
  
  
  van_ranger_srs = ranger::ranger(formula = SalePrice ~ ., dependent.variable.name = "SalePrice",
                                  data = train_data_srs
                                  #max.depth = 6
                                  )
  
  
  hj = hajek_forest(X = as.matrix(train_data %>% dplyr::select(-c("SalePrice"))),
                    y = train_data[,"SalePrice"],
                    w = n*(1/weights)/sum(1/weights),
                    minobs = 0.001,
                    mtry = floor(sqrt(ncol(train_data)-1))
  )
  hj_reg = hajek_forest(X = as.matrix(train_data %>% dplyr::select(-c("SalePrice"))),
                    y = train_data[,"SalePrice"],
                    w = n*(1/weights)/sum(1/weights),
                    minobs = 0.02,
                    mtry = floor(sqrt(ncol(train_data)-1))
  )
  #pop_ranger = ranger::ranger(formula = SalePrice ~ ., dependent.variable.name = "SalePrice",
  #                            data = pop_data)
  
  predictor <- Predictor$new(wb_ranger, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  ice_pdp_wb <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
  
  predictor <- Predictor$new(van_ranger, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  ice_pdp_van <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
  
  predictor <- Predictor$new(van_ranger_srs, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun) 
  ice_pdp_van_srs <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
  
  predictor <- Predictor$new(hj, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun_hj)
  ice_pdp_hj <- FeatureEffect$new(predictor, feature = feature, method = "pdp")
  
  predictor <- Predictor$new(hj_reg, data = test_data %>% dplyr::select(-c("SalePrice")), y = test_data$SalePrice, predict.function = pred_fun_hj)
  ice_pdp_hj_reg <- FeatureEffect$new(predictor, feature = feature, method = "pdp")

  # combine plots
  return(rbind(cbind("method" = rep("wb", nrow(ice_pdp_wb$results)), ice_pdp_wb$results), 
               cbind("method" = rep("van", nrow(ice_pdp_van$results)), ice_pdp_van$results),
               cbind("method" = rep("van_srs", nrow(ice_pdp_van_srs$results)), ice_pdp_van_srs$results),
               cbind("method" = rep("hj", nrow(ice_pdp_hj$results)), ice_pdp_hj$results),
               cbind("method" = rep("hj_reg", nrow(ice_pdp_hj_reg$results)), ice_pdp_hj_reg$results)
               )
  )
  
}

calculate_PDP_rep = function(dataused, feature, n, nreps){
  outls = list()
  for(i in 1:nreps){
    pdp_out = calculate_PDP(dataused, feature, n)[,1:3]
    colnames(pdp_out) =c('method', "x", 'pdp')
    outls[[i]] = cbind(variable = feature, pdp_out)
  }
  return(do.call("rbind", outls))
}

n = 100

YrBuiltdpdp = calculate_PDP_rep(dataused, "YearBuilt", n, 100)
YrSoldpdp = calculate_PDP_rep(dataused, "YrSold", n, 100)
sqfpdp = calculate_PDP_rep(dataused, "sqm", n, 100)
lotsqfpdp = calculate_PDP_rep(dataused, "lotsqm", n, 100)

ggframe = rbind(YrSoldpdp,
                YrBuiltdpdp,
                sqfpdp,
                lotsqfpdp
                )

ggplot(data = ggframe , aes(x, pdp, color = method, linetype = method)) +
       geom_smooth(se = FALSE) + facet_wrap(variable ~ ., scales = 'free') + theme_bw()
