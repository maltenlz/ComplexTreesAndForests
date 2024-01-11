library(dplyr)
library(sampling)
library(rpart)
library(rpart.plot)
data <- read.csv("D:/my papers/Weighted-Tree-Forest/housing_data.csv")
dataused = data[,c(11, 12, 13, 14, 18)]
colnames(dataused) = c("YrSold", "SalePrice", "sqm", "lotsqm", "YearBuilt")
dataused$YearBuilt = as.numeric(dataused$YearBuilt)
dataused$YrSold = as.numeric(substr(dataused$YrSold, 1,4))

dataused = dataused %>% filter(YearBuilt > 0) %>% drop_na() %>% distinct()

n = 1000

set.seed(12345) 

dataused$u = rnorm(nrow(dataused), dataused$SalePrice, 10000)
pi = inclusionprobabilities(dataused$u, n)
sampleind <- UPrandompivotal(pik = pi)
train_data = dataused[sampleind == 1,] %>% dplyr::select(-c("u"))
pop_data = dataused %>% dplyr::select(-c("u"))
weights = pi[sampleind == 1]


(corrected_part1 = rpart(SalePrice~.,
                         weights = ((1/weights)/sum(1/weights))*n,
                         data    = train_data,
                         control = rpart.control(maxdepth = 2)))


(sample_part1    = rpart(SalePrice~.,
                         data = train_data,
                         control = rpart.control(maxdepth = 2))
)

(pop_part        = rpart(SalePrice~.,
                         data = pop_data, control = rpart.control(maxdepth =2))
)
prp(corrected_part1, yesno = 0)
prp(sample_part1, yesno = 0)
prp(pop_part, yesno = 0)
