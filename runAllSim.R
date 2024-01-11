source("simulation.R")
set.seed(1234)
simulation_settings = expand.grid(skewness = c(0.01, 0.5, 1, 2), train_size = c(50, 100, 250, 500, 1000))

for (i in 1:nrow(simulation_settings)){
  run_simulation(train_size = simulation_settings$train_size[i],
                 skewness = simulation_settings$skewness[i],
                 runs = 100)
}
