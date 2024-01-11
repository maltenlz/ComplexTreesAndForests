# Defines the simulation Settings to be run

source("simulation.R")
set.seed(1234)
simulation_settings = expand.grid(skewness = c(0.01, 0.5, 1, 2), train_size = c(50, 100, 250, 500, 1000), scenario = c("skewed_y", "skewed_x"))

for (i in 1:nrow(simulation_settings)){
  run_simulation(train_size = simulation_settings$train_size[i],
                 skewness = simulation_settings$skewness[i],
                 scenario = simulation_settings$scenario[i],
                 runs = 100)
}
