# CMEE 2022 HPC exercises R code main pro forma
# You don't HAVE to use this but it will be very helpful.
# If you opt to write everything yourself from scratch please ensure you use
# EXACTLY the same function and parameter names and beware that you may lose
# marks if it doesn't work properly because of not using the pro-forma.

name <- "Yijie Jiao"
preferred_name <- "Yijie Jiao"
email <- "yj2323@imperial.ac.uk"
username <- "yj2323"

# Please remember *not* to clear the workspace here, or anywhere in this file.
# If you do, it'll wipe out your username information that you entered just
# above, and when you use this file as a 'toolbox' as intended it'll also wipe
# away everything you're doing outside of the toolbox.  For example, it would
# wipe away any automarking code that may be running and that would be annoying!

# Question 1
species_richness <- function(community){
  unique_species <- unique(community)
  return(length(unique_species))
}
#species_richness(c(1,4,4,5,1,6,1))

# Question 2
init_community_max <- function(size){
  return(seq(1, size))
}

# Question 3
init_community_min <- function(size){
  return(rep(1, size))
}

# Question 4
choose_two <- function(max_value){
  first_number <- sample(1:max_value, 1)
  second_number <- sample(setdiff(1:max_value, first_number), 1)
  return(c(first_number, second_number))
}

# Question 5
neutral_step <- function(community){
  indexes <- choose_two(length(community))
  dead_index <- indexes[1]
  reproduce_index <- indexes[2]
  community[dead_index] <- community[reproduce_index]
  return(community)
}

# Question 6
neutral_generation <- function(community){
  size <- length(community)
  steps <- size %/% 2 + sample(c(0, 1), 1) * (size %% 2)
  for (i in 1:steps) {
    community <- neutral_step(community)
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration)  {
  richness_series <- numeric(duration + 1)
  richness_series[1] <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation(community)
    richness_series[i + 1] <- species_richness(community)
  }
  return(richness_series)
}

# Question 8
question_8 <- function() {
  community <- init_community_max(100)
  duration <- 200
  richness_series <- neutral_time_series(community, duration)
  
  
  png(filename="question_8.png", width = 600, height = 400)
  # plot your graph here
  plot(richness_series, type = "l", xlab = "Generation", ylab = "Species Richness",
       main = "Neutral Model Simulation over 200 Generations")
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}
#question_8()

# Question 9
neutral_step_speciation <- function(community,speciation_rate)  {
  indexes <- choose_two(length(community))
  dead_index <- indexes[1]
  reproduce_index <- indexes[2]
  
  if (runif(1) < speciation_rate) {
    # Assign a unique species number
    new_species <- max(community) + 1
    community[dead_index] <- new_species
  } else {
    community[dead_index] <- community[reproduce_index]
  }
  
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  size <- length(community)
  steps <- size %/% 2 + sample(c(0, 1), 1) * (size %% 2)
  for (i in 1:steps) {
    community <- neutral_step_speciation(community, speciation_rate)
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  richness_series <- numeric(duration + 1)
  richness_series[1] <- species_richness(community)
  for (i in 1:duration) {
    community <- neutral_generation_speciation(community, speciation_rate)
    richness_series[i + 1] <- species_richness(community)
  }
  return(richness_series)
}

# Question 12
question_12 <- function()  {
  speciation_rate <- 0.1
  community_size <- 100
  duration <- 200
  
  community_max <- init_community_max(community_size)
  community_min <- init_community_min(community_size)
  
  richness_series_max <- neutral_time_series_speciation(community_max, duration, speciation_rate)
  richness_series_min <- neutral_time_series_speciation(community_min, duration, speciation_rate)
  
  
  
  png(filename="question_12.png", width = 600, height = 400)
  # plot your graph here
  plot(richness_series_max, type = "l", col = "blue", xlab = "Generation", ylab = "Species Richness",
       main = "Neutral Model Simulation over 200 Generations with Speciation Rate 0.1")
  lines(richness_series_min, type = "l", col = "red")
  legend("topright", legend = c("Max Diversity", "Min Diversity"), col = c("blue", "red"), lty = 1)
  dev.off()
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 13
species_abundance <- function(community)  {
  abundance <- table(community)
  sorted_abundance <- sort(abundance, decreasing = TRUE)
  return(sorted_abundance)
}

# Question 14
octaves <- function(abundance_vector) {
  if (length(abundance_vector) == 0) {
    return(integer(0))
  }
  octave_classes <- floor(log2(abundance_vector)) + 1
  max_octave <- max(octave_classes)
  octave_counts <- tabulate(octave_classes, nbins = max_octave)
  return(octave_counts)
}

# Question 15
sum_vect <- function(x, y) {
  len_diff <- abs(length(x) - length(y))
  if (length(x) < length(y)) {
    x <- c(x, rep(0, len_diff))
  } else {
    y <- c(y, rep(0, len_diff))
  }
  return(x + y)
}

# Question 16 
question_16 <- function() {
  speciation_rate <- 0.1
  community_size <- 100
  burn_in_generations <- 200
  simulation_duration <- 2000
  record_frequency <- 20
  
  # Function to run the simulation and collect octave data
  run_simulation <- function(initial_community) {
    community <- initial_community
    octave_list <- list()
    for (i in 1:(burn_in_generations + simulation_duration)) {
      community <- neutral_generation_speciation(community, speciation_rate)
      if (i > burn_in_generations && i %% record_frequency == 0) {
        abundance_vector <- species_abundance(community)
        octave_list[[length(octave_list) + 1]] <- octaves(abundance_vector)
      }
    }
    # Adjusting octave lists to have the same length before averaging
    max_length <- max(sapply(octave_list, length))
    adjusted_octave_list <- lapply(octave_list, function(o) {c(o, rep(0, max_length - length(o)))})
    mean_octaves <- colMeans(do.call(rbind, adjusted_octave_list), na.rm = TRUE)
    return(mean_octaves)
  }
  
  # Running simulations for both initial conditions
  mean_octaves_max <- run_simulation(init_community_max(community_size))
  mean_octaves_min <- run_simulation(init_community_min(community_size))
  
  png(filename="question_16_min.png", width = 600, height = 400)
  # plot your graph here
  barplot(mean_octaves_max, main = "Mean Species Abundance Distribution (Max Initial Condition)",
          xlab = "Octave", ylab = "Mean Abundance")
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_16_max.png", width = 600, height = 400)
  # plot your graph here
  barplot(mean_octaves_min, main = "Mean Species Abundance Distribution (Min Initial Condition)",
          xlab = "Octave", ylab = "Mean Abundance")
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}
#question_16()

# Question 17
neutral_cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name) {
  start_time <- proc.time()[3]
  community <- init_community_min(size)
  time_series <- numeric()
  abundance_list <- list()
  generation <- 0
  
  while (proc.time()[3] - start_time < wall_time * 60) {
    community <- neutral_generation_speciation(community, speciation_rate)
    generation <- generation + 1
    
    # Record species richness during burn-in period
    if (generation <= burn_in_generations && generation %% interval_rich == 0) {
      time_series <- c(time_series, species_richness(community))
    }
    
    # Record species abundances as octaves
    if (generation %% interval_oct == 0) {
      abundance_vector <- species_abundance(community)
      abundance_list[[length(abundance_list) + 1]] <- octaves(abundance_vector)
    }
  }
  
  total_time <- proc.time()[3] - start_time
  
  # Save the simulation results
  save(time_series, abundance_list, community, total_time, speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, file = output_file_name)
}


# Questions 18 and 19 involve writing code elsewhere to run your simulations on
# the cluster

# Question 20 
process_neutral_cluster_results <- function() {
  
  
  combined_results <- list() #create your list output here to return
  # save results to an .rda file
  
}

plot_neutral_cluster_results <- function(){

    # load combined_results from your rda file
  
  
  
    png(filename="plot_neutral_cluster_results", width = 600, height = 400)
    # plot your graph here
    Sys.sleep(0.1)
    dev.off()
    
    return(combined_results)
}


# Question 21
state_initialise_adult <- function(num_stages, initial_size) {
  state_vector <- rep(0, num_stages)
  state_vector[num_stages] <- initial_size
  return(state_vector)
}


# Question 22
state_initialise_spread <- function(num_stages, initial_size) {
  base_allocation <- floor(initial_size / num_stages)
  additional_individuals <- initial_size %% num_stages
  state_vector <- rep(base_allocation, num_stages)
  state_vector[1:additional_individuals] <- state_vector[1:additional_individuals] + 1
  return(state_vector)
}


# Question 23
deterministic_step <- function(state, projection_matrix) {
  new_state <- as.vector(projection_matrix %*% state)
  return(new_state)
}


# Question 24
deterministic_simulation <- function(initial_state, projection_matrix, simulation_length) {
  population_size <- numeric(simulation_length + 1)
  population_size[1] <- sum(initial_state)
  state <- initial_state
  
  for (i in 1:simulation_length) {
    state <- deterministic_step(state, projection_matrix)
    population_size[i + 1] <- sum(state)
  }
  
  return(population_size)
}


# Question 25
question_25 <- function() {
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4), 
                          nrow=4, ncol=4, byrow=TRUE)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0), 
                                nrow=4, ncol=4, byrow=TRUE)
  projection_matrix <- reproduction_matrix + growth_matrix
  
  # Initial conditions
  initial_condition_adults <- c(0, 0, 0, 100)
  initial_condition_spread <- c(25, 25, 25, 25)
  
  # Run deterministic simulations
  simulation_length <- 24
  population_adults <- deterministic_simulation(initial_condition_adults, projection_matrix, simulation_length)
  population_spread <- deterministic_simulation(initial_condition_spread, projection_matrix, simulation_length)
  
  
  png(filename="question_25.png", width = 600, height = 400)
  # plot your graph here
  plot(population_adults, type = 'l', col = 'blue', ylim = range(c(population_adults, population_spread)),
       xlab = "Time Step", ylab = "Total Population Size", main = "Population Size Time Series")
  lines(population_spread, type = 'l', col = 'red')
  legend("topright", legend = c("100 Adults", "Spread Across Stages"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}
question_25
# Question 26
multinomial <- function(pool, probs) {
  # Ensure the probabilities sum up to 1
  if (sum(probs) < 1) {
    probs <- c(probs, 1 - sum(probs)) # Account for the implicit probability of death
  }
  
  # Draw from the multinomial distribution
  outcomes <- rmultinom(1, pool, probs)
  
  return(as.vector(outcomes))
}

# Question 27
survival_maturation <- function(state, growth_matrix) {
  new_state <- rep(0, length(state))
  
  for (i in 1:length(state)) {
    current_individuals <- state[i]
    probs <- growth_matrix[i,]
    
    # Generating the number of individuals transitioning to other stages
    transitions <- multinomial(current_individuals, probs)
    
    # Adding results for each life stage to new_state
    new_state <- new_state + transitions
  }
  
  return(new_state)
}


# Question 28
random_draw <- function(probability_distribution) {
  values <- 1:length(probability_distribution)
  drawn_value <- sample(values, size = 1, prob = probability_distribution)
  return(drawn_value)
}

# Question 29
stochastic_recruitment <- function(reproduction_matrix, clutch_distribution) {
  recruitment_rate <- reproduction_matrix[nrow(reproduction_matrix), ncol(reproduction_matrix)]
  mean_clutch_size <- sum(clutch_distribution * (1:length(clutch_distribution)))
  recruitment_probability <- recruitment_rate / mean_clutch_size
  
  if (recruitment_probability > 1) {
    stop("Inconsistency in model parameters: recruitment probability exceeds 1.")
  }
  
  return(recruitment_probability)
}


# Question 30
offspring_calc <- function(state, clutch_distribution, recruitment_probability) {
  num_adults <- state[length(state)] # Number of adults is the last element in the state vector
  num_clutches <- rbinom(1, num_adults, recruitment_probability) # Number of adults that recruit
  
  total_offspring <- 0
  if (num_clutches > 0) {
    for (i in 1:num_clutches) {
      clutch_size <- random_draw(clutch_distribution)
      total_offspring <- total_offspring + clutch_size
    }
  }
  
  return(total_offspring)
}

# Question 31
stochastic_step <- function(state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability) {
  # Apply survival and maturation
  new_state <- survival_maturation(state, growth_matrix)
  
  # Compute the number of offspring
  total_offspring <- offspring_calc(state, clutch_distribution, recruitment_probability)
  
  # Add offspring to the appropriate entry of new_state (first life stage)
  new_state[1] <- new_state[1] + total_offspring
  
  return(new_state)
}


# Question 32
stochastic_simulation <- function(initial_state, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length) {
  recruitment_probability <- stochastic_recruitment(reproduction_matrix, clutch_distribution)
  population_size <- numeric(simulation_length + 1)
  population_size[1] <- sum(initial_state)
  state <- initial_state
  
  for (i in 1:simulation_length) {
    if (sum(state) == 0) {
      population_size[(i+1):(simulation_length + 1)] <- 0
      break
    }
    
    state <- stochastic_step(state, growth_matrix, reproduction_matrix, clutch_distribution, recruitment_probability)
    population_size[i + 1] <- sum(state)
  }
  
  return(population_size)
}


# Question 33
question_33 <- function(){
  growth_matrix <- matrix(c(0.1, 0.0, 0.0, 0.0,
                            0.5, 0.4, 0.0, 0.0,
                            0.0, 0.4, 0.7, 0.0,
                            0.0, 0.0, 0.25, 0.4),
                          nrow=4, ncol=4, byrow=TRUE)
  reproduction_matrix <- matrix(c(0.0, 0.0, 0.0, 2.6,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0, 0.0),
                                nrow=4, ncol=4, byrow=TRUE)
  projection_matrix <- reproduction_matrix + growth_matrix
  
  # Define clutch distribution and simulation length
  clutch_distribution <- c(0.06, 0.08, 0.13, 0.15, 0.16, 0.18, 0.15, 0.06, 0.03)
  simulation_length <- 24
  
  # Define initial conditions
  initial_condition_adults <- c(0, 0, 0, 100)
  initial_condition_spread <- c(25, 25, 25, 25)
  
  # Run stochastic simulations
  population_adults_stochastic <- stochastic_simulation(initial_condition_adults, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)
  population_spread_stochastic <- stochastic_simulation(initial_condition_spread, growth_matrix, reproduction_matrix, clutch_distribution, simulation_length)
  
  png(filename="question_33.png", width = 600, height = 400)
  # plot your graph here
  plot(population_adults_stochastic, type = 'l', col = 'blue', ylim = range(c(population_adults_stochastic, population_spread_stochastic)),
       xlab = "Time Step", ylab = "Total Population Size", main = "Stochastic Population Size Time Series")
  lines(population_spread_stochastic, type = 'l', col = 'red')
  legend("topright", legend = c("100 Adults", "Spread Across Stages"), col = c("blue", "red"), lty = 1)
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Questions 34 and 35 involve writing code elsewhere to run your simulations on the cluster

# Question 36
question_36 <- function(){
  
  png(filename="question_36", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Question 37
question_37 <- function(){
  
  png(filename="question_37_small", width = 600, height = 400)
  # plot your graph for the small initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="question_37_large", width = 600, height = 400)
  # plot your graph for the large initial population size here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}



# Challenge questions - these are optional, substantially harder, and a maximum
# of 14% is available for doing them. 

# Challenge question A
Challenge_A <- function() {
  
  
  
  png(filename="Challenge_A_min", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  png(filename="Challenge_A_max", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question B
Challenge_B <- function() {
  
  
  
  png(filename="Challenge_B", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question C
Challenge_C <- function() {
  
  
  
  png(filename="Challenge_C", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()

}

# Challenge question D
Challenge_D <- function() {
  
  
  
  png(filename="Challenge_D", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function(){
  
  
  
  png(filename="Challenge_E", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function(){
  
  
  
  png(filename="Challenge_F", width = 600, height = 400)
  # plot your graph here
  Sys.sleep(0.1)
  dev.off()
  
}
