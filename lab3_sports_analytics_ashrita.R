library(dplyr)
library(tidyr)
library(Matrix)
game_data <- readRDS("data.rds")



# Creating the transition probability matrix:
### (1) get team names 
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))

n <- length(teams)

### (2) initialize a matrix
transition_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))


### (3) looping through data to fill in matrix
for (i in 1:nrow(game_data)) {
  visiting_team <- as.character(game_data[i, 1])
  home_team <- as.character(game_data[i, 3])
  visiting_score <- game_data[i, 5]
  home_score <- game_data[i, 6]
  
  # if visiting team loses
  if (visiting_score < home_score) {
    transition_matrix[visiting_team, home_team] <- transition_matrix[visiting_team, home_team] + 1
  }
  
  # if home team loses
  if (home_score < visiting_score) {
    transition_matrix[home_team, visiting_team] <- transition_matrix[home_team, visiting_team] + 1
  }
}


### (4) normalizing the transition prob matrix
transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/")


# Scenario 1: converging to the steady state vector
prob <- 1/n
b <- rep(prob, n)

n_iterations <- 10000

for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

sorted_b <- setNames(as.numeric(b), teams)

# LAN        SEA        BOS        MIN        KCA        CLE 
# 0.03948072 0.03394242 0.03584793 0.03187847 0.02999425 0.03454301 
# TOR        PHI        SFN        SLN        COL        ATL 
# 0.03532205 0.03243858 0.03400499 0.03536689 0.03045380 0.03445328 
# WAS        CHN        NYA        MIL        BAL        ANA 
# 0.03267800 0.03489770 0.03830918 0.03498321 0.03316139 0.03219857 
# CHA        TEX        ARI        SDN        CIN        TBA 
# 0.02930756 0.03184672 0.03200487 0.03235099 0.03064543 0.03660620 
# OAK        HOU        NYN        PIT        DET        MIA 
# 0.03197763 0.03721329 0.03341753 0.03138499 0.02959835 0.02969201 


# Scenario 2: 

# Reading in the data
game_data <- readRDS("data.rds")

# Creating an empty list to store the transition matrices for each year
transition_matrices_yearly <- list()

# Getting the list of teams (you only need to do this once)
teams <- unique(c(game_data$Visiting_Team, game_data$Home_Team))

n <- length(teams)

# Loop through the unique years in the dataset
seasons <- unique(game_data$season)

# Looping through each year and calculating the transition matrix
for (season in seasons) {
  
  # Filter the game data for the current year
  year_data <- game_data[game_data$season == season, ]
  
  # Initialize a transition matrix for the current year
  transition_matrix <- matrix(0, nrow = n, ncol = n, dimnames = list(teams, teams))
  
  # Loop through the data for the current year to fill the transition matrix
  for (i in 1:nrow(year_data)) {
    visiting_team <- as.character(year_data[i, 1])
    home_team <- as.character(year_data[i, 3])
    visiting_score <- year_data[i, 5]
    home_score <- year_data[i, 6]
    
    # if visiting team loses
    if (visiting_score < home_score) {
      transition_matrix[visiting_team, home_team] <- transition_matrix[visiting_team, home_team] + 1
    }
    
    # if home team loses
    if (home_score < visiting_score) {
      transition_matrix[home_team, visiting_team] <- transition_matrix[home_team, visiting_team] + 1
    }
  }
  
  # Normalize the transition matrix for the current year
  transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/")
  
  # Store the transition matrix for this year
  transition_matrices_yearly[[as.character(season)]] <- transition_matrix
}

# get the b for each year
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2014
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2014 <- b

prob <- 1/n
b <- rep(prob, n)
#####
year_for_steady_state <- 2015
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2015 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2016
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2016 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2017
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2017 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2018
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2018 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2019
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2019 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2020
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2020 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2021
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2021 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2022
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2022 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2023
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2023 <- b
####
prob <- 1/n
b <- rep(prob, n)

year_for_steady_state <- 2024
transition_matrix <- transition_matrices_yearly[[as.character(year_for_steady_state)]]

# Steady state computation for the chosen year
n_iterations <- 10000
for (i in 1:n_iterations) {
  b <- b %*% transition_matrix
}

b_2024 <- b
####

library(dplyr)
df <- rbind(b_2014, b_2015, b_2016, b_2017, b_2018, b_2019, b_2020, b_2021, 
      b_2022, b_2023, b_2024)

year <- 2014:2024
df <- cbind(year, df)

scores <- unlist(lapply(df, function(x) x))

years <- rep(c(2014:2024), 30)
teams_2 <- rep(teams, each = 11)

final_df <- data.frame(scores, years, teams_2)

library(ggplot2)

ggplot(final_df, aes(x = years, y = scores, group = teams_2, color = teams_2)) + geom_point() +
  geom_line() + facet_wrap(~teams_2) + theme(legend.position = "none")
