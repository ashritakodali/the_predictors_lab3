 
#Load libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(knitr)
library(kableExtra)

# Read in the data
games <- readRDS("C:/Users/nblau/Downloads/data.rds")

# Identify unique team names 
teams <- unique(c(games$Visiting_Team, games$Home_Team))
n_teams <- length(teams) #30

# Create empty transition matrix
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))

# Fill in matrix based on results of games 
for (i in 1:nrow(games)) {
  visitor <- games$Visiting_Team[i]
  home <- games$Home_Team[i]
  visitor_score <- games$Visiting_Score[i]
  home_score <- games$Home_Score[i]
  
  # Check if home team loses to visiting team
  if (visitor_score > home_score) {
    #increment matrix entry by 1 
    transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
   # Check if visiting team loses to home team
  } else if (home_score > visitor_score) {
    #increment matrix entry by 1 
    transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
  }
}

# Normalize rows to sum to 1 so matrix behaves like a Markov chain
transition_matrix <- transition_matrix / rowSums(transition_matrix, na.rm = TRUE)

### Option b)

# Compute steady-state vector using repeated matrix multiplication
b <- rep(1 / n_teams, n_teams)  # Start with a uniform probability distribution

# Do the multiplication 1000 times and see what row vector results from this (this is our steady state) ranking
for (i in 1:10000) {
  b <- b %*% transition_matrix
}

# Set names for the teams
steady_state <- setNames(as.numeric(b), teams)

# Steady_state 
# LAN        SEA        BOS        MIN        KCA        CLE        TOR        PHI        SFN        SLN 
# 0.03948072 0.03394242 0.03584793 0.03187847 0.02999425 0.03454301 0.03532205 0.03243858 0.03400499 0.03536689 
# COL        ATL        WAS        CHN        NYA        MIL        BAL        ANA        CHA        TEX 
# 0.03045380 0.03445328 0.03267800 0.03489770 0.03830918 0.03498321 0.03316139 0.03219857 0.02930756 0.03184672 
# ARI        SDN        CIN        TBA        OAK        HOU        NYN        PIT        DET        MIA 
# 0.03200487 0.03235099 0.03064543 0.03660620 0.03197763 0.03721329 0.03341753 0.03138499 0.02959835 0.02969201 
# 
# 

# Rank teams from highest to lowest probability
team_rankings <- sort(steady_state, decreasing = TRUE)

# Final Rankings 
# LAN        NYA        HOU        TBA        BOS        SLN        TOR        MIL        CHN        CLE 
# 0.03948072 0.03830918 0.03721329 0.03660620 0.03584793 0.03536689 0.03532205 0.03498321 0.03489770 0.03454301 
# ATL        SFN        SEA        NYN        BAL        WAS        PHI        SDN        ANA        ARI 
# 0.03445328 0.03400499 0.03394242 0.03341753 0.03316139 0.03267800 0.03243858 0.03235099 0.03219857 0.03200487 
# OAK        MIN        TEX        PIT        CIN        COL        KCA        MIA        DET        CHA 
# 0.03197763 0.03187847 0.03184672 0.03138499 0.03064543 0.03045380 0.02999425 0.02969201 0.02959835 0.02930756 

##############################################################################
## Part 2
### Question: How do the rankings of teams change over the years?
##############################################################################

# Calculate transition matrix for a given year
calculate_matrix <- function(games_year) {
  transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams))
  
  # Fill in the transition matrix
  for (i in 1:nrow(games_year)) {
    visitor <- games_year$Visiting_Team[i]
    home <- games_year$Home_Team[i]
    visitor_score <- games_year$Visiting_Score[i]
    home_score <- games_year$Home_Score[i]
    
    if (visitor_score > home_score) {
      # Home team loses to visiting team
      transition_matrix[home, visitor] <- transition_matrix[home, visitor] + 1
    } else if (home_score > visitor_score) {
      # Visiting team loses to home team
      transition_matrix[visitor, home] <- transition_matrix[visitor, home] + 1
    }
  }
  
  # Normalize each row to sum to 1
  transition_matrix <- transition_matrix / rowSums(transition_matrix, na.rm = TRUE)
  
  transition_matrix
}

# Compute steady-state vector (rankings)
find_steady_state <- function(transition_matrix) {
  b <- rep(1 / n_teams, n_teams)  # Start with a uniform distribution
  for (i in 1:10000) {
    b <- b %*% transition_matrix
  }
  b
}

# Loop through each year
yearly_rankings <- data.frame()
for (year in unique(games$season)) {
  # Subset games by the year
  games_year <- filter(games, season == year)
  
  # Calculate transition matrix for the year
  transition_matrix <- calculate_matrix(games_year)
  
  # Find steady-state rankings for the year
  steady_state <- find_steady_state(transition_matrix)
  
  # Put rankings into dataframe
  yearly_rankings <- rbind(yearly_rankings, data.frame(season = year, team = teams, rank = steady_state))
}
#####################################################################
## Putting the matrix in a form suitable for graphing/creating tables 
#####################################################################

# Convert wide format to long format 
long_data <- yearly_rankings %>%
  pivot_longer(cols = starts_with("rank."), 
               names_to = "ranked_team",  # change name
               values_to = "rank") %>%
  mutate(ranked_team = str_remove(ranked_team, "rank.")) %>%  #extract only the team name 
  select(-team)

# Remove duplicate rows
final_data <- long_data %>%
  distinct(season, ranked_team, .keep_all = TRUE)  # Keep only unique rows for season and ranked_team

### Make table visualization 

# Convert data from long format to wide format to make the table
table <- final_data %>%
  select(season, ranked_team, rank) %>%  
  pivot_wider(names_from = season, values_from = rank) %>% 
  rename(Team = ranked_team) #rename column 


# Identify highest ranking for each year (column) and highlight this 
table_highlighted <- table %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%  
  mutate(across(where(is.numeric), ~ ifelse(. == max(.), cell_spec(., bold = TRUE, color = "red"), as.character(.))))

# Create kable table 
table_highlighted %>%
  kable(caption = "MLB Team Rankings Across Seasons", escape = FALSE, digits = 4, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#0072B2") %>%  
  row_spec(0, bold = TRUE, font_size = 14, color = "black")  


