# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Project and code information ------------------------------------------------
##
## Project: Fantasy draft data tracking 2025/26
##
## Purpose of script: functions to be called in the analysis file
##
## Author: Jacob Cohen
##
## Date Created: 2025-08-19
##
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Get total points per week ---------------------------------------------------
message("Get total points per week")

get_manager_points <- function(weekly_data){
  weekly_data %>%
    filter(bench == 0) %>%
    group_by(gameweek, manager) %>%
    summarise(total_points = sum(points), .groups = "drop") %>%
    arrange(gameweek, desc(total_points))
}

#join manager points onto matchups and calculate weekly result
get_match_results <- function(matchups, weekly_data){
  
  manager_points <- get_manager_points(weekly_data = weekly_data)
  
  matchups %>%
    left_join(manager_points, by = c("gameweek", "manager1" = "manager")) %>%
    rename(points1 = total_points) %>%
    left_join(manager_points, by = c("gameweek", "manager2" = "manager")) %>%
    rename(points2 = total_points) %>%
    mutate(
      manager1_result = case_when(
        is.na(points1) ~ NA, #ignore NAs
        points1 > points2 ~ 3, #3 points for a win
        points1 < points2 ~ 0, #0 points for a loss
        points1 == points2 ~ 1), #1 point for a draw
      
      manager2_result = case_when(
        is.na(points2) ~ NA, #ignore NAs
        points2 > points1 ~ 3, #3 points for a win
        points2 < points1 ~ 0, #0 points for a loss
        points2 == points1 ~ 1 #1 point for a draw
      )
    )
}

## Create a win matrix ---------------------------------------------------------
message("Create a win matrix")

get_win_matrix <- function(weekly_data, matchups, ordered_managers){
  
  match_results <- get_match_results(matchups = matchups, weekly_data = weekly_data)
  
  all_managers <- unique(c(match_results$manager1, match_results$manager2))
  
  #build the win matrix
  win_matrix <- match_results %>%
    #determine winner and loser for each matchup
    mutate(
      winner = case_when(
        manager1_result > manager2_result ~ manager1,
        manager2_result > manager1_result ~ manager2,
        TRUE ~ NA_character_  #draws or missing data
      ),
      loser = case_when(
        manager1_result < manager2_result ~ manager1,
        manager2_result < manager1_result ~ manager2,
        TRUE ~ NA_character_
      )
    ) %>%
    #keep only rows where there was a clear winner
    filter(!is.na(winner)) %>%
    #count how many times each loser has been beaten by each winner
    count(loser, winner) %>%
    #ensure that all manager combinations are represented, even if no games occurred
    complete(
      loser = all_managers,
      winner = all_managers,
      fill = list(n = 0)  #fill missing counts with 0
    ) %>%
    #optionally remove self-matchups
    filter(loser != winner) %>%
    #reshape data so that each winner is a column, each loser is a row
    pivot_wider(
      names_from = winner,
      values_from = n
    ) %>%
    #arrange the rows in the same order as all_managers
    arrange(match(loser, all_managers)) %>%
    #move 'loser' from a column to row names (to make it a matrix-like structure)
    column_to_rownames("loser")
  
  win_matrix <- win_matrix[ordered_managers, ordered_managers]
  
  return(win_matrix)

}


## Generate a luck data frame --------------------------------------------------
message("Generate a luck data frame")

#step 1: calculate weekly medians
get_weekly_medians <- function(weekly_data){
  
  manager_points <- get_manager_points(weekly_data = weekly_data)
  
  weekly_medians <- manager_points %>%
    group_by(gameweek) %>%
    summarise(median_points = median(total_points), .groups = "drop")
  
  return(weekly_medians)
}

get_luck <- function(weekly_data, matchups){
  
  match_results <- get_match_results(matchups = matchups, weekly_data = weekly_data)
  
  weekly_medians <- get_weekly_medians(weekly_data = weekly_data)
  
  manager_points <- get_manager_points(weekly_data = weekly_data)
  
  #step 2: reshape match_results to long format and join points + median
  luck_df <- match_results %>%
    select(gameweek, manager1, manager2, manager1_result, manager2_result) %>%
    pivot_longer(
      cols = c(manager1, manager2),
      names_to = "which_manager",
      values_to = "manager"
    ) %>%
    mutate(
      result = if_else(
        which_manager == "manager1", manager1_result, manager2_result
      )
    ) %>%
    select(gameweek, manager, result) %>%
    left_join(manager_points, by = c("gameweek", "manager")) %>%
    left_join(weekly_medians, by = "gameweek") %>%
    #step 3: define luck based on result vs median
    mutate(
      luck = case_when(
        result == 0 & total_points > median_points ~ "Unlucky",
        result == 1 & total_points > median_points ~ "Unlucky",
        result == 3 & total_points < median_points ~ "Lucky",
        result == 1 & total_points < median_points ~ "Lucky",
        is.na(result) ~ NA,
        TRUE ~ NA
      )
    )
  
  return(luck_df)
}



## Track weekly bench totals ---------------------------------------------------
message("Track weekly bench totals")

get_bench_totals <- function(weekly_data){
  bench_totals <- weekly_data %>%
    filter(bench == 1) %>%  # Keep only bench players
    group_by(gameweek, manager) %>%
    summarise(bench_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
    arrange(gameweek, desc(bench_points))
  
  return(bench_totals)
}

#NOW DO IT ACCOUNTING FOR FORMATIONS AND MANAGER CHOICES
#step 1: filter out players who were subbed in automatically (they were already used)
get_eligible_players <- function(weekly_data){
  eligible_players <- weekly_data %>%
    filter(
      (bench == 0) |    #starters OR
        (automatic_sub_on == 0 & bench == 1)  #bench players who didn't come on
    )
  
  return(eligible_players)
}

#step 2: function to build optimal legal lineup
get_optimal_lineup <- function(df) {
  #separate by position
  gk <- df %>% filter(position == "gkp") %>% arrange(desc(points))
  def <- df %>% filter(position == "def") %>% arrange(desc(points))
  mid <- df %>% filter(position == "mid") %>% arrange(desc(points))
  fwd <- df %>% filter(position == "fwd") %>% arrange(desc(points))
  
  #generate all valid formations
  formations <- list(
    c(gk = 1, def = 3, mid = 4, fwd = 3),
    c(gk = 1, def = 4, mid = 4, fwd = 2),
    c(gk = 1, def = 3, mid = 5, fwd = 2),
    c(gk = 1, def = 4, mid = 5, fwd = 1),
    c(gk = 1, def = 5, mid = 3, fwd = 2),
    c(gk = 1, def = 5, mid = 4, fwd = 1)
  )
  
  best_score <- 0
  best_lineup <- NULL
  
  for (f in formations) {
    if (
      nrow(gk) >= f["gk"] &
      nrow(def) >= f["def"] &
      nrow(mid) >= f["mid"] &
      nrow(fwd) >= f["fwd"]
    ) {
      lineup <- bind_rows(
        gk[1:f["gk"], ],
        def[1:f["def"], ],
        mid[1:f["mid"], ],
        fwd[1:f["fwd"], ]
      )
      total <- sum(lineup$points, na.rm = TRUE)
      if (total > best_score) {
        best_score <- total
        best_lineup <- lineup
      }
    }
  }
  
  best_lineup
}

#step 3: apply function to each manager and gameweek
get_optimal_lineups <- function(weekly_data){
  eligible_players <- get_eligible_players(weekly_data = weekly_data)
  
  optimal_lineups <- eligible_players %>%
    group_by(manager, gameweek) %>%
    group_modify(~ get_optimal_lineup(.x)) %>%
    ungroup()
  
  return(optimal_lineups)
}

#step 4: get optimal points
get_optimal_points <- function(weekly_data){
  optimal_lineups <- get_optimal_lineups(weekly_data = weekly_data)
  
  optimal_points <- optimal_lineups %>%
    group_by(manager, gameweek) %>%
    summarise(optimal_points = sum(points, na.rm = TRUE), .groups = "drop")
  
  return(optimal_points)
}

get_bench_points_lost <- function(weekly_data){
  #step 5: join with actual points
  actual_points <- weekly_data %>%
    filter(bench == 0 | automatic_sub_on == 1) %>%
    group_by(manager, gameweek) %>%
    summarise(actual_points = sum(points, na.rm = TRUE), .groups = "drop")
  
  optimal_points <- get_optimal_points(weekly_data = weekly_data)
  
  #step 7: final table
  potential_points_lost <- optimal_points %>%
    left_join(actual_points, by = c("manager", "gameweek")) %>%
    mutate(
      potential_points = optimal_points - actual_points
    )
  
  return(potential_points_lost)
}


## See how optimal points changes the table ------------------------------------
message("See how optimal points changes the table")

get_match_results_optimal <- function(weekly_data, matchups){
  
  match_results <- get_match_results(matchups = matchups, weekly_data = weekly_data)
  
  optimal_points <- get_optimal_points(weekly_data = weekly_data)
  
  #step 1: join optimal points for manager1 and manager2
  match_results_optimal <- match_results %>%
    left_join(
      optimal_points,
      by = c("manager1" = "manager", "gameweek")
    ) %>%
    rename(optimal_points1 = optimal_points) %>%
    left_join(
      optimal_points,
      by = c("manager2" = "manager", "gameweek")
    ) %>%
    rename(optimal_points2 = optimal_points)
  
  #step 2: calculate optimal matchup results (3 = win, 1 = draw, 0 = lose)
  match_results_optimal <- match_results_optimal %>%
    mutate(
      manager1_opt_result = case_when(
        optimal_points1 > optimal_points2 ~ 3,
        optimal_points1 < optimal_points2 ~ 0,
        optimal_points1 == optimal_points2 ~ 1,
        TRUE ~ NA_real_
      ),
      manager2_opt_result = case_when(
        optimal_points2 > optimal_points1 ~ 3,
        optimal_points2 < optimal_points1 ~ 0,
        optimal_points2 == optimal_points1 ~ 1,
        TRUE ~ NA_real_
      )
    )
  
  return(match_results_optimal)
}

get_optimised_league_table <- function(weekly_data, matchups){
  
  match_results_optimal <- get_match_results_optimal(weekly_data = weekly_data, matchups = matchups)
  
  optimal_points <- get_optimal_points(weekly_data = weekly_data)
  
  optimised_league_long <- match_results_optimal %>%
    select(gameweek, manager1, manager2, manager1_opt_result, manager2_opt_result) %>%
    pivot_longer(
      cols = c(manager1, manager2),
      names_to = "which_manager",
      values_to = "manager"
    ) %>%
    mutate(
      opt_result = if_else(
        which_manager == "manager1",
        manager1_opt_result,
        manager2_opt_result
      )
    ) %>%
    select(gameweek, manager, opt_result)
  
  league_table_optimised <- optimised_league_long %>%
    filter(!is.na(manager)) %>%                      # remove NA rows
    group_by(manager) %>%
    summarise(
      W = sum(opt_result == 3, na.rm = TRUE),
      D = sum(opt_result == 1, na.rm = TRUE),
      L = sum(opt_result == 0, na.rm = TRUE),
      total_optimised_league_points = sum(opt_result, na.rm = TRUE)
    ) %>%
    left_join(
      optimal_points %>%
        group_by(manager) %>%
        summarise(total_optimised_manager_points = sum(optimal_points, na.rm = TRUE)),
      by = "manager"
    ) %>%
    arrange(desc(total_optimised_league_points), desc(total_optimised_manager_points)) %>%
    mutate(rank = row_number()) %>%
    select(rank, manager, W, D, L, total_optimised_manager_points, total_optimised_league_points)
  
  return(league_table_optimised)
}

## Track cumulative totals -----------------------------------------------------
message("Track cumulative totals")

#get data frame with weekly results in long format and add cumulative columns
get_cumulative_points <- function(weekly_data, matchups){
  
  match_results <- get_match_results(weekly_data = weekly_data, matchups = matchups)
  
  manager_points <- get_manager_points(weekly_data = weekly_data)
  
  cumulative_points <- match_results %>%
    select(gameweek, manager1, manager2, manager1_result, manager2_result) %>%
    pivot_longer(
      cols = c(manager1, manager2),
      names_to = "which_manager",
      values_to = "manager"
    ) %>%
    mutate(
      weekly_league_points = if_else(
        which_manager == "manager1",
        manager1_result,
        manager2_result
      )
    ) %>%
    select(gameweek, manager, weekly_league_points) %>%
    left_join(manager_points, by = c("gameweek", "manager")) %>%
    rename(weekly_manager_points = total_points) %>%
    arrange(manager, gameweek) %>%
    group_by(manager) %>%
    mutate(
      cumulative_league_points = cumsum(weekly_league_points),
      cumulative_manager_points = cumsum(weekly_manager_points)
    ) %>%
    ungroup() %>%
    group_by(gameweek) %>%
    arrange(gameweek, desc(cumulative_league_points), desc(cumulative_manager_points)) %>%
    mutate(
      weekly_position = if_else(
        is.na(cumulative_league_points),
        NA_integer_,
        row_number()
      )
    ) %>%
    ungroup()
  
  #add bench totals
  bench_totals <- get_bench_totals(weekly_data = weekly_data)
  
  cumulative_points <- cumulative_points %>%
    # Join weekly bench points
    left_join(bench_totals, by = c("gameweek", "manager")) %>%
    # Compute cumulative bench totals
    group_by(manager) %>%
    mutate(cumulative_bench_points = cumsum(bench_points)) %>%
    ungroup()
  
  #add actual bench points lost
  bench_points_lost <- get_bench_points_lost(weekly_data = weekly_data) 
  
  cumulative_points <- cumulative_points %>%
    left_join(
      bench_points_lost %>% select(manager, gameweek, potential_points),
      by = c("manager", "gameweek")
    ) %>%
    rename(actual_bench_points_lost = potential_points) %>%
    group_by(manager) %>%
    arrange(gameweek, .by_group = TRUE) %>%
    mutate(cumulative_actual_bench_points_lost = cumsum(actual_bench_points_lost)) %>%
    ungroup()
  
  #add optimal points differentials
  optimal_points <- get_optimal_points(weekly_data = weekly_data)
  
  #step 1: join optimal points for manager1 and manager2
  match_results_optimal <- match_results %>%
    left_join(
      optimal_points,
      by = c("manager1" = "manager", "gameweek")
    ) %>%
    rename(optimal_points1 = optimal_points) %>%
    left_join(
      optimal_points,
      by = c("manager2" = "manager", "gameweek")
    ) %>%
    rename(optimal_points2 = optimal_points) %>%
    #step 2: calculate optimal matchup results (3 = win, 1 = draw, 0 = lose)
    mutate(
      manager1_opt_result = case_when(
        optimal_points1 > optimal_points2 ~ 3,
        optimal_points1 < optimal_points2 ~ 0,
        optimal_points1 == optimal_points2 ~ 1,
        TRUE ~ NA_real_
      ),
      manager2_opt_result = case_when(
        optimal_points2 > optimal_points1 ~ 3,
        optimal_points2 < optimal_points1 ~ 0,
        optimal_points2 == optimal_points1 ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    #step 3: calculate each manager’s potential gain (optimal_points - actual_points), ensuring it's not negative
    mutate(
      potential_gain1 = pmax(manager1_opt_result - manager1_result, 0),
      potential_gain2 = pmax(manager2_opt_result - manager2_result, 0)
    )
  
  #step 4: convert to long format
  potential_gains_long <- match_results_optimal %>%
    select(gameweek, manager1, manager2, potential_gain1, potential_gain2) %>%
    pivot_longer(
      cols = c(potential_gain1, potential_gain2),
      names_to = "which_manager",
      values_to = "potential_gain"
    ) %>%
    mutate(
      manager = if_else(which_manager == "potential_gain1", manager1, manager2)
    ) %>%
    select(gameweek, manager, potential_gain) %>%
    filter(!is.na(potential_gain))
  
  #Step 5: add to the cumulative points data frame
  cumulative_points <- cumulative_points %>%
    left_join(potential_gains_long, by = c("manager", "gameweek")) %>%
    rename(league_points_lost = potential_gain) %>%
    group_by(manager) %>%
    arrange(gameweek, .by_group = TRUE) %>%
    mutate(cumulative_league_points_lost = cumsum(league_points_lost)) %>%
    ungroup()
  
  return(cumulative_points)
}


## Create league table ---------------------------------------------------------
message("Create league table")

get_league_table <- function(weekly_data, matchups){
  
  cumulative_points <- get_cumulative_points(weekly_data = weekly_data, matchups = matchups)
  
  league_table <- cumulative_points %>%
    filter(!is.na(manager) & !is.na(weekly_league_points)) %>%
    mutate(
      W = if_else(weekly_league_points == 3, 1, 0),
      D = if_else(weekly_league_points == 1, 1, 0),
      L = if_else(weekly_league_points == 0, 1, 0)
    ) %>%
    group_by(manager) %>%
    summarise(
      W = sum(W),
      D = sum(D),
      L = sum(L),
      total_manager_points = max(cumulative_manager_points),
      total_league_points = max(cumulative_league_points)
    ) %>%
    arrange(desc(total_league_points), desc(total_manager_points)) %>%
    mutate(rank = row_number()) %>%
    select(rank, manager, W, D, L, total_manager_points, total_league_points)
  
  return(league_table)
}

## Top points scorers per manager ----------------------------------------------
message("Top points scorers per manager")

#initial function for all top player functions
get_starters <- function(weekly_data){
  
  starters <- weekly_data %>%
    filter(bench == 0)
  
  return(starters)
}

get_player_summary <- function(weekly_data){
  
  starters <- get_starters(weekly_data = weekly_data)
  
  #calculate total points and appearances per player per manager
  summary_df <- starters %>%
    group_by(manager, position, player) %>%
    summarise(
      total_points = sum(points),
      appearances = n(),
      .groups = "drop"
    ) %>%
    mutate(points_per_appearance = total_points / appearances)
  
  return(summary_df)
}

#top points by position
get_top_scorers_by_position <- function(weekly_data){
  
  summary_df <- get_player_summary(weekly_data = weekly_data)
  
  #for each manager and position, pick top scorer by total_points, tie break by fewer appearances
  top_scorers <- summary_df %>%
    group_by(manager, position) %>%
    arrange(desc(total_points), appearances) %>%
    slice(1) %>%
    ungroup() %>%
    select(manager, position, player, total_points)
  
  #pivot wider to get one row per manager with positions as columns
  top_scorers_wide <- top_scorers %>%
    pivot_wider(
      names_from = position,
      values_from = c(player, total_points),
      names_glue = "{position}_{.value}"
    )
  
  #rename columns to desired format
  colnames(top_scorers_wide) <- colnames(top_scorers_wide) %>%
    sub("_player$", "", .) %>%
    sub("_total_points$", "_points", .)
  
  #reorder columns nicely (optional)
  top_scorers_wide <- top_scorers_wide %>%
    select(manager, gkp, gkp_points, def, def_points, mid, mid_points, fwd, fwd_points)
  
  return(top_scorers_wide)
}

#top overall points
get_top_overall_player <- function(weekly_data, min_appearances){
  
  summary_df <- get_player_summary(weekly_data = weekly_data)
  
  summary_df <- summary_df %>%
    filter(appearances >= min_appearances) #only players with enough appearances
  
  #pick top scorer per manager, tie break by fewer appearances
  top_scorers <- summary_df %>%
    group_by(manager) %>%
    arrange(desc(total_points), appearances) %>%
    slice(1) %>%
    ungroup()
  
  #select and rename columns for clarity
  top_scorers <- top_scorers %>%
    select(manager, player, total_points, appearances)
  
  return(top_scorers)
}

#top overall points per appearance
get_top_per_appearance <- function(weekly_data, min_appearances){
  
  summary_df <- get_player_summary(weekly_data = weekly_data)
  
  summary_df <- summary_df %>%
    filter(appearances >= min_appearances) %>%  #only players with enough appearances
    mutate(points_per_appearance = total_points / appearances)
  
  #pick top scorer per manager by points per appearance, break ties by total_points descending
  top_scorers <- summary_df %>%
    group_by(manager) %>%
    arrange(desc(points_per_appearance), desc(total_points), appearances) %>%
    slice(1) %>%
    ungroup()
  
  #select and rename columns for clarity
  top_scorers <- top_scorers %>%
    select(manager, player, total_points, appearances, points_per_appearance)
  
  return(top_scorers)
}


## Top club per manager --------------------------------------------------------
message("Top club per manager")

get_top_club_per_manager <- function(weekly_data) {
  
  starters <- get_starters(weekly_data = weekly_data)
  
  #count player appearances per manager and team
  club_appearances <- starters %>%
    group_by(manager, team) %>%
    summarise(player_gameweeks = n(), .groups = "drop")
  
  #for each manager, get the club with the highest player_gameweeks
  top_clubs <- club_appearances %>%
    group_by(manager) %>%
    arrange(desc(player_gameweeks)) %>%
    slice(1) %>%
    ungroup() %>%
    rename(club = team, total_appearances = player_gameweeks)
  
  return(top_clubs)
}


## Most shared player ----------------------------------------------------------
message("Top club per manager")

get_most_managers_per_player <- function(weekly_data, list_length){
  
  starters <- get_starters(weekly_data = weekly_data)
  
  # Count unique managers per player
  player_manager_counts <- starters %>%
    distinct(player, manager) %>%
    group_by(player) %>%
    summarise(
      num_managers = n(),
      managers = paste(sort(unique(manager)), collapse = ", ")
      , .groups = "drop") %>%
    arrange(desc(num_managers)) %>%
    slice_head(n = list_length)
  
  return(player_manager_counts)
}

## Waiver data -----------------------------------------------------------------
message("Waiver data")

get_waiver_summary <- function(waivers){
  
  #step 1: count total number of transfers per manager
  total_transfers <- waivers %>%
    group_by(manager) %>%
    summarise(total_transfers = n(), .groups = "drop")
  
  #step 2: count number of transfers per position per manager
  transfers_by_position <- waivers %>%
    group_by(manager, position) %>%
    summarise(position_transfers = n(), .groups = "drop") %>%
    pivot_wider(
      names_from = position,
      values_from = position_transfers,
      values_fill = 0
    )
  
  #step 3: combine into one summary table
  transfer_summary <- total_transfers %>%
    left_join(transfers_by_position, by = "manager") %>%
    select(manager, total_transfers, everything())
  
  return(transfer_summary)
}

## Alternative luck formulation based on opponent not optimising ---------------
message("Alternative luck formulation based on opponent not optimising")

get_luck_optimised <- function(weekly_data, matchups){
  
  #step 1: get optimal and actual match results
  match_results_optimal <- get_match_results_optimal(weekly_data, matchups)
  
  #step 2: identify when a manager benefited (won or drew only because the opponent was suboptimal)
  benefit_data <- match_results_optimal %>%
    mutate(
      # Did a manager gain league points compared to the optimal result?
      manager1_benefit = manager1_result > manager1_opt_result,
      manager2_benefit = manager2_result > manager2_opt_result,
      manager1_unlucky = manager1_result < manager1_opt_result,
      manager2_unlucky = manager2_result < manager2_opt_result
    ) %>%
    select(gameweek, manager1, manager2, manager1_benefit, manager2_benefit, manager1_unlucky, manager2_unlucky)
  
  #step 3: convert to long format
  benefit_long <- benefit_data %>%
    pivot_longer(
      cols = c(manager1, manager2),
      names_to = "manager_type",
      values_to = "manager"
    ) %>%
    mutate(
      benefited = case_when(
        manager_type == "manager1" ~ manager1_benefit,
        manager_type == "manager2" ~ manager2_benefit
      ),
      unlucky = case_when(
        manager_type == "manager1" ~ manager1_unlucky,
        manager_type == "manager2" ~ manager2_unlucky
      )
    ) %>%
    select(gameweek, manager, benefited, unlucky)
  
  #step 4: summarise totals per manager
  luck_summary <- benefit_long %>%
    filter(!is.na(manager)) %>%
    group_by(manager) %>%
    summarise(
      lucky = sum(benefited, na.rm = TRUE),
      bad_manager = sum(unlucky, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(lucky))
  
  return(luck_summary)
}

## Count total automatic substitutions per manager -----------------------------
message("Count total automatic substitutions per manager")

get_automatic_subs <- function(weekly_data){
  
  all_managers <- unique(weekly_data$manager)
  all_managers <- all_managers[!is.na(all_managers)]
  
  automatic_subs <- weekly_data %>%
    filter(automatic_sub_on == 1) %>%
    count(manager, name = "automatic_subs")
  
  total_auto_subs <- tibble(manager = all_managers) %>%
    left_join(automatic_subs, by = "manager") %>%
    mutate(automatic_subs = replace_na(automatic_subs, 0)) %>%
    arrange(desc(automatic_subs))
  
  return(total_auto_subs)
}
