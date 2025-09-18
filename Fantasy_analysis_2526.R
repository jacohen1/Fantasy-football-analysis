# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Project and code information ------------------------------------------------
##
## Project: Fantasy draft data tracking 2025/26
##
## Purpose of script: call functions and plot data
##
## Author: Jacob Cohen
##
## Date Created: 2025-08-19
##
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

## Clear workspace -------------------------------------------------------------
message("Clear workspace")

rm(list = ls())


## Load packages ---------------------------------------------------------------
message("Load packages")

packages <- c('here', 'ggplot2', 'readxl', 'tidyverse', 'gt')
lapply(packages, library, character.only=TRUE)


## Load data -------------------------------------------------------------------
message("Load data")
#path to the Excel file
file_path <- here("Data", "Fantasy_data_2526.xlsm")

#get all sheet names
sheet_names <- excel_sheets(file_path)

#function to remove any rows that have an NA in them
read_clean <- function(file, sheet) {
  df <- read_excel(file, sheet = sheet)
  #keep only rows with no NAs in any column
  df <- df %>% filter(if_all(everything(), ~ !is.na(.)))
  return(df)
}

#read each sheet into its own data frame
for(sheet in sheet_names){
  assign(sheet, read_clean(file_path, sheet))
}

#source functions
source(here("Code", "Fantasy_functions_2526.R"))

## Get fantasy data ------------------------------------------------------------
message("Get fantasy data")

##cumulative totals
cumulative_totals <- get_cumulative_points(weekly_data = weekly_data, matchups = matchups)

cumulative_totals <- cumulative_totals %>%
  mutate(manager = str_to_title(manager))

##league table
league_table <- get_league_table(weekly_data = weekly_data, matchups = matchups)

##win matrix
win_matrix <- get_win_matrix(weekly_data = weekly_data, matchups = matchups,
                             ordered_managers = c("jacob", "josh", "michael", "miz", "nez", "benji"))

##luck
luck <- get_luck(weekly_data = weekly_data, matchups = matchups)
luck_managerial <- get_luck_optimised(weekly_data = weekly_data, matchups = matchups)

##optimised league table
league_table_optimised <- get_optimised_league_table(weekly_data = weekly_data, matchups = matchups)

##top players
top_position <- get_top_scorers_by_position(weekly_data = weekly_data)
top_player <- get_top_overall_player(weekly_data = weekly_data, min_appearances = 3)
top_player_per_appearance <- get_top_per_appearance(weekly_data = weekly_data, min_appearances = 3)

##top club
top_club <- get_top_club_per_manager(weekly_data = weekly_data)

##most shared player
shared_players <- get_most_managers_per_player(weekly_data = weekly_data, list_length = 15)

##waivers
waiver_summary <- get_waiver_summary(waivers = waivers)

#automatic subs
auto_subs <- get_automatic_subs(weekly_data = weekly_data)

#preferred formation
pref_form <- get_pref_formation(weekly_data = weekly_data)

## Plot league tables ----------------------------------------------------------
message("Plot league tables")

#create a lookup table for managers and their team names
manager_teams <- tibble::tibble(
  manager = c("josh", "miz", "jacob", "nez", "benji", "michael"),
  team = c("Moyes R Back In Town", "Miz 3.0", "Placeholder FC", "Wirtz Up Dog", "Kinder Mbeumo", "TBC FC")
)

##league table
#create gt table
league_table_table <- league_table %>%
  arrange(desc(total_league_points)) %>%   #sort the league table by total league points in descending order, then total manager points in descending order
  mutate(manager = paste(manager_teams$team[match(manager, manager_teams$manager)], str_to_title(manager), sep = "<br>")) %>% #add team names to manager names
  
  gt() %>%  #start building a gt table
  
  #add a title to the table
  tab_header(
    title = "Better Late than Never 2025/26"
  ) %>%
  
  #format the number columns to show 0 decimal places
  fmt_number(
    columns = c(total_league_points, total_manager_points),
    decimals = 0
  ) %>%
  
  fmt_markdown(
    columns = c(manager) #so <br> creates a line break
  ) %>% 
  
  #rename column headers for display
  cols_label(
    rank = "Rank",
    manager = "Manager",
    W = "W",
    D = "D",
    L = "L",
    total_manager_points = "+",
    total_league_points = "Pts"
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager, rank, total_league_points))
  ) %>%
  
  #center-align all text in the body of the table
  tab_style(
    style = cell_text(align = "center", color = "#381B58"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #center-align all column labels (headers)
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  
  #customize table appearance
  tab_options(
    data_row.padding = px(10),      #add vertical padding between rows (for readability)
    column_labels.padding = px(20),#add vertical padding to column headers
    table.width = pct(70)          #set table width to 70% of the page/container
  )

league_table_table

#save table 
gtsave(league_table_table, filename = here("Results/25_26", "league_table.png"))

##optimised league table
#create gt table
opt_league_table_table <- league_table_optimised %>%
  arrange(desc(total_optimised_league_points), desc(total_optimised_manager_points)) %>%   #sort the league table by total league points in descending order, then total manager points in descending order
  mutate(manager = paste(manager_teams$team[match(manager, manager_teams$manager)], str_to_title(manager), sep = "<br>")) %>% #add team names to manager names
  
  gt() %>%  #start building a gt table
  
  #add a title to the table
  tab_header(
    title = "Better Late than Never 2025/26 - Optimal Table"
  ) %>%
  
  #format the number columns to show 0 decimal places
  fmt_number(
    columns = c(total_optimised_league_points, total_optimised_manager_points),
    decimals = 0
  ) %>%
  
  fmt_markdown(
    columns = c(manager) #so <br> creates a line break
  ) %>% 
  
  #rename column headers for display
  cols_label(
    rank = "Rank",
    manager = "Manager",
    W = "W",
    D = "D",
    L = "L",
    total_optimised_manager_points = "+",
    total_optimised_league_points = "Pts"
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager, rank, total_optimised_league_points))
  ) %>%
  
  #center-align all text in the body of the table
  tab_style(
    style = cell_text(align = "center", color = "#381B58"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #center-align all column labels (headers)
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  
  #customize table appearance
  tab_options(
    data_row.padding = px(10),      #add vertical padding between rows (for readability)
    column_labels.padding = px(20),#add vertical padding to column headers
    table.width = pct(70)          #set table width to 70% of the page/container
  )

opt_league_table_table

#save table
gtsave(opt_league_table_table, filename = here("Results/25_26", "optimised_league_table.png"))


## Plot win matrix -------------------------------------------------------------
message("Plot win matrix")

##win matrix
win_df <- as.data.frame(win_matrix)
win_df$loser <- rownames(win_matrix)

win_long <- win_df %>%
  pivot_longer(
    -loser,
    names_to = "winner",
    values_to = "wins"
  )

win_long$loser <- factor(win_long$loser, levels = rev(levels(factor(win_long$loser))))

#Plot
win_results <- ggplot(win_long, aes(x = winner, y = loser, fill = wins)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = wins), color = "black", size = 12, fontface = "bold") +
  scale_x_discrete(labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"), position = "top") +
  scale_y_discrete(labels = rev(c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"))) +
  scale_fill_gradient(low = "white", high = "white") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(color = "darkgreen", face = "bold", size = 38),
        axis.text.y = element_text(color = "red", face = "bold", size = 38),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "none") +
  coord_fixed(ratio = 0.7)

#save plot
ggsave(here("Results/25_26", "win_matrix.png"), win_results, height = 35, width = 35, units = "cm")


## Plot position data ----------------------------------------------------------
message("Plot position data")

#top player per position
top_position_table <- top_position %>%
  #Capitalise text columns
  mutate(manager = str_to_title(manager),
         gkp = str_to_title(str_replace_all(gkp, "_", "-")),
         def = str_to_title(str_replace_all(def, "_", "-")),
         mid = str_to_title(str_replace_all(mid, "_", "-")),
         fwd = str_to_title(str_replace_all(fwd, "_", "-"))) %>%
  
  #create the gt table
  gt() %>%
  
  #add a title to the table
  tab_header(
    title = "Top Scorers by Position 2025/26"
  ) %>%
  
  #format all *_points columns to have 0 decimal places
  fmt_number(
    columns = ends_with("_points"),
    decimals = 0
  ) %>%
  
  #rename columns for a cleaner table display
  cols_label(
    manager = "Manager",
    gkp = "GKP",
    gkp_points = "",
    def = "DEF",
    def_points = "",
    mid = "MID",
    mid_points = "",
    fwd = "FWD",
    fwd_points = ""
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "center")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(-c(manager))
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "left")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(c(manager))
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager))
  ) %>%
  
  #center-align all other columns (except manager)
  tab_style(
    style = cell_text(align = "center" , color = "#381B58"),
    locations = cells_body(columns = -c(manager))
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  ) %>%
  
  #add a thick right border to selected columns (to visually separate sections)
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(columns = c("manager", "gkp_points", "def_points", "mid_points"))
  ) %>%
  
  #bold GKP with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(gkp, gkp_points),
      rows = gkp_points == max(gkp_points, na.rm = TRUE)
    )
  ) %>%
  
  #bold DEF with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(def, def_points),
      rows = def_points == max(def_points, na.rm = TRUE)
    )
  ) %>%
  
  #bold MID with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(mid, mid_points),
      rows = mid_points == max(mid_points, na.rm = TRUE)
    )
  ) %>%
  
  #bold FWD with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(fwd, fwd_points),
      rows = fwd_points == max(fwd_points, na.rm = TRUE)
    )
  )

top_position_table

gtsave(top_position_table, filename = here("Results/25_26", "top_position.png"))


## Plot top player data --------------------------------------------------------
message("Plot top player data")

#top player per manager
top_player_table <- top_player %>%
  #Capitalise text columns
  mutate(manager = str_to_title(manager),
         player = str_to_title(str_replace_all(player, "_", "-"))) %>%
  
  #create the gt table
  gt() %>%
  
  #add a title to the table
  tab_header(
    title = "Top Scorers 2025/26"
  ) %>%
  
  #format all *_points columns to have 0 decimal places
  fmt_number(
    columns = ends_with("_points"),
    decimals = 0
  ) %>%
  
  #rename columns for a cleaner table display
  cols_label(
    manager = "Manager",
    player = "Player",
    total_points = "Points",
    appearances = "Appearances"
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "left")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager))
  ) %>%
  
  #center-align all other columns (except manager)
  tab_style(
    style = cell_text(align = "left" , color = "#381B58"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  ) %>%
  
  #bold player with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(player, total_points),
      rows = total_points == max(total_points, na.rm = TRUE)
    )
  )

top_player_table

gtsave(top_player_table, filename = here("Results/25_26", "top_players.png"))


## Plot top player per appearance data -----------------------------------------
message("Plot top player per appearance data")

#top player per manager
top_player_app_table <- top_player_per_appearance %>%
  #Capitalise text columns
  mutate(manager = str_to_title(manager),
         player = str_to_title(str_replace_all(player, "_", "-"))) %>%
  
  #create the gt table
  gt() %>%
  
  #add a title to the table
  tab_header(
    title = "Top Scorers per Appearance 2025/26"
  ) %>%
  
  #format all *_points columns to have 0 decimal places
  fmt_number(
    columns = ends_with("_points"),
    decimals = 0
  ) %>%
  
  fmt_number(
    columns = points_per_appearance,
    decimals = 1
  ) %>%
  
  #rename columns for a cleaner table display
  cols_label(
    manager = "Manager",
    player = "Player",
    total_points = "Points",
    appearances = "Appearances",
    points_per_appearance = "Points per Appearance"
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "left")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager))
  ) %>%
  
  #center-align all other columns (except manager)
  tab_style(
    style = cell_text(align = "left" , color = "#381B58"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  ) %>%
  
  #bold player with highest points
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),
      cell_text(weight = "bold", color = "#00FF87")
    ),
    locations = cells_body(
      columns = c(player, total_points, appearances, points_per_appearance),
      rows = points_per_appearance == max(points_per_appearance, na.rm = TRUE)
    )
  )

top_player_app_table

gtsave(top_player_app_table, filename = here("Results/25_26", "top_players_app.png"))


## Plot top club data ----------------------------------------------------------
message("Plot top club data")

#create club lookup table
club_lookup <- tibble::tibble(
  club = c(
    "arsenal", "villa", "bournemouth", "brentford",
    "brighton", "burnley", "chelsea", "palace",
    "everton", "fulham", "leeds", "liverpool",
    "man_city", "united", "newcastle",
    "forest", "sunderland", "spurs",
    "west_ham", "wolves"
  ),
  club_name = c(
    "Arsenal", "Aston Villa", "Bournemouth", "Brentford",
    "Brighton & Hove Albion", "Burnley", "Chelsea", "Crystal Palace",
    "Everton", "Fulham", "Leeds United", "Liverpool",
    "Manchester City", "Manchester United", "Newcastle United",
    "Nottingham Forest", "Sunderland", "Tottenham Hotspur",
    "West Ham United", "Wolverhampton Wanderers"
  ),
  colour = c(
    "#EF0107", "#670E36", "#DA291C", "#E30613",
    "#0057B8", "#6C1D45", "#034694", "#1B458F",
    "#00369C", "#000000", "#FFFFFF", "#C8102E",  # Liverpool same red
    "#6CABDD", "#DA291C", "#241F20", "#E53233",
    "#E00000", "#132257", "#7A263A", "#FDB913"
  )
)


#top club per manager
#step 1: combine data and club_lookup
club_data <- top_club %>%
  left_join(club_lookup, by = "club") %>%
  mutate(manager = stringr::str_to_title(manager)) %>%
  #create a safe version of colour to use in styling but drop it from visible table
  select(manager, club_name, total_appearances, colour)

#step 2: create the gt table and colour rows manually
club_table <- club_data %>%
  gt() %>%
  cols_label(
    manager = "Manager",
    club_name = "Club",
    total_appearances = "Appearances"
  )

#step 3: add background colours row-by-row using tab_style()
for(i in seq_len(nrow(club_data))) {
  club_table <- club_table %>%
    tab_style(
      style = list(
        cell_fill(color = club_data$colour[i]),
        cell_text(color = ifelse(club_data$colour[i] == "#FFFFFF", "black", "white"))
      ),
      locations = cells_body(
        rows = i,
        columns = c("club_name", "total_appearances")
      )
    )
}

#step 4: output the table
club_table <- club_table %>%
  cols_hide(columns = colour) %>%
  
  #add a title to the table
  tab_header(
    title = "Top Club per Manager 2025/26"
  ) %>%
  
  #format appearance column to have 0 decimal places
  fmt_number(
    columns = c(total_appearances),
    decimals = 0
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "center")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager))
  ) %>%
  
  #center-align all other columns (except manager)
  tab_style(
    style = cell_text(align = "center", weight = "bold"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  )

club_table

gtsave(club_table, filename = here("Results/25_26", "top_clubs.png"))


## Plot shared player data -----------------------------------------------------
message("Plot shared player data")

#fix managers capitalization
shared_players2 <- shared_players %>%
  mutate(player = str_to_title(str_replace_all(player, "_", "-")),
         managers = str_split(managers, pattern = ",") %>%   # split by comma
           map(~ str_to_title(trimws(.x))) %>%               # trim spaces & title case
           map_chr(~ paste(.x, collapse = ", "))             # join back with comma + space
  )


#create gt table
shared_players_table <- shared_players2 %>%
  
  gt() %>%
  
  cols_label(
    player = "Player",
    num_managers = "Number of Managers",
    managers = "Managers"
  ) %>%
  
  #add a title to the table
  tab_header(
    title = "Most Shared Players 2025/26"
  ) %>%
  
  #format number of managers column to have 0 decimal places
  fmt_number(
    columns = c(num_managers),
    decimals = 0
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "center")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(everything())
  ) %>%
  
  #center-align all other columns
  tab_style(
    style = cell_text(align = "center", weight = "bold", color = "#381B58"),
    locations = cells_body(columns = everything())
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  )

shared_players_table

gtsave(shared_players_table, filename = here("Results/25_26", "shared_players.png"))


## Plot preferred formation data -----------------------------------------------
message("Plot preferred formation data")

#top player per position
top_formation_table <- pref_form %>%
  #Capitalise text columns
  mutate(manager = str_to_title(manager)) %>%
  
  #create the gt table
  gt() %>%
  
  #add a title to the table
  tab_header(
    title = "Preferred Formation 2025/26"
  ) %>%
  
  #format times_used column to have 0 decimal places
  fmt_number(
    columns = times_used,
    decimals = 0
  ) %>%
  
  #rename columns for a cleaner table display
  cols_label(
    manager = "Manager",
    pref_formation = "Formation",
    times_used = "Times used"
  ) %>%
  
  #style the table title
  tab_style(
    style = cell_text(color = "#381B58", weight = "bold", size = px(30)),
    locations = cells_title(groups = "title")  # Target the title
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "center")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(-c(manager))
  ) %>%
  
  #style the column labels (headers)
  tab_style(
    style = list(
      cell_fill(color = "#381B58"),  # Set the background color
      cell_text(weight = "bold", color = "#00FF87", align = "left")  # Optional: keep text bold & black
    ),
    locations = cells_column_labels(c(manager))
  ) %>%
  
  #style the 'manager' column in the table body
  tab_style(
    style = cell_text(weight = "bold", color = "#381B58"),
    locations = cells_body(columns = c(manager))
  ) %>%
  
  #center-align all other columns (except manager)
  tab_style(
    style = cell_text(align = "center" , color = "#381B58"),
    locations = cells_body(columns = -c(manager))
  ) %>%
  
  #table options for layout
  tab_options(
    data_row.padding = px(10),
    column_labels.padding = px(20),
    table.width = pct(70)
  )

top_formation_table

gtsave(top_formation_table, filename = here("Results/25_26", "preferred_formation.png"))


## Cumulative plots ------------------------------------------------------------
message("Cumulative plots")

#league points
league_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_league_points, color = manager, group = manager)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  labs(title = "Cumulative League Points", x = "Gameweek", y = "League Points", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
    legend.title = element_text(size = 38, face = "bold"),
    plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 38, color = "black"),
    axis.title = element_text(size = 42),
    axis.title.y = element_text(size = 42, vjust = 1.5),
    axis.text.x = element_text(size = 38, color = "black"),
    legend.text = element_text(size = 38, face = "bold"),
    legend.key.height= unit(1, 'cm'),
    legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373")) +
  coord_cartesian(xlim= c(1,38))

#manager points
manager_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_manager_points, color = manager, group = manager)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  labs(title = "Cumulative Total Points", x = "Gameweek", y = "Total Points", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373")) +
  coord_cartesian(xlim= c(1,38))

#bench points lost
bench_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_actual_bench_points_lost, color = manager, group = manager)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  labs(title = "Cumulative Points Left on Bench", x = "Gameweek", y = "Points Left on Bench", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373")) +
  coord_cartesian(xlim= c(1,38))

#league points lost
league_points_dropped <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_league_points_lost, color = manager, group = manager)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  labs(title = "Cumulative Points Dropped\n(due to points left on bench)", x = "Gameweek", y = "Points Dropped", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373")) +
  coord_cartesian(xlim= c(1,38))

#league position
league_position <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = weekly_position, color = manager, group = manager)) +
  geom_line(size = 3) +
  geom_point(size = 6) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30, 35)) +
  scale_y_reverse(breaks = c(1, 2, 3, 4, 5, 6)) +
  labs(title = "League Position", x = "Gameweek", y = "Position", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373")) +
  coord_cartesian(xlim= c(1,38))

#save plots
ggsave(here("Results/25_26", "matchup_points.png"), league_points, units = "cm", width = 60, height = 30)
ggsave(here("Results/25_26", "overall_points.png"), manager_points, units = "cm", width = 60, height = 30)
ggsave(here("Results/25_26", "bench_points_lost.png"), bench_points, units = "cm", width = 60, height = 30)
ggsave(here("Results/25_26", "matchup_points_dropped.png"), league_points_dropped, units = "cm", width = 60, height = 30)
ggsave(here("Results/25_26", "league_position.png"), league_position, units = "cm", width = 60, height = 30)


## Luck (median score) ---------------------------------------------------------
message("Luck (median score)")

#get all managers first (from the original data)
all_managers <- luck %>%
  mutate(manager = str_to_title(manager)) %>%
  filter(!is.na(manager)) %>%
  distinct(manager)

#now build the count table including zeros for missing combos
luck_plot_data <- luck %>%
  mutate(
    manager = str_to_title(manager),
    luck = ifelse(is.na(luck), "Neutral", luck)
  ) %>%
  filter(luck != "Neutral") %>%
  count(manager, luck) %>%
  complete(manager = all_managers$manager, luck = c("Lucky", "Unlucky"), fill = list(n = 0))


luck_median <- ggplot(luck_plot_data, aes(x = manager, y = n, fill = luck)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Lucky" = "darkgreen", "Unlucky" = "red3")) +
  labs(title = "Luck (based on median league score)", fill = "", x = "", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black", face = "bold"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'))

#save plots
ggsave(here("Results/25_26", "luck_median.png"), luck_median, units = "cm", width = 40, height = 30)


## Luck (managerial matchup) ---------------------------------------------------------
message("Luck (managerial matchup)")

#pivot longer and capitalise manager names
luck_managerial_long <- luck_managerial %>%
  pivot_longer(cols = c(lucky, bad_manager),
               names_to = "luck_type",
               values_to = "count") %>%
  mutate(
    manager = str_to_title(manager),   # Capitalise names
    luck_type = recode(luck_type,
                       "lucky" = "Benefited",
                       "bad_manager" = "Bad Managerial Choices"),
    luck_type = factor(luck_type, levels = c("Benefited", "Bad Managerial Choices"))
  )


luck_managerial_plot <- ggplot(luck_managerial_long, aes(x = manager, y = count, fill = luck_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Benefited" = "darkgreen", "Bad Managerial Choices" = "red3")) +
  labs(title = "Luck (based on managerial choices)", fill = "", x = "", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black", face = "bold"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'))

#save plots
ggsave(here("Results/25_26", "luck_managerial.png"), luck_managerial_plot, units = "cm", width = 40, height = 30)


## Waiver plots ----------------------------------------------------------------
message("Waiver plots")

#pivot longer and capitalise manager names
waiver_long <- waiver_summary %>%
  pivot_longer(cols = c(def, mid, fwd, gkp),
               names_to = "position",
               values_to = "count") %>%
  mutate(
    manager = str_to_title(manager),   #capitalise names
    position = recode(position,
                      "def" = "Defender",
                      "mid" = "Midfielder",
                      "fwd" = "Forward",
                      "gkp" = "Goalkeeper"),
    position = factor(position, levels = c("Defender", "Midfielder", "Forward", "Goalkeeper"))
  ) %>%
  select(-c(total_transfers))


waiver_details_plot <- ggplot(waiver_long, aes(x = manager, y = count, fill = position)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  #scale_fill_manual(values = c("Benefited" = "darkgreen", "Bad Managerial Choices" = "red3")) +
  labs(title = "Transfers by position", fill = "", x = "", y = "Count") +
  theme_bw(base_size = 14) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black", face = "bold"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm'))

#save plots
ggsave(here("Results/25_26", "waiver_details.png"), waiver_details_plot, units = "cm", width = 40, height = 30)

#total waivers
waiver_plot <- ggplot(data = waiver_summary, aes(x = str_to_title(manager), y = total_transfers, color = manager, fill = manager)) +
  geom_col() +
  labs(title = "Total transfers", x = "", y = "Count", color = "", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373"),
                     labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez")) +
  scale_fill_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373"),
                    labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"))

#save plots
ggsave(here("Results/25_26", "waiver_overall.png"), waiver_plot, units = "cm", width = 40, height = 30)


## Auto sub plots ----------------------------------------------------------------
message("Auto sub plots")

auto_sub_plot <- ggplot(data = auto_subs, aes(x = str_to_title(manager), y = automatic_subs, color = manager, fill = manager)) +
  geom_col() +
  labs(title = "Total automatic substitutions", x = "", y = "Count", color = "", fill = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 38, face = "bold"),
        plot.title = element_text(size = 42, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 38, color = "black"),
        axis.title = element_text(size = 42),
        axis.title.y = element_text(size = 42, vjust = 1.5),
        axis.text.x = element_text(size = 38, color = "black"),
        legend.text = element_text(size = 38, face = "bold"),
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(1, 'cm')) +
  scale_color_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373"),
                     labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez")) +
  scale_fill_manual(values = c("#D1352B", "#4A7CB3", "#68AD57", "#8E529F", "#EF8632", "#737373"),
                    labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"))

#save plots
ggsave(here("Results/25_26", "auto_subs.png"), auto_sub_plot, units = "cm", width = 40, height = 30)
