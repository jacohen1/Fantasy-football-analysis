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
file_path <- here("Data", "Fantasy_data_2526.xlsx")

#get all sheet names
sheet_names <- excel_sheets(file_path)

#read each sheet into its own data frame
for(sheet in sheet_names){
  assign(sheet, read_excel(file_path, sheet = sheet))
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

##optimised league table
league_table_optimised <- get_optimised_league_table(weekly_data = weekly_data, matchups = matchups)

##top players
top_position <- get_top_scorers_by_position(weekly_data = weekly_data)
top_player <- get_top_overall_player(weekly_data = weekly_data)
top_player_per_appearance <- get_top_per_appearance(weekly_data = weekly_data, min_appearances = 1)

##top club
top_club <- get_top_club_per_manager(weekly_data = weekly_data)

##most shared player
shared_players <- get_most_managers_per_player(weekly_data = weekly_data, list_length = 15)

##waivers
waiver_summary <- get_waiver_summary(waivers = waivers)

## Plot tables ---------------------------------------------------------------
message("Plot tables")

##league table
#create gt table
league_table_table <- league_table %>%
  arrange(desc(total_league_points)) %>%
  mutate(manager = str_to_title(manager)) %>%
  gt() %>%
  tab_header(
    title = "Better Late than Never"
  ) %>%
  fmt_number(
    columns = c(total_league_points, total_manager_points),
    decimals = 0
  ) %>%
  cols_label(
    rank = "Rank",
    manager = "Manager",
    W = "W",
    D = "D",
    L = "L",
    total_manager_points = "+",
    total_league_points = "Pts"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(columns = c(manager))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    data_row.padding = px(10), #increase row padding
    column_labels.padding = px(20), #increase header padding
    table.width = pct(70)   
  )

gtsave(league_table_table, filename = "league_table.png")

##league table
#create gt table
opt_league_table_table <- league_table_optimised %>%
  arrange(desc(total_optimised_league_points)) %>%
  mutate(manager = str_to_title(manager)) %>%
  gt() %>%
  tab_header(
    title = "Better Late than Never"
  ) %>%
  fmt_number(
    columns = c(total_optimised_league_points, total_optimised_manager_points),
    decimals = 0
  ) %>%
  cols_label(
    rank = "Rank",
    manager = "Manager",
    W = "W",
    D = "D",
    L = "L",
    total_optimised_league_points = "Pts",
    total_optimised_manager_points = "+"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(columns = c(manager))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    data_row.padding = px(10), #increase row padding
    column_labels.padding = px(20), #increase header padding
    table.width = pct(70)   
  )

gtsave(opt_league_table_table, filename = "optimised_league_table.png")

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
  geom_tile(color = "black", size = 0.5) +
  geom_text(aes(label = wins), color = "black", size = 8) +
  scale_x_discrete(labels = c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"), position = "top") +
  scale_y_discrete(labels = rev(c("Benji", "Jacob", "Josh", "Michael", "Miz", "Nez"))) +
  scale_fill_gradient(low = "white", high = "white") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "darkgreen", face = "bold", size = 20),
    axis.text.y = element_text(color = "red", face = "bold", size = 20),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "none"
  )

top_position_table <- top_position %>%
  mutate(
    manager = str_to_title(manager),
    gkp = str_to_title(str_replace_all(gkp, "_", "-")),
    def = str_to_title(str_replace_all(def, "_", "-")),
    mid = str_to_title(str_replace_all(mid, "_", "-")),
    fwd = str_to_title(str_replace_all(fwd, "_", "-"))
  ) %>%
  gt() %>%
  tab_header(
    title = "Top Scorers by Position"
  ) %>%
  fmt_number(
    columns = ends_with("_points"),
    decimals = 0
  ) %>%
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
  tab_options(
    data_row.padding = px(8),
    table.width = pct(100)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(columns = c(manager))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = -c(manager))
  ) %>%
  tab_style(
    style = cell_text(align = "center", weight = "bold", color = "black"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    data_row.padding = px(10), #increase row padding
    column_labels.padding = px(20), #increase header padding
    table.width = pct(70)   
  ) %>% tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(columns = c("manager", "gkp_points", "def_points", "mid_points"))
  )

gtsave(top_position_table, filename = "top_position.png")

## Cumulative plots ------------------------------------------------------------
message("Cumulative plots")

league_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_league_points, color = manager)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Cumulative League Points", x = "Gameweek", y = "League Points", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim= c(1,38))

manager_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_manager_points, color = manager)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Cumulative Total Points", x = "Gameweek", y = "Total Points", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim= c(1,38))

bench_points <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_actual_bench_points_lost, color = manager)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Cumulative Bench Points", x = "Gameweek", y = "Bench Points", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim= c(1,38))

league_points_dropped <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = cumulative_league_points_lost, color = manager)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Cumulative Points Dropped\n(due to points left on bench)", x = "Gameweek", y = "Points Dropped", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim= c(1,38))

league_position <- ggplot(data = cumulative_totals[which(!is.na(cumulative_totals$manager)), ], aes(x = gameweek, y = weekly_position, color = manager)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "League Position", x = "Gameweek", y = "Position", color = "Manager") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(xlim= c(1,38))


## Bar graphs ------------------------------------------------------------------
message("Bar graphs")

##luck
# Get all managers first (from the original data)
all_managers <- luck %>%
  mutate(manager = str_to_title(manager)) %>%
  filter(!is.na(manager)) %>%
  distinct(manager)

# Now build the count table including zeros for missing combos
luck_plot_data <- luck %>%
  mutate(
    manager = str_to_title(manager),
    luck = ifelse(is.na(luck), "Neutral", luck)
  ) %>%
  filter(luck != "Neutral") %>%
  count(manager, luck) %>%
  complete(manager = all_managers$manager, luck = c("Lucky", "Unlucky"), fill = list(n = 0))


ggplot(luck_plot_data, aes(x = manager, y = n, fill = luck)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Lucky" = "darkgreen", "Unlucky" = "red3")) +
  labs(title = "Luck", fill = "", x = "", y = "") +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        legend.text = element_text(face = "bold", size = 15),
        axis.text.x = element_text(color = "black", face = "bold", size = 15),
        axis.text.y = element_text(color = "black", face = "bold", size = 15))

