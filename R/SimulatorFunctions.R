
# Generally Useful Functions ----------------------------------------------

clean_names <- function(data){
  x <- data
  x <- gsub("\\s+(II|III|IV|V|Jr\\.|Sr\\.)$", "", x)
  x <- sub(" (Q|IR|O|SUSP|D)$", "", x)
  x <- gsub("\\.", "", x)
  x
}


age_joiner <- function(pff_data){
  agedat <- nflreadr::load_players() |>
    dplyr::select(display_name, pff_id, birth_date)
  new_dat <- suppressMessages(left_join(pff_data, agedat))
  new_dat <- new_dat |>
    mutate(birthyear = substr(birth_date, 1, 4))
  new_dat$birthyear <- as.numeric(new_dat$birthyear)
  new_dat <- new_dat |>
    mutate(Age = Year - birthyear)
  new_dat
}

# Depth Chart Functions ----------------------------------------------

#' Returns the most updated depth chart for a selected NFL team
#'
#' @param team A two to three letter character string abbreviation for NFL teams
#'    Must be one of the following abbreviations:
#'    "ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL"
#'    "DEN" "DET" "GB"  "HOU" "IND" "JAX" "KC"  "LA"  "LAC"
#'    "LV"  "MIA" "MIN" "NE"  "NO"  "NYG" "NYJ" "PHI" "PIT"
#'    "SEA" "SF"  "TB"  "TEN" "WAS"
#'@return A data frame representation of the depth chart with each players position
#'
depthchart <- function(tm){
  baseteamchart <- nflreadr::load_depth_charts() |>
    filter(team==tm & dt == dt[1]) |>
    filter(!pos_abb %in% c("PK", "P", "H", "PR", "KR", "LS")) |>
    dplyr::select(team, player_name, espn_id, pos_abb, pos_rank)
  allplayers <- nflreadr::load_players() |>
    dplyr::select(pff_id, espn_id)
  newchart <- suppressMessages(left_join(baseteamchart, allplayers))
  newchart <- newchart |>
    mutate(position = pos_abb,
           player = player_name,
           final_position = paste0(pos_abb, pos_rank)) |>
    dplyr::select(position, player, final_position, pff_id)
  newchart
}

#' Returns the most updated ESPN depth chart for a selected NFL team with
#' projected snaps for 2025
#'
#' @param team A two to three letter character string abbreviation for NFL teams
#'    Must be one of the following abbreviations:
#'    "ARI" "ATL" "BAL" "BUF" "CAR" "CHI" "CIN" "CLE" "DAL"
#'    "DEN" "DET" "GB"  "HOU" "IND" "JAX" "KC"  "LA"  "LAC"
#'    "LV"  "MIA" "MIN" "NE"  "NO"  "NYG" "NYJ" "PHI" "PIT"
#'    "SEA" "SF"  "TB"  "TEN" "WAS"
#' @param def_assignment A string of what type of defensive assignment you want
#' snaps for
#'    Must be one of the following:
#'    "coverage", "run_defense", "pass_rush"
#'@return A data frame representation of the depth chart with each players position
#'

snaps_to_depth_chart <- function(team, def_assignment){
  dpt_cht <- depthchart(team)
  dpt_cht$cleaned_player <- clean_names(dpt_cht$player)
  dpt_cht <- dpt_cht |>
    mutate(position = case_when(
      position %in% c("LT", "LG", "C", "RG", "RT") ~ "OL",
      position %in% c("LDE", "NT", "RDE", "WLB", "LILB", "RILB", "SLB",
                      "LDT", "RDT", "MLB") ~ "F7",
      position %in% c("LCB", "SS", "FS", "RCB", "NB") ~ "DB",
      TRUE ~ position
    ))
  dpt_cht <- dpt_cht |>
    mutate(depth_next = case_when(
      final_position %in% c("WR1", "WR2", "WR3") ~ "WR1",
      final_position %in% c("WR4", "WR5", "WR6") ~ "WR2",
      final_position %in% c("WR7", "WR8", "WR9") ~ "WR3",
      final_position %in% c("WR10", "WR11", "WR12") ~ "WR4",
      final_position %in% c("LT1", "LG1", "C1", "RG1", "RT1") ~ "OL1",
      final_position %in% c("LT2", "LG2", "C2", "RG2", "RT2") ~ "OL2",
      final_position %in% c("LT3", "LG3", "C3", "RG3", "RT3") ~ "OL3",
      final_position %in% c("LDE1", "NT1", "RDE1", "WLB1", "LILB1", "RILB1", "SLB1",
                            "LDT1", "RDT1", "MLB1") ~ "F71",
      final_position %in% c("LDE2", "NT2", "RDE2", "WLB2", "LILB2", "RILB2", "SLB2",
                            "LCB2", "LDT2", "RDT2", "MLB2") ~ "F72",
      final_position %in% c("LDE3", "NT3", "RDE3", "WLB3", "LILB3", "RILB3", "SLB3",
                            "LDT3", "RDT3", "MLB3") ~ "F73",
      final_position %in% c("LCB1", "SS1", "FS1", "RCB1", "NB1") ~ "DB1",
      final_position %in% c("LCB2", "SS2", "FS2", "RCB2", "NB2") ~ "DB2",
      final_position %in% c("LCB3", "SS3", "FS3", "RCB3", "NB3") ~ "DB3",
      TRUE ~ final_position
    ))

  ### QBs
  qbPFF <- qbsnapsdf2024
  qbPFF$cleaned_player <- clean_names(qbPFF$player)
  qbPFF <- qbPFF[-1]
  dpt_cht <- suppressMessages(left_join(dpt_cht, qbPFF))
  dpt_cht <- dpt_cht |>
    mutate(mean_att = if_else(position=="QB" & is.na(mean_att), 1, mean_att))
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_att"] <- "attempts"
  dpt_cht_QB <- dpt_cht |>
    filter(position=="QB") |>
    mutate(depth_next = "QB")
  dpt_cht_QB$predicted_QB_attempts = predict(rush_att_model,dpt_cht_QB)
  dpt_cht_QB <- dpt_cht_QB |>
    dplyr::select(-c(depth_next, attempts))
  dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_QB))
  dpt_cht <- dpt_cht |>
    select(-attempts)

  ### RBs
  rushingPFF <- rushdf
  rushingPFF$cleaned_player <- clean_names(rushingPFF$player)
  rushingPFF <- rushingPFF[-1]
  dpt_cht <- suppressMessages(left_join(dpt_cht, rushingPFF))
  dpt_cht <- dpt_cht |>
    mutate(mean_att = if_else(position=="RB" & is.na(mean_att), 1, mean_att))
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_att"] <- "attempts"
  dpt_cht_RB <- dpt_cht |>
    filter(position=="RB") |>
    filter(depth_next %in% c("RB1", "RB2", "RB3"))
  dpt_cht_RB$predicted_attempts = predict(rush_att_model,dpt_cht_RB)
  dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_RB))

  ### WRs/TEs
  receivingPFF <- recdf
  receivingPFF$cleaned_player <- clean_names(receivingPFF$player)
  receivingPFF <- receivingPFF[-1]
  dpt_cht <- suppressMessages(left_join(dpt_cht, receivingPFF))
  dpt_cht <- dpt_cht |>
    mutate(mean_routes = if_else(position %in% c("WR", "TE") & is.na(mean_routes), 5, mean_routes))
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_routes"] <- "routes"
  dpt_cht_rec <- dpt_cht |>
    filter(position %in% c("WR", "TE")) |>
    filter(depth_next %in% c("WR1", "WR2", "WR3", "TE1", "TE2", "TE3"))
  dpt_cht_rec$predicted_routes = predict(rec_route_model,dpt_cht_rec)
  dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_rec))

  ### OL
  blockingPFF <- blockdf
  blockingPFF$cleaned_player <- clean_names(blockingPFF$player)
  blockingPFF <- blockingPFF[-1]
  dpt_cht <- suppressMessages(left_join(dpt_cht, blockingPFF))
  dpt_cht <- dpt_cht |>
    mutate(mean_snaps = if_else(position=="OL" & is.na(mean_snaps), 5, mean_snaps))
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_snaps"] <- "snap_counts_offense"
  dpt_cht_block <- dpt_cht |>
    filter(position %in% c("OL")) |>
    filter(depth_next %in% c("OL1", "OL2", "OL3"))
  dpt_cht_block$predicted_snaps = predict(block_model,dpt_cht_block)
  dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_block))

  ### Defense
  defPFF <- defdf
  defPFF$cleaned_player <- clean_names(defPFF$player)
  defPFF <- defPFF[-1]
  dpt_cht <- suppressMessages(left_join(dpt_cht, defPFF))
  dpt_cht <- dpt_cht |>
    mutate(mean_cov_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_cov_snaps),
                                    5, mean_cov_snaps),
           mean_pass_rush_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_pass_rush_snaps),
                                    5, mean_pass_rush_snaps),
           mean_run_def_snaps = if_else(position %in% c("F7", "DB") & is.na(mean_run_def_snaps),
                                    5, mean_run_def_snaps))
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_cov_snaps"] <- "snap_counts_coverage"
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_run_def_snaps"] <- "snap_counts_run_defense"
  colnames(dpt_cht)[colnames(dpt_cht)=="mean_pass_rush_snaps"] <- "snap_counts_pass_rush"
  dpt_cht_def <- dpt_cht |>
    filter(position %in% c("F7", "DB")) |>
    filter(depth_next %in% c("F71", "F72", "F73", "DB1", "DB2", "DB3"))
  dpt_cht_def$predicted_cov_snaps = predict(def_cover_model,dpt_cht_def)
  dpt_cht_def$predicted_run_def_snaps = predict(def_run_model,dpt_cht_def)
  dpt_cht_def$predicted_pass_rush_snaps = predict(def_pass_rush_model,dpt_cht_def)
  dpt_cht <- suppressMessages(left_join(dpt_cht, dpt_cht_def))

  dpt_cht <- dpt_cht |>
    mutate(final_snap_ct = case_when(
      def_assignment=="coverage" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_coverage),
      def_assignment=="run_defense" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_run_defense),
      def_assignment=="pass_rush" ~ coalesce(attempts, routes, snap_counts_offense, snap_counts_pass_rush)
    ),
    final_predicted_snap_ct = case_when(
      def_assignment=="coverage" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
                                            predicted_snaps, predicted_cov_snaps),
      def_assignment=="run_defense" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
                                               predicted_snaps, predicted_run_def_snaps),
      def_assignment=="pass_rush" ~ coalesce(predicted_QB_attempts, predicted_attempts, predicted_routes,
                                             predicted_snaps, predicted_pass_rush_snaps)
    ))

  dpt_cht_final <- dpt_cht[c("position", "player", "cleaned_player", "final_position",
                             "depth_next", "pff_id",
                              "final_predicted_snap_ct")]
  dpt_cht_final <- dpt_cht_final |>
    filter(!(position!="QB" & is.na(final_predicted_snap_ct))) |>
    filter(player!="-")
  dpt_cht_final
}

# PFF Grade Predictor Functions -------------------------------------------
QB_PFF_grade <- function(team, year){
  last_varname <- paste0("PFF_QBs_", year - 1)
  last_data <- get(last_varname, envir = parent.env(environment()))
  PFF_passing_year_last <- last_data |>
    filter(position=="QB" & attempts >= 10) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              pass_grade_mean_last = mean(grades_pass, na.rm = T),
              pass_grade_sd = sd(grades_pass, na.rm = T),
              rush_grade_mean_last = mean(grades_run, na.rm = T),
              rush_grade_sd = sd(grades_run, na.rm = T),
              games_last = n())
  cur_varname <- paste0("PFF_QBs_", year)
  cur_data <- get(cur_varname, envir = parent.env(environment()))
  PFF_passing_year_cur <- cur_data
  if(nrow(PFF_passing_year_cur) > 1){
    PFF_passing_year_cur <- PFF_passing_year_cur |>
      filter(position=="QB" & attempts >= 10) |>
      group_by(player_id) |>
      summarise(player = player[1],
                Week = Week[1],
                pass_grade_mean_cur = mean(grades_pass, na.rm = T),
                rush_grade_mean_cur = mean(grades_run, na.rm = T),
                pass_grade_sd_cur = sd(grades_pass, na.rm = T),
                rush_grade_sd_cur = sd(grades_run, na.rm = T),
                games_cur = n())
    total_pff <- suppressMessages(full_join(PFF_passing_year_last, PFF_passing_year_cur))
    cur_weight <- unique(PFF_passing_year_cur$Week) * .2
    cur_weight <- max(1, cur_weight)
    total_pff <- total_pff |>
      mutate(pass_grade_mean = case_when(
        is.na(pass_grade_mean_last) ~ pass_grade_mean_cur,
        is.na(pass_grade_mean_cur) ~ pass_grade_mean_last,
        TRUE ~ (cur_weight * pass_grade_mean_cur) +
          ((1 - cur_weight) * pass_grade_mean_last)
      ),
             rush_grade_mean = case_when(
               is.na(rush_grade_mean_last) ~ rush_grade_mean_cur,
               is.na(rush_grade_mean_cur) ~ rush_grade_mean_last,
               TRUE ~ (cur_weight * rush_grade_mean_cur) +
                 ((1 - cur_weight) * rush_grade_mean_last)
             ),
             games = case_when(
               is.na(games_last) ~ games_cur,
               is.na(games_cur) ~ games_last,
               (games_cur + games_last) <= 17 ~ (games_cur + games_last),
               TRUE ~ 17
             ))
  } else{
    total_pff <- PFF_passing_year_last |>
      mutate(pass_grade_mean = pass_grade_mean_last,
             rush_grade_mean = rush_grade_mean_last,
             games = games_last)
  }
  colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
  total_pff$pff_id <- as.character(total_pff$pff_id)
  with_age <- total_pff
  with_age <- with_age |>
    mutate(Year = if_else(is.na(Year), year, Year),
           position = if_else(is.na(position), "QB", position))
  with_age <- age_joiner(with_age)
  with_age <- with_age |>
    mutate(Age_next = Age + 1) |>
    filter(position == "QB")
  pass_predictions <- predict(PFF_passing_grade_mean_model, with_age)
  rush_predictions <- predict(PFF_QB_rushing_grade_mean_model, with_age)
  finaldf <- tibble(pff_id = with_age$pff_id,
                    pass_grade_mean = pass_predictions,
                    rush_grade_mean = rush_predictions,
                    pass_grade_sd = if_else(with_age$games_cur >= 3,
                                            with_age$pass_grade_sd_cur,
                                            with_age$pass_grade_sd),
                    rush_grade_sd = if_else(with_age$games_cur >= 3,
                                            with_age$rush_grade_sd_cur,
                                            with_age$rush_grade_sd))

  ### DEPTH CHART PART
  quarterback <- snaps_to_depth_chart(team, "coverage") |>
    filter(final_position == "QB1")

  combined <- suppressMessages(left_join(quarterback, finaldf))
  combined <- combined |>
    mutate(rush_grade_mean = if_else(is.na(rush_grade_mean), 60, rush_grade_mean),
           rush_grade_sd = if_else(is.na(rush_grade_sd), 15, rush_grade_sd),
           pass_grade_mean = if_else(is.na(pass_grade_mean), 61.5, pass_grade_mean),
           pass_grade_sd = if_else(is.na(pass_grade_sd), 17.5, pass_grade_sd))
  combined
}

RB_PFF_grade <- function(team, year){
  last_rush_varname <- paste0("PFF_Rushing_", year - 1)
  last_rush_data <- get(last_rush_varname, envir = parent.env(environment()))
  PFF_year_last <- last_rush_data |>
    filter(position=="HB" & attempts >= 5) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              rush_grade_mean_last = mean(grades_run, na.rm = T),
              rush_grade_sd = sd(grades_run, na.rm = T),
              games_last = n())
  last_rec_varname <- paste0("PFF_Receiving_", year - 1)
  last_rec_data <- get(last_rec_varname, envir = parent.env(environment())) |>
    filter(position=="HB") |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              rec_grade_mean_last = mean(grades_pass_route, na.rm = T),
              rec_grade_sd = sd(grades_pass_route, na.rm = T))
  PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_rec_data))
  last_block_varname <- paste0("PFF_Blocking_", year - 1)
  last_block_data <- get(last_block_varname, envir = parent.env(environment())) |>
    filter(position=="HB") |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
              pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
              run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
              run_block_grade_sd = sd(grades_run_block, na.rm = T))
  PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_block_data))
  PFF_year_last <- PFF_year_last |>
    mutate(position = if_else(position=="HB", "RB", position))
  cur_rush_varname <- paste0("PFF_Rushing_", year)
  cur_rush_data <- get(cur_rush_varname, envir = parent.env(environment()))
  PFF_year_cur <- cur_rush_data
  if(nrow(PFF_year_cur) > 1){
    PFF_year_cur <- PFF_year_cur |>
      filter(position=="HB" & attempts >= 5) |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                rush_grade_mean_cur = mean(grades_run, na.rm = T),
                rush_grade_sd_cur = sd(grades_run, na.rm = T),
                games_cur = n())
    cur_rec_varname <- paste0("PFF_Receiving_", year)
    cur_rec_data <- get(cur_rec_varname, envir = parent.env(environment())) |>
      filter(position=="HB") |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                rec_grade_mean_cur = mean(grades_pass_route, na.rm = T),
                rec_grade_sd_cur = sd(grades_pass_route, na.rm = T))
    PFF_year_cur <- suppressMessages(left_join(PFF_year_cur, cur_rec_data))
    cur_block_varname <- paste0("PFF_Blocking_", year)
    cur_block_data <- get(cur_block_varname, envir = parent.env(environment())) |>
      filter(position=="HB") |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
                run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
                pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
                run_block_grade_sd_cur = sd(grades_run_block, na.rm = T))
    PFF_year_cur <- suppressMessages( left_join(PFF_year_cur, cur_block_data))
    PFF_year_cur <- PFF_year_cur |>
      mutate(position = if_else(position=="HB", "RB", position))
    total_pff <- suppressMessages(full_join(PFF_year_last, PFF_year_cur))
    cur_weight <- unique(PFF_year_cur$Week) * .2
    cur_weight <- max(1, cur_weight)
    total_pff <- total_pff |>
      mutate(rush_grade_mean = case_when(
        is.na(rush_grade_mean_last) ~ rush_grade_mean_cur,
        is.na(rush_grade_mean_cur) ~ rush_grade_mean_last,
        TRUE ~ (cur_weight * rush_grade_mean_cur) +
          ((1 - cur_weight) * rush_grade_mean_last)
      ),
             rec_grade_mean = case_when(
               is.na(rec_grade_mean_last) ~ rec_grade_mean_cur,
               is.na(rec_grade_mean_cur) ~ rec_grade_mean_last,
               TRUE ~ (cur_weight * rec_grade_mean_cur) +
                 ((1 - cur_weight) * rec_grade_mean_last)
             ),
             pass_block_grade_mean = case_when(
               is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
               is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
               TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
                 ((1 - cur_weight) * pass_block_grade_mean_last)
             ),
             run_block_grade_mean = case_when(
               is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
               is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
               TRUE ~ (cur_weight * run_block_grade_mean_cur) +
                 ((1 - cur_weight) * run_block_grade_mean_last)
             ),
             games = case_when(
               is.na(games_last) ~ games_cur,
               is.na(games_cur) ~ games_last,
               (games_cur + games_last) <= 17 ~ (games_cur + games_last),
               TRUE ~ 17
             ))
  } else{
    total_pff <- PFF_year_last |>
      mutate(rush_grade_mean = rush_grade_mean_last,
             rec_grade_mean = rec_grade_mean_last,
             pass_block_grade_mean = pass_block_grade_mean_last,
             run_block_grade_mean = run_block_grade_mean_last,
             games = games_last)
  }
  colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
  total_pff$pff_id <- as.character(total_pff$pff_id)

  with_age <- total_pff
  with_age <- with_age |>
    mutate(Year = if_else(is.na(Year), year, Year),
           position = if_else(is.na(position), "RB", position))
  with_age <- age_joiner(with_age)
  with_age <- with_age |>
    mutate(Age_next = Age + 1)
  rush_predictions <- predict(PFF_rushing_grade_mean_model, with_age)
  rec_predictions <- predict(PFF_receiving_grade_mean_model, with_age)
  passblockdf <- with_age |>
    rename(block_grade_mean = pass_block_grade_mean)
  passblock_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
  runblockdf <- with_age |>
    rename(block_grade_mean = run_block_grade_mean)
  runblock_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
  finaldf <- tibble(pff_id = with_age$pff_id,
                    rush_grade_mean = rush_predictions,
                    rush_grade_sd = if_else(with_age$games_cur >= 3,
                                            with_age$rush_grade_sd_cur,
                                            with_age$rush_grade_sd),
                    rec_grade_mean = rec_predictions,
                    rec_grade_sd = if_else(with_age$games_cur >= 3,
                                           with_age$rec_grade_sd_cur,
                                           with_age$rec_grade_sd),
                    pass_block_grade_mean = passblock_predictions,
                    pass_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                  with_age$pass_block_grade_sd_cur,
                                                  with_age$pass_block_grade_sd),
                    run_block_grade_mean = runblock_predictions,
                    run_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                 with_age$run_block_grade_sd_cur,
                                                 with_age$run_block_grade_sd))

  ### DEPTH CHART PART
  runningbacks <- snaps_to_depth_chart(team, "coverage") |>
    filter(position == "RB")

  combined <- suppressMessages(left_join(runningbacks, finaldf))
  combined <- combined |>
    mutate(rush_grade_mean = if_else(is.na(rush_grade_mean), 62, rush_grade_mean),
           rush_grade_sd = if_else(is.na(rush_grade_sd), 17.5, rush_grade_sd),
           rec_grade_mean = if_else(is.na(rec_grade_mean), 59, rec_grade_mean),
           rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
           pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 57.5, pass_block_grade_mean),
           pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 12.5, pass_block_grade_sd),
           run_block_grade_mean = if_else(is.na(run_block_grade_mean), 57.5, run_block_grade_mean),
           run_block_grade_sd = if_else(is.na(run_block_grade_sd), 12.5, run_block_grade_sd))
  combined
}

WRorTE_PFF_grade <- function(team, year, pos){
  last_rec_varname <- paste0("PFF_Receiving_", year - 1)
  last_rec_data <- get(last_rec_varname, envir = parent.env(environment()))
  PFF_year_last <- last_rec_data |>
    filter(position==pos & routes >= 7) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              rec_grade_mean_last = mean(grades_pass_route, na.rm = T),
              rec_grade_sd = sd(grades_pass_route, na.rm = T),
              games_last = n())
  last_block_varname <- paste0("PFF_Blocking_", year - 1)
  last_block_data <- get(last_block_varname, envir = parent.env(environment())) |>
    filter(position==pos) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
              pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
              run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
              run_block_grade_sd = sd(grades_run_block, na.rm = T))
  PFF_year_last <- suppressMessages(left_join(PFF_year_last, last_block_data))

  cur_rec_varname <- paste0("PFF_Receiving_", year)
  cur_rec_data <- get(cur_rec_varname, envir = parent.env(environment()))
  PFF_year_cur <- cur_rec_data
  if(nrow(PFF_year_cur) > 1){
    PFF_year_cur <- PFF_year_cur |>
      filter(position==pos & routes >= 7) |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                rec_grade_mean_cur = mean(grades_pass_route, na.rm = T),
                rec_grade_sd_cur = sd(grades_pass_route, na.rm = T),
                games_cur = n())
    cur_block_varname <- paste0("PFF_Blocking_", year)
    cur_block_data <- get(cur_block_varname, envir = parent.env(environment())) |>
      filter(position==pos) |>
      group_by(player_id) |>
      summarise(player = player[1],
                pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
                pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
                run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
                run_block_grade_sd_cur = sd(grades_run_block, na.rm = T))
    PFF_year_cur <- suppressMessages(left_join(PFF_year_cur, cur_block_data))
    total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
    cur_weight <- unique(PFF_year_cur$Week) * .2
    cur_weight <- max(1, cur_weight)
    total_pff <- total_pff |>
      mutate(rec_grade_mean = case_when(
        is.na(rec_grade_mean_last) ~ rec_grade_mean_cur,
        is.na(rec_grade_mean_cur) ~ rec_grade_mean_last,
        TRUE ~ (cur_weight * rec_grade_mean_cur) +
          ((1 - cur_weight) * rec_grade_mean_last)
      ),
             pass_block_grade_mean = case_when(
               is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
               is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
               TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
                 ((1 - cur_weight) * pass_block_grade_mean_last)
             ),
             run_block_grade_mean = case_when(
               is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
               is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
               TRUE ~ (cur_weight * run_block_grade_mean_cur) +
                 ((1 - cur_weight) * run_block_grade_mean_last)
             ),
             games = case_when(
               is.na(games_last) ~ games_cur,
               is.na(games_cur) ~ games_last,
               (games_cur + games_last) <= 17 ~ (games_cur + games_last),
               TRUE ~ 17
             ))
  } else{
    total_pff <- PFF_year_last |>
      mutate(rec_grade_mean = rec_grade_mean_last,
             pass_block_grade_mean = pass_block_grade_mean_last,
             run_block_grade_mean = run_block_grade_mean_last,
             games = games_last)
  }
  colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
  total_pff$pff_id <- as.character(total_pff$pff_id)
  with_age <- total_pff
  with_age <- with_age |>
    mutate(Year = if_else(is.na(Year), year, Year),
           position = if_else(is.na(position), pos, position))
  with_age <- age_joiner(with_age)
  with_age <- with_age |>
    mutate(Age_next = Age + 1)
  rec_predictions <- predict(PFF_receiving_grade_mean_model, with_age)
  passblockdf <- with_age |>
    rename(block_grade_mean = pass_block_grade_mean)
  passblock_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
  runblockdf <- with_age |>
    rename(block_grade_mean = run_block_grade_mean)
  runblock_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
  finaldf <- tibble(pff_id = with_age$pff_id,
                    rec_grade_mean = rec_predictions,
                    rec_grade_sd = if_else(with_age$games_cur >= 3,
                                           with_age$rec_grade_sd_cur,
                                           with_age$rec_grade_sd),
                    pass_block_grade_mean = passblock_predictions,
                    pass_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                  with_age$pass_block_grade_sd_cur,
                                                  with_age$pass_block_grade_sd),
                    run_block_grade_mean = runblock_predictions,
                    run_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                 with_age$run_block_grade_sd_cur,
                                                 with_age$run_block_grade_sd))

  ### DEPTH CHART PART
  players <- snaps_to_depth_chart(team, "coverage") |>
    filter(position == pos)

  combined <- suppressMessages(left_join(players, finaldf))
  if(pos=="WR"){
    combined <- combined |>
      mutate(rec_grade_mean = if_else(is.na(rec_grade_mean), 62, rec_grade_mean),
             rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
             pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 59, pass_block_grade_mean),
             pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 12.5, pass_block_grade_sd),
             run_block_grade_mean = if_else(is.na(run_block_grade_mean), 59, run_block_grade_mean),
             run_block_grade_sd = if_else(is.na(run_block_grade_sd), 12.5, run_block_grade_sd))
  } else if(pos=="TE"){
    combined <- combined |>
      mutate(rec_grade_mean = if_else(is.na(rec_grade_mean), 60, rec_grade_mean),
             rec_grade_sd = if_else(is.na(rec_grade_sd), 17.5, rec_grade_sd),
             pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 62, pass_block_grade_mean),
             pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 15, pass_block_grade_sd),
             run_block_grade_mean = if_else(is.na(run_block_grade_mean), 62, run_block_grade_mean),
             run_block_grade_sd = if_else(is.na(run_block_grade_sd), 15, run_block_grade_sd))
  }
  combined
}

OL_PFF_grade <- function(team, year){
  last_varname <- paste0("PFF_Blocking_", year - 1)
  last_data <- get(last_varname, envir = parent.env(environment()))
  PFF_year_last <- last_data |>
    mutate(position = if_else(position %in% c("T", "G", "C"), "OL", position)) |>
    filter(position=="OL" & snap_counts_offense >= 15) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              pass_block_grade_mean_last = mean(grades_pass_block, na.rm = T),
              pass_block_grade_sd = sd(grades_pass_block, na.rm = T),
              run_block_grade_mean_last = mean(grades_run_block, na.rm = T),
              run_block_grade_sd = sd(grades_run_block, na.rm = T),
              games_last = n())
  cur_varname <- paste0("PFF_Blocking_", year)
  cur_data <- get(cur_varname, envir = parent.env(environment()))
  PFF_year_cur <- cur_data
  if(nrow(PFF_year_cur) > 1){
    PFF_year_cur <- PFF_year_cur |>
      mutate(position = if_else(position %in% c("T", "G", "C"), "OL", position)) |>
      filter(position=="OL" & snap_counts_offense >= 15) |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                pass_block_grade_mean_cur = mean(grades_pass_block, na.rm = T),
                pass_block_grade_sd_cur = sd(grades_pass_block, na.rm = T),
                run_block_grade_mean_cur = mean(grades_run_block, na.rm = T),
                run_block_grade_sd_cur = sd(grades_run_block, na.rm = T),
                games_cur = n())
    total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
    cur_weight <- unique(PFF_year_cur$Week) * .2
    cur_weight <- max(1, cur_weight)
    total_pff <- total_pff |>
      mutate(pass_block_grade_mean = case_when(
        is.na(pass_block_grade_mean_last) ~ pass_block_grade_mean_cur,
        is.na(pass_block_grade_mean_cur) ~ pass_block_grade_mean_last,
        TRUE ~ (cur_weight * pass_block_grade_mean_cur) +
          ((1 - cur_weight) * pass_block_grade_mean_last)
      ),
             run_block_grade_mean = case_when(
               is.na(run_block_grade_mean_last) ~ run_block_grade_mean_cur,
               is.na(run_block_grade_mean_cur) ~ run_block_grade_mean_last,
               TRUE ~ (cur_weight * run_block_grade_mean_cur) +
                 ((1 - cur_weight) * run_block_grade_mean_last)
             ),
             games = case_when(
               is.na(games_last) ~ games_cur,
               is.na(games_cur) ~ games_last,
               (games_cur + games_last) <= 17 ~ (games_cur + games_last),
               TRUE ~ 17
             ))
  } else{
    total_pff <- PFF_year_last |>
      mutate(pass_block_grade_mean = pass_block_grade_mean_last,
             run_block_grade_mean = run_block_grade_mean_last,
             games = games_last)
  }
  colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
  total_pff$pff_id <- as.character(total_pff$pff_id)
  with_age <- total_pff
  with_age <- with_age |>
    mutate(Year = if_else(is.na(Year), year, Year),
           position = if_else(is.na(position), "OL", position))
  with_age <- age_joiner(with_age)
  with_age <- with_age |>
    mutate(Age_next = Age + 1)
  passblockdf <- with_age |>
    rename(block_grade_mean = pass_block_grade_mean)
  pass_block_predictions <- predict(PFF_pass_blocking_grade_mean_model, passblockdf)
  runblockdf <- with_age |>
    rename(block_grade_mean = run_block_grade_mean)
  run_block_predictions <- predict(PFF_run_blocking_grade_mean_model, runblockdf)
  finaldf <- tibble(pff_id = with_age$pff_id,
                    pass_block_grade_mean = pass_block_predictions,
                    run_block_grade_mean = run_block_predictions,
                    pass_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                  with_age$pass_block_grade_sd_cur,
                                                  with_age$pass_block_grade_sd),
                    run_block_grade_sd = if_else(with_age$games_cur >= 3,
                                                 with_age$run_block_grade_sd_cur,
                                                 with_age$run_block_grade_sd))

  ### DEPTH CHART PART
  players <- snaps_to_depth_chart(team, "coverage") |>
    filter(position == "OL")


  combined <- suppressMessages(left_join(players, finaldf))
  combined <- combined |>
    mutate(run_block_grade_mean = if_else(is.na(run_block_grade_mean), 60, run_block_grade_mean),
           run_block_grade_sd = if_else(is.na(run_block_grade_sd), 17.5, run_block_grade_sd),
           pass_block_grade_mean = if_else(is.na(pass_block_grade_mean), 60, pass_block_grade_mean),
           pass_block_grade_sd = if_else(is.na(pass_block_grade_sd), 17.5, pass_block_grade_sd))
  combined
}

## for testing
team <- "DEN"
year <- 2025
pos <- "DB"
##

Defense_PFF_grade <- function(team, year, pos){
  last_def_varname <- paste0("PFF_Defense_", year - 1)
  last_def_data <- get(last_def_varname, envir = parent.env(environment()))
  PFF_year_last <- last_def_data |>
    mutate(position = case_when(
      position %in% c("DI", "ED", "LB") ~ "F7",
      position %in% c("CB", "S") ~ "DB",
      TRUE ~ position
    ),
    snap_counts_defense = snap_counts_coverage + snap_counts_pass_rush +
      snap_counts_run_defense) |>
    filter(position==pos & snap_counts_defense >= 15) |>
    group_by(player_id) |>
    summarise(player = player[1],
              Year = year,
              position = position[1],
              def_cover_grade_mean_last = mean(grades_coverage_defense, na.rm = T),
              def_cover_grade_sd = sd(grades_coverage_defense, na.rm = T),
              def_pass_rush_grade_mean_last = mean(grades_pass_rush_defense, na.rm = T),
              def_pass_rush_grade_sd = sd(grades_pass_rush_defense, na.rm = T),
              def_run_defense_grade_mean_last = mean(grades_run_defense, na.rm = T),
              def_run_defense_grade_sd = sd(grades_run_defense, na.rm = T),
              games_last = n())
  cur_def_varname <- paste0("PFF_Defense_", year)
  cur_def_data <- get(cur_def_varname, envir = parent.env(environment()))
  PFF_year_cur <- cur_def_data
  if(nrow(PFF_year_cur) > 1){
    PFF_year_cur <- PFF_year_cur |>
      mutate(position = case_when(
        position %in% c("DI", "ED", "LB") ~ "F7",
        position %in% c("CB", "S") ~ "DB",
        TRUE ~ position
      ),
      snap_counts_defense = snap_counts_coverage + snap_counts_pass_rush +
        snap_counts_run_defense) |>
      filter(position==pos & snap_counts_defense >= 15) |>
      group_by(player_id) |>
      summarise(player = player[1],
                position = position[1],
                Week = Week[1],
                def_cover_grade_mean_cur = mean(grades_coverage_defense, na.rm = T),
                def_pass_rush_grade_mean_cur = mean(grades_pass_rush_defense, na.rm = T),
                def_run_defense_grade_mean_cur = mean(grades_run_defense, na.rm = T),
                def_cover_grade_sd_cur = sd(grades_coverage_defense, na.rm = T),
                def_pass_rush_grade_sd_cur = sd(grades_pass_rush_defense, na.rm = T),
                def_run_defense_grade_sd_cur = sd(grades_run_defense, na.rm = T),
                games_cur = n())
    total_pff <- suppressMessages(left_join(PFF_year_last, PFF_year_cur))
    cur_weight <- unique(PFF_year_cur$Week) * .2
    cur_weight <- max(1, cur_weight)
    total_pff <- total_pff |>
      mutate(def_coverage_grade_mean = case_when(
        is.na(def_cover_grade_mean_last) ~ def_cover_grade_mean_cur,
        is.na(def_cover_grade_mean_cur) ~ def_cover_grade_mean_last,
        TRUE ~ (cur_weight * def_cover_grade_mean_cur) +
          ((1 - cur_weight) * def_cover_grade_mean_last)
      ),
             def_pass_rush_grade_mean = case_when(
               is.na(def_pass_rush_grade_mean_last) ~ def_pass_rush_grade_mean_cur,
               is.na(def_pass_rush_grade_mean_cur) ~ def_pass_rush_grade_mean_last,
               TRUE ~ (cur_weight * def_pass_rush_grade_mean_cur) +
                 ((1 - cur_weight) * def_pass_rush_grade_mean_last)
             ),
             def_run_grade_mean = case_when(
               is.na(def_run_defense_grade_mean_last) ~ def_run_defense_grade_mean_cur,
               is.na(def_run_defense_grade_mean_cur) ~ def_run_defense_grade_mean_last,
               TRUE ~ (cur_weight * def_run_defense_grade_mean_cur) +
                 ((1 - cur_weight) * def_run_defense_grade_mean_last)
             ),
             games = case_when(
               is.na(games_last) ~ games_cur,
               is.na(games_cur) ~ games_last,
               (games_cur + games_last) <= 17 ~ (games_cur + games_last),
               TRUE ~ 17
             ))
  } else{
    total_pff <- PFF_year_last |>
      mutate(def_coverage_grade_mean = def_cover_grade_mean_last,
             def_pass_rush_grade_mean = def_pass_rush_grade_mean_last,
             def_run_grade_mean = def_run_defense_grade_mean_last,
             games = games_last)
  }
  colnames(total_pff)[colnames(total_pff) == "player_id"] <- "pff_id"
  total_pff$pff_id <- as.character(total_pff$pff_id)
  with_age <- total_pff
  with_age <- with_age |>
    mutate(Year = if_else(is.na(Year), year, Year),
           position = if_else(is.na(position), "OL", position))
  with_age <- age_joiner(with_age)
  with_age <- with_age |>
    mutate(Age_next = Age + 1)
  def_coverage_predictions <- predict(PFF_def_coverage_grade_mean_model, with_age)
  def_pass_rush_predictions <- predict(PFF_def_pass_rush_grade_mean_model, with_age)
  def_run_defense_predictions <- predict(PFF_def_run_grade_mean_model, with_age)
  finaldf <- tibble(pff_id = with_age$pff_id,
                    def_coverage_grade_mean = def_coverage_predictions,
                    def_coverage_grade_sd = if_else(with_age$games_cur >= 3,
                                                    with_age$def_cover_grade_sd_cur,
                                                    with_age$def_cover_grade_sd),
                    def_pass_rush_grade_mean = def_pass_rush_predictions,
                    def_pass_rush_grade_sd = if_else(with_age$games_cur >= 3,
                                                     with_age$def_pass_rush_grade_sd_cur,
                                                     with_age$def_pass_rush_grade_sd),
                    def_run_defense_grade_mean = def_run_defense_predictions,
                    def_run_defense_grade_sd = if_else(with_age$games_cur >= 3,
                                                       with_age$def_run_defense_grade_sd_cur,
                                                       with_age$def_run_defense_grade_sd))

  ### DEPTH CHART PART
  cover_players <- snaps_to_depth_chart(team, "coverage") |>
    filter(position == pos) |>
    rename(predicted_coverage_snaps = final_predicted_snap_ct)
  pass_rush_players <- snaps_to_depth_chart(team, "pass_rush") |>
    filter(position == pos) |>
    rename(predicted_pass_rush_snaps = final_predicted_snap_ct)
  run_defense_players <- snaps_to_depth_chart(team, "run_defense") |>
    filter(position == pos) |>
    rename(predicted_run_defense_snaps = final_predicted_snap_ct)

  players <- suppressMessages(left_join(cover_players, pass_rush_players))
  players <- suppressMessages(left_join(players, run_defense_players))

  combined <- suppressMessages(left_join(players, finaldf))
  if(pos=="DB"){
    combined <- combined |>
      mutate(def_coverage_grade_mean = if_else(is.na(def_coverage_grade_mean), 62, def_coverage_grade_mean),
             def_coverage_grade_sd = if_else(is.na(def_coverage_grade_sd), 17.5, def_coverage_grade_sd),
             def_pass_rush_grade_mean = if_else(is.na(def_pass_rush_grade_mean), 59, def_pass_rush_grade_mean),
             def_pass_rush_grade_sd = if_else(is.na(def_pass_rush_grade_sd), 12.5, def_pass_rush_grade_sd),
             def_run_defense_grade_mean = if_else(is.na(def_run_defense_grade_mean), 60, def_run_defense_grade_mean),
             def_run_defense_grade_sd = if_else(is.na(def_run_defense_grade_sd), 17.5, def_run_defense_grade_sd))
  } else if(pos=="F7"){
    combined <- combined |>
      mutate(def_coverage_grade_mean = if_else(is.na(def_coverage_grade_mean), 59, def_coverage_grade_mean),
             def_coverage_grade_sd = if_else(is.na(def_coverage_grade_sd), 17.5, def_coverage_grade_sd),
             def_pass_rush_grade_mean = if_else(is.na(def_pass_rush_grade_mean), 62, def_pass_rush_grade_mean),
             def_pass_rush_grade_sd = if_else(is.na(def_pass_rush_grade_sd), 17.5, def_pass_rush_grade_sd),
             def_run_defense_grade_mean = if_else(is.na(def_run_defense_grade_mean), 62, def_run_defense_grade_mean),
             def_run_defense_grade_sd = if_else(is.na(def_run_defense_grade_sd), 15, def_run_defense_grade_sd))
  }
  combined
}

full_PFF <- function(team, year){
  quarterback <- QB_PFF_grade(team, year)
  runningbacks <- RB_PFF_grade(team, year)
  widereceivers <- WRorTE_PFF_grade(team, year, "WR")
  tightends <- WRorTE_PFF_grade(team, year, "TE")
  oline <- OL_PFF_grade(team, year)
  defensivebacks <- Defense_PFF_grade(team, year, "DB")
  frontseven <- Defense_PFF_grade(team, year, "F7")
  allpositions <- list(quarterback, runningbacks, widereceivers, tightends,
                       oline, defensivebacks, frontseven)
  finaldf <- suppressMessages(reduce(allpositions, full_join))
  #### TEST FOR MORE VARIABLE OUTCOMES (9/4/25)
  # finaldf <- finaldf |>
  #   mutate(pass_grade_mean = if_else(pass_grade_mean>=64, 64 + (pass_grade_mean - 64)^1.75,
  #                          64 - (64 - pass_grade_mean)^1.75),
  #          rush_grade_mean = if_else(rush_grade_mean>=64, 64 + (rush_grade_mean - 64)^1.75,
  #                                    64 - (64 - rush_grade_mean)^1.75),
  #          rec_grade_mean = if_else(rec_grade_mean>=64, 64 + (rec_grade_mean - 64)^1.75,
  #                                    64 - (64 - rec_grade_mean)^1.75),
  #          pass_block_grade_mean = if_else(pass_block_grade_mean>=64, 64 +
  #                                            (pass_block_grade_mean - 64)^1.75,
  #                                    64 - (64 - pass_block_grade_mean)^1.75),
  #          run_block_grade_mean = if_else(run_block_grade_mean>=64, 64 +
  #                                           (run_block_grade_mean - 64)^1.75,
  #                                    64 - (64 - run_block_grade_mean)^1.75),
  #          def_coverage_grade_mean = if_else(def_coverage_grade_mean>=64, 64 +
  #                                              (def_coverage_grade_mean - 64)^1.75,
  #                                    64 - (64 - def_coverage_grade_mean)^1.75),
  #          def_pass_rush_grade_mean = if_else(def_pass_rush_grade_mean>=64, 64 +
  #                                               (def_pass_rush_grade_mean - 64)^1.75,
  #                                    64 - (64 - def_pass_rush_grade_mean)^1.75),
  #          def_run_defense_grade_mean = if_else(def_run_defense_grade_mean>=64,
  #                                               64 + (def_run_defense_grade_mean - 64)^1.75,
  #                                    64 - (64 - def_run_defense_grade_mean)^1.75),)
  finaldf
}


# Simulator Functions -----------------------------------------------------

choose_personnel <- function(posstm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo
  )
  probs <- predict(off_personnel_model, modeldat, type = "probs")
  personnels <- sort(unique(offdf$simple_personnel))
  sample(personnels, size = 1, prob = probs)
}

choose_def_personnel <- function(deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter, personnel){
  modeldat <- data.frame(
    defensiveTeam = deftm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    YdstoEZBef = YdsBef,
    down = down,
    yardsToGo = togo,
    simple_personnel = personnel
  )
  probs <- predict(defpersonnelmodel, modeldat, type = "probs")
  def_personnels <- sort(unique(offdf$simple_def_personnel))
  sample(def_personnels, size = 1, prob = probs)
}

choose_formation <- function(posstm, down, togo, YdsBef, posstmdiff,
                             quarter_secs, quarter, personnel, def_personnel){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    simple_def_personnel = def_personnel
  )
  probs <- predict(off_formation_model, modeldat, type = "probs")
  formations <- sort(unique(offdf$off_form))
  sample(formations, size = 1, prob = probs)
}

choose_def_coverage <- function(deftm, down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, def_personnel, formation){
  modeldat <- data.frame(
    defensiveTeam = deftm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    simple_def_personnel = def_personnel,
    off_form = formation
  )
  probs <- predict(defcoveragemodel, modeldat, type = "probs")
  coverages <- sort(unique(offdf$def_simple_coverage))
  sample(coverages, size = 1, prob = probs)
}

runorpass <- function(posstm, down, togo, YdsBef, posstmdiff, quarter_secs,
                      quarter, personnel, formation, def_personnel, coverage){
  modeldat <- data.frame(
    possessionTeam = posstm,
    QuarterSeconds = quarter_secs,
    quarter = quarter,
    PossTmDiff = posstmdiff,
    down = down,
    yardsToGo = togo,
    YdstoEZBef = YdsBef,
    simple_personnel = personnel,
    off_form = formation,
    simple_def_personnel = def_personnel,
    def_simple_coverage = coverage
  )
  passprob <- predict(runpass_model, modeldat, type = "response")
  runprob <- 1 - passprob
  probs <- c(passprob, runprob)
  types <- c("Pass", "Run")
  sample(types, size = 1, prob = probs)
}

# Route Selection Function ------------------------------------------------

routeselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                           off_dat, def_dat){
  whole_offense <- off_dat
  whole_defense <- def_dat
  personnel <- choose_personnel(posstm, down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter)
  def_personnel <- choose_def_personnel(deftm, down, togo, YdsBef, posstmdiff,
                                        quarter_secs, quarter, personnel)
  formation <- choose_formation(posstm, down, togo, YdsBef, posstmdiff,
                                quarter_secs, quarter, personnel, def_personnel)
  coverage <- choose_def_coverage(deftm, down, togo, YdsBef, posstmdiff, quarter_secs,
                                  quarter, personnel, def_personnel, formation)
  runpassselection <- runorpass(posstm, down, togo, YdsBef, posstmdiff, quarter_secs,
                                quarter, personnel, formation, def_personnel, coverage)
  if(personnel %in% c("03", "02", "01", "00")){
    runpassselection <- "Pass"
  }
  if(personnel=="13"){
    personnel <- "12"
  }
  if(personnel=="03"){
    personnel <- "02"
  }
  if(personnel=="23"){
    personnel <- "22"
  }
  if(runpassselection=="Pass"){
    ### SETS UP DATAFRAME FOR MODEL USAGE LATER
    modeldat <- data.frame(
      possessionTeam = posstm,
      QuarterSeconds = quarter_secs,
      quarter = quarter,
      PossTmDiff = posstmdiff,
      down = down,
      yardsToGo = togo,
      YdstoEZBef = YdsBef,
      simple_personnel = personnel,
      off_form = formation,
      runorpass = "Pass",
      defensiveTeam = deftm,
      simple_def_personnel = def_personnel,
      def_simple_coverage = coverage
    )

    ### FIGURES OUT NUMBER OF COVER PLAYERS AND PASS RUSHERS
    coverx <- posterior_predict(coverplayersmodel, newdata = modeldat)
    coverplayers <- round(apply(coverx, 2, sample, size = 1),0)
    passrushers <- 11 - coverplayers

    ### FIGURES OUT THE NUMBER OF ROUTES RAN
    routesrunprobs <- predict(route_count_model, modeldat, type = "probs")
    num_RB <- as.numeric(substr(personnel, 1, 1))
    num_TE <- as.numeric(substr(personnel, 2, 2))
    routesrunpossibilities <- c((num_RB+num_TE):6)
    newroutesrunprobs <- routesrunprobs[(num_RB+num_TE):6]
    routesrun <- sample(routesrunpossibilities, size=1, prob=newroutesrunprobs)

    ### FIGURES OUT THE SPECIFIC ROUTES RAN
    anglect <- cornerct <- crossct <- flatct <- goct <- hitchct <- inct <-
      outct <- postct <- screenct <- slantct <- wheelct <- 0
    chosenroutelist <- c()
    for(n in 1:routesrun){
      modeldat <- modeldat |>
        mutate(ANGLE_count = anglect,
               CORNER_count = cornerct,
               CROSS_count = crossct,
               FLAT_count = flatct,
               GO_count = goct,
               HITCH_count = hitchct,
               IN_count = inct,
               OUT_count = outct,
               POST_count = postct,
               SCREEN_count = screenct,
               SLANT_count = slantct,
               WHEEL_count = wheelct)

      angleprobs <- predict(angle_model, modeldat, type = "probs")
      cornerprobs <- predict(corner_model, modeldat, type = "probs")
      crossprobs <- predict(cross_model, modeldat, type = "probs")
      flatprobs <- predict(flat_model, modeldat, type = "probs")
      goprobs <- predict(go_model, modeldat, type = "probs")
      hitchprobs <- predict(hitch_model, modeldat, type = "probs")
      inprobs <- predict(in_model, modeldat, type = "probs")
      outprobs <- predict(out_model, modeldat, type = "probs")
      postprobs <- predict(post_model, modeldat, type = "probs")
      screenprobs <- predict(screen_model, modeldat, type = "probs")
      slantprobs <- predict(slant_model, modeldat, type = "probs")
      wheelprobs <- predict(wheel_model, modeldat, type = "probs")
      allprobs <- c(angleprobs, cornerprobs, crossprobs, flatprobs, goprobs, hitchprobs,
                    inprobs, outprobs, postprobs, screenprobs, slantprobs, wheelprobs)
      probsdf <- as.data.frame(t(allprobs))
      colnames(probsdf) <- c("a0", "a1", "a2", "co0", "co1", "co2", "co3", "cr0",
                             "cr1", "cr2", "cr3", "cr4", "f0", "f1", "f2", "f3",
                             "f4", "g0", "g1", "g2", "g3", "g4", "h0", "h1", "h2",
                             "h3", "h4", "h5", "i0", "i1", "i2", "i3", "i4",
                             "o0", "o1", "o2", "o3", "o4", "p0", "p1", "p2", "p3",
                             "sc0", "sc1", "sc2", "sc3", "sc4", "sl0", "sl1",
                             "sl2", "sl3", "sl4", "w0", "w1", "w2")
      aprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("a", anglect+1))])
      coprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("co", cornerct+1))])
      crprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("cr", crossct+1))])
      fprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("f", flatct+1))])
      gprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("g", goct+1))])
      hprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("h", hitchct+1))])
      iprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("i", inct+1))])
      oprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("o", outct+1))])
      pprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("p", postct+1))])
      scprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sc", screenct+1))])
      slprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("sl", slantct+1))])
      wprob <- as.numeric(probsdf[which(colnames(probsdf)==paste0("w", wheelct+1))])
      probs <- c(aprob, coprob, crprob, fprob, gprob, hprob, iprob, oprob, pprob,
                 scprob, slprob, wprob)
      route_options <- c("angle", "corner", "cross", "flat", "go", "hitch", "in",
                         "out", "post", "screen", "slant", "wheel")
      chosen_route <- sample(route_options, 1, prob = probs)
      varname <- paste0(chosen_route, "ct")
      value <- get(varname)
      chosenroutelist <- append(chosenroutelist, chosen_route)
      assign(varname, get(varname) + 1)
    }

    #### GETS SPECIFIC OFFENSIVE AND DEFENSIVE PLAYERS ON THE FIELD
    mydf <- data.frame(
      RB = as.numeric(substr(personnel, 1, 1)),
      WR = length(chosenroutelist) - as.numeric(substr(personnel, 1, 1)) -
        as.numeric(substr(personnel, 2,2)),
      TE = as.numeric(substr(personnel, 2, 2))
    )
    off_depth_chart <- snaps_to_depth_chart(posstm, "coverage")
    onfieldQB <- off_depth_chart$pff_id[off_depth_chart$final_position=="QB1"]
    runningbacks <- off_depth_chart |>
      filter(position=="RB" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    rbprobs <- runningbacks$final_predicted_snap_ct/sum(runningbacks$final_predicted_snap_ct)
    onfieldRBS <- sample(runningbacks$pff_id, mydf$RB, prob = rbprobs)
    widereceivers <- off_depth_chart |>
      filter(position=="WR" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    wrprobs <-  widereceivers$final_predicted_snap_ct/sum(widereceivers$final_predicted_snap_ct)
    onfieldWRS <- sample(widereceivers$pff_id, mydf$WR, prob = wrprobs)
    tightends <- off_depth_chart |>
      filter(position=="TE" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    teprobs <- tightends$final_predicted_snap_ct/sum(tightends$final_predicted_snap_ct)
    onfieldTEs <- sample(tightends$pff_id, mydf$TE, prob = teprobs)
    backupOL <- off_depth_chart |>
      filter(final_position %in% c("LT2", "LG2", "C2", "RG2", "RT2",
                                   "LT3", "LG3", "C3", "RG3", "RT3"))
    backupOLprobs <- backupOL$final_predicted_snap_ct/sum(backupOL$final_predicted_snap_ct)
    onfieldOL <- off_depth_chart$pff_id[off_depth_chart$final_position %in%
                                          c("LT1", "LG1", "C1", "RG1", "RT1")]
    onfieldOLcount <- 10 - sum(mydf[1,])
    if(onfieldOLcount >5){
      onfieldOL <- c(onfieldOL, sample(backupOL$pff_id, onfieldOLcount-5, prob = backupOLprobs))
    }

    ### FIGURING OUT DEFENSE ON FIELD PLAYERS
    if(def_personnel=="Nickel"){
      frontsevenamount <- 6
    } else if(def_personnel=="Heavy"){
      frontsevenamount <- 8
    } else if(def_personnel=="Goalline"){
      frontsevenamount <- 9
    } else if (def_personnel=="Dime"){
      frontsevenamount <- 5
    } else if (def_personnel=="Quarter"){
      frontsevenamount <- 4
    } else{
      frontsevenamount <- sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    }
    secondarycount <- 11 - frontsevenamount
    if(coverplayers > secondarycount){
      frontsevencoverplayers <- coverplayers - secondarycount
      secondaryrushers <- 0
    } else{
      frontsevencoverplayers <- 0
      secondaryrushers <- secondarycount - coverplayers
    }

    pass_rush_depth_chart <- snaps_to_depth_chart(deftm, "pass_rush")
    coverage_depth_chart <- snaps_to_depth_chart(deftm, "coverage")

    ### FRONT SEVEN
    f7passrushers <- pass_rush_depth_chart |>
      filter(position=="F7" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    if(frontsevencoverplayers!=0){
      f7coverplayers <- coverage_depth_chart |>
        filter(position == "F7" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    }

    ### SECONDARY
    secondary_coverplayers <- coverage_depth_chart |>
      filter(position=="DB" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    if(secondaryrushers!=0){
      secondary_rushers <- pass_rush_depth_chart |>
        filter(position=="DB" & !is.na(final_predicted_snap_ct) & !grepl("( O| IR| D)$", player))
    }

    f7passrushprobs <- f7passrushers$final_predicted_snap_ct/sum(f7passrushers$final_predicted_snap_ct)
    secondarycoverprobs <- secondary_coverplayers$final_predicted_snap_ct/sum(secondary_coverplayers$final_predicted_snap_ct)

    # BY SCENARIO
    if(frontsevenamount > passrushers){
      on_field_F7_pass_rushers <- sample(f7passrushers$pff_id, passrushers, prob = f7passrushprobs)
      f7coverplayers <- f7coverplayers |>
        filter(!pff_id %in% on_field_F7_pass_rushers)
      f7coverprobs <- f7coverplayers$final_predicted_snap_ct/sum(f7coverplayers$final_predicted_snap_ct)
      on_field_F7_cover_players <- sample(f7coverplayers$pff_id, frontsevenamount - passrushers,
                                          prob = f7coverprobs)
      on_field_secondary_cover_players <- sample(secondary_coverplayers$pff_id, secondarycount,
                                                 prob = secondarycoverprobs)
      on_field_secondary_rushers <- c()
    } else if (frontsevenamount < passrushers){
      on_field_F7_pass_rushers <- sample(f7passrushers$pff_id, frontsevenamount, prob = f7passrushprobs)
      on_field_secondary_cover_players <- sample(secondary_coverplayers$pff_id, coverplayers,
                                                 prob = secondarycoverprobs)
      secondary_rushers <- secondary_rushers |>
        filter(!pff_id %in% on_field_secondary_cover_players)
      secondarypassrushprobs <- secondary_rushers$final_predicted_snap_ct/sum(secondary_rushers$final_predicted_snap_ct)
      on_field_secondary_rushers <- sample(secondary_rushers$pff_id, secondarycount - coverplayers,
                                           prob = secondarypassrushprobs)
      on_field_F7_cover_players <- c()
    } else if (frontsevenamount == passrushers){
      on_field_F7_pass_rushers <- sample(f7passrushers$pff_id, passrushers, prob = f7passrushprobs)
      on_field_secondary_cover_players <- sample(secondary_coverplayers$pff_id, coverplayers,
                                                 prob = secondarycoverprobs)
      on_field_F7_cover_players <- c()
      on_field_secondary_rushers <- c()
    }

    allcoverplayers <- c(on_field_F7_cover_players, on_field_secondary_cover_players)
    covertgtplyrcnt <- sample(c(0,1,2), 1, prob = c(.09, .9, .01))
    potentialtgtcovers <- coverage_depth_chart |> filter(pff_id %in% allcoverplayers)
    covertgtprobs <- potentialtgtcovers$final_predicted_snap_ct/sum(potentialtgtcovers$final_predicted_snap_ct)
    covertargetplayers <- sample(potentialtgtcovers$pff_id, covertgtplyrcnt, prob = covertgtprobs)
    noncovertgtplayers <- potentialtgtcovers |>
      filter(!pff_id %in% covertargetplayers)

    modeldat$passrushers <- passrushers

    ttt_pred_dist <- posterior_predict(timeToThrowmodel, newdata = modeldat)
    timetoThrow <- round(apply(ttt_pred_dist, 2, sample, size = 1),3)
    modeldat$timeToThrow <- timetoThrow

    chosenroutelist <- toupper(chosenroutelist)

    personneldf <- data.frame(
      RB = as.numeric(substr(personnel, 1, 1)),
      WR = length(chosenroutelist) - as.numeric(substr(personnel, 1, 1)) -
        as.numeric(substr(personnel, 2, 2)),
      TE = as.numeric(substr(personnel, 2, 2))
    )

    poschosenroutelist <- sort(chosenroutelist)

    if(personneldf$RB!=0){
      rbroutedf <- posroutedf |> filter(position=="RB" & route %in% poschosenroutelist)
      RBproblist <- c()
      for(route in poschosenroutelist){
        RBprob <- rbroutedf$prob[rbroutedf$route==route]
        RBproblist <- append(RBproblist, RBprob)
      }
      RBroutes <- sample(poschosenroutelist, personneldf$RB, prob = RBproblist/sum(RBproblist))
      rbroutelist <- c()
      for(r in 1:personneldf$RB){
        RBindex <- match(RBroutes[r], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-RBindex]
        rbroutelist <- append(rbroutelist, paste0("RB: ", RBroutes[r]))
      }
    } else{
      rbroutelist <- c()
    }

    if(personneldf$TE!=0){
      teroutedf <- posroutedf |> filter(position=="TE" & route %in% poschosenroutelist)
      TEproblist <- c()
      for(route in poschosenroutelist){
        TEprob <- teroutedf$prob[teroutedf$route==route]
        TEproblist <- append(TEproblist, TEprob)
      }
      TEroutes <- sample(poschosenroutelist, personneldf$TE, prob = TEproblist/sum(TEproblist))
      teroutelist <- c()
      for(t in 1:personneldf$TE){
        TEindex <- match(TEroutes[t], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-TEindex]
        teroutelist <- append(teroutelist, paste0("TE: ", TEroutes[t]))
      }
    } else{
      teroutelist <- c()
    }

    if(personneldf$WR!=0){
      wrroutedf <- posroutedf |> filter(position=="WR" & route %in% poschosenroutelist)
      WRproblist <- c()
      for(route in poschosenroutelist){
        WRprob <- wrroutedf$prob[wrroutedf$route==route]
        WRproblist <- append(WRproblist, WRprob)
      }
      WRroutes <- sample(poschosenroutelist, personneldf$WR, prob = WRproblist/sum(WRproblist))
      wrroutelist <- c()
      for(w in 1:personneldf$WR){
        WRindex <- match(WRroutes[w], poschosenroutelist)
        poschosenroutelist <- poschosenroutelist[-WRindex]
        wrroutelist <- append(wrroutelist, paste0("WR: ", WRroutes[w]))
      }
    } else{
      wrroutelist <- c()
    }

    finalvec <- c(rbroutelist, wrroutelist, teroutelist)

    tgtrouteprobs <- data.frame(t(predict(targetted_route_model, modeldat, type = "probs")))
    tgtprobs <- c()
    for(route in chosenroutelist){
      colkey <- which(colnames(tgtrouteprobs)==route)
      prob <- as.numeric(tgtrouteprobs[colkey])
      tgtprobs <- append(tgtprobs, prob)
    }
    tgtprobs1 <- tgtprobs/sum(tgtprobs)
    targetted_route <- sample(chosenroutelist, 1, prob = tgtprobs1)

    tgtposoptions <- finalvec[grepl(targetted_route, finalvec)]
    tgtposselection <- sample(tgtposoptions, 1,
                              prob = rep(1/length(tgtposoptions), length(tgtposoptions)))
    tgtpos <- str_split_fixed(tgtposselection, ":", 2)[,1]
    on_field_recs <- whole_offense |>
      filter(pff_id %in% c(onfieldRBS, onfieldWRS, onfieldTEs))
    if(tgtpos=="RB"){
      newRBs <- on_field_recs |>
        filter(position=="RB")
      tgtprobs <- newRBs$final_predicted_snap_ct / sum(newRBs$final_predicted_snap_ct)
      tgtplayer <- sample(newRBs$pff_id, 1, prob = tgtprobs)
    } else if(tgtpos=="WR"){
      newWRs <- on_field_recs |>
        filter(position=="WR")
      tgtprobs <- newWRs$final_predicted_snap_ct / sum(newWRs$final_predicted_snap_ct)
      tgtplayer <- sample(newWRs$pff_id, 1, prob = tgtprobs)
    } else if(tgtpos=="TE"){
      newTEs <- on_field_recs |>
        filter(position=="TE")
      tgtprobs <- newTEs$final_predicted_snap_ct / sum(newTEs$final_predicted_snap_ct)
      tgtplayer <- sample(newTEs$pff_id, 1, prob = tgtprobs)
    } else{
      tgtplayer <- NA
    }

    otherroutes <- on_field_recs$pff_id[on_field_recs$pff_id != tgtplayer]

    list(pers = personnel, form = formation, runorpass = "Pass",
         def_pers = def_personnel, coverage = coverage,
         routes = finalvec, tgt_route = targetted_route, otherroutes = otherroutes,
         quarterback = onfieldQB,
         oline = onfieldOL,
         passrushers = c(on_field_F7_pass_rushers, on_field_secondary_rushers),
         coveringtgtplayer = covertargetplayers,
         othercoverplayers = noncovertgtplayers$pff_id,
         tgt_pos = tgtpos, tgt_player = tgtplayer)

  } else if(runpassselection == "Run"){
    RBs <- as.numeric(substr(personnel, 1, 1))
    TEs <- as.numeric(substr(personnel, 2, 2))
    WRs <- 5 - RBs - TEs

    onfieldQB <- whole_offense$pff_id[whole_offense$position=="QB"]
    runningbacks <- whole_offense |>
      filter(position=="RB")
    rbprobs <- runningbacks$final_predicted_snap_ct / sum(runningbacks$final_predicted_snap_ct)
    onfieldRBS <- sample(runningbacks$pff_id, RBs, prob = rbprobs)
    widereceivers <- whole_offense |>
      filter(position=="WR")
    wrprobs <- widereceivers$final_predicted_snap_ct / sum(widereceivers$final_predicted_snap_ct)
    onfieldWRS <- sample(widereceivers$pff_id, WRs, prob = wrprobs)
    tightends <- whole_offense |>
      filter(position=="TE")
    teprobs <- tightends$final_predicted_snap_ct / sum(tightends$final_predicted_snap_ct)
    onfieldTES <- sample(tightends$pff_id, TEs, prob = teprobs)

    olinecount <- 10 - (RBs + WRs + TEs)
    onfieldOL <- whole_offense$pff_id[whole_offense$depth_next=="OL1"]
    if(olinecount > 5){
      backupOL <- whole_offense |>
        filter(position=="OL" & depth_next!="OL1")
      backupOLprobs <- backupOL$final_predicted_snap_ct / sum(backupOL$final_predicted_snap_ct)
      backupOL <- sample(backupOL$pff_id, (olinecount - 5), prob = backupOLprobs)
      onfieldOL <- c(onfieldOL, backupOL)
    }
    potential_rush_posdf <- whole_offense |>
      filter(pff_id %in% c(onfieldQB, onfieldRBS))
    if(formation=="WILDCAT"){
      potential_rush_posdf <- potential_rush_posdf |>
        filter(position=="RB")
    }
    rushposprobs <- potential_rush_posdf$final_predicted_snap_ct /
      sum(potential_rush_posdf$final_predicted_snap_ct)
    rushplayer <- sample(potential_rush_posdf$pff_id, 1, prob = rushposprobs)
    rushpos <- potential_rush_posdf$position[potential_rush_posdf$pff_id==rushplayer]

    rest_of_offense <- c(onfieldQB, onfieldRBS, onfieldWRS, onfieldTES)
    rest_of_offense <- rest_of_offense[-which(rest_of_offense==rushplayer)]
    if(rushpos!="QB"){
      rest_of_offense <- rest_of_offense[-which(rest_of_offense==onfieldQB)]
    }

    # deflengthdf <- offdf |> filter(defensiveTeam==deftm)
    # deflength <- nrow(deflengthdf)

    frontsevenamount <- suppressWarnings(case_when(
      def_personnel=="Nickel" ~ 6,
      def_personnel=="Heavy" ~ 8,
      def_personnel=="Goalline" ~ 9,
      def_personnel=="Dime" ~ 5,
      def_personnel=="Quarter" ~ 4,
      TRUE ~ sum(as.numeric(str_split_fixed(def_personnel, "-", 2)))
    ))

    f7df <- whole_defense |>
      filter(position=="F7")
    f7probs <- f7df$predicted_run_defense_snaps / sum(f7df$predicted_run_defense_snaps)
    f7players <- sample(f7df$pff_id, frontsevenamount, prob = f7probs)

    secondaryamount <- 11 - frontsevenamount
    secondarydf <- whole_defense |>
      filter(position=="DB")
    secondaryprobs <- secondarydf$predicted_run_defense_snaps / sum(secondarydf$predicted_run_defense_snaps)
    secondaryplayers <- sample(secondarydf$pff_id, secondaryamount, prob = secondaryprobs)

    list(pers = personnel, form = formation, runorpass = "Run",
         def_pers = def_personnel, coverage = coverage,
         rushpos = rushpos, rushplayer = rushplayer,
         quarterback = onfieldQB,
         rest_of_offense = rest_of_offense,
         oline = onfieldOL,
         defense = c(f7players, secondaryplayers))
  }
}

saferouteselection <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                               off_dat, def_dat){
  saferoutefun <- safely(routeselection)
  out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                       off_dat, def_dat))
  while(!is.null(out$error) | any(is.na(out$result))){
    out <- suppressWarnings(saferoutefun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                         off_dat, def_dat))
  }
  out$result
}


# Yards Gained Function ---------------------------------------------------

yardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                        off_dat, def_dat){
  output <- suppressWarnings(saferouteselection(posstm, deftm, down, togo,
                                                YdsBef, posstmdiff, quarter_secs, quarter,
                                                off_dat, def_dat))
  whole_offense <- off_dat
  whole_defense <- def_dat
  if(output$runorpass=="Pass"){
    if(output$coverage %in% c("Cover_1", "RedZone", "GoalLine", "Cover_0",
                              "Cover2Man", "Bracket")){
      manzone <- "Man"
    } else{
      manzone <- "Zone"
    }
    tgtroute <- output$tgt_route
    qbdf <- whole_offense |>
      filter(pff_id==output$quarterback)
    passing_grade <- rnorm(1, mean = qbdf$pass_grade_mean, sd = qbdf$pass_grade_sd)
    recdf <- whole_offense |>
      filter(pff_id==output$tgt_player)
    recgrade <- rnorm(1, mean = recdf$rec_grade_mean, sd = recdf$rec_grade_sd)
    cover_tgtplayer_df <- whole_defense |>
      filter(pff_id %in% output$coveringtgtplayer)
    ### NOTE: FOR NOW, I DON'T HAVE SEPARATE MAN AND ZONE COVER GRADES. THAT WOULD BE
    ### EASY TO IMPLEMENT THOUGH (9/3/25)
    if(nrow(cover_tgtplayer_df) > 0){
      cover_tgt_grade <- mean(mapply(rnorm, n=1, mean = cover_tgtplayer_df$def_coverage_grade_mean,
                                     sd = cover_tgtplayer_df$def_coverage_grade_sd))
    } else{
      cover_tgt_grade <- 30
    }
    ###
    olinedf <- whole_offense |>
      filter(pff_id %in% output$oline)
    olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$pass_block_grade_mean,
                                   sd = olinedf$pass_block_grade_sd))

    passrushdf <- whole_defense |>
      filter(pff_id %in% output$passrushers)
    passrushgrade <- mean(mapply(rnorm, n=1, mean = passrushdf$def_pass_rush_grade_mean,
                                 sd = passrushdf$def_pass_rush_grade_sd))
    dummydf <- data.frame(
      down = down,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      def_simple_coverage = output$coverage,
      off_form = output$form,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      possessionTeam = posstm,
      defensiveTeam = deftm,
      runorpass = output$runorpass,
      targettedroute = tgtroute,
      targettedposition = output$tgt_pos,
      oline_brocking_grade_mean = olineblockgrade,
      passrushers = length(output$passrushers),
      pffpassrush = olineblockgrade - passrushgrade,
      pass_grade_mean = passing_grade,
      receiving_grade_mean = recgrade,
      oline_brocking_grade_mean = olineblockgrade,
      cover_target_route_grade_mean = cover_tgt_grade,
      passrush_players_grade_mean = passrushgrade
    )
    pressureplayers <- sample(c(0:4), 1,
                              prob = predict(pressureplayersmodel, newdata = dummydf, type = "probs"))
    dummydf$pressureplayers <- pressureplayers
    sackprob <- predict(sack_model, dummydf, type = "response")
    sack <- sample(c("Yes", "No"), 1, prob = c(sackprob, 1-sackprob))
    if(sack=="Yes"){
      random_yards <- round(apply(posterior_predict(sack_yards_model, newdata = dummydf), 2, sample, size = 1),0)
      list(runpass = "Pass", result = "Sack", yards = random_yards)
    } else{
      ttt <- round(apply(posterior_predict(timeToThrowmodel, newdata = dummydf), 2, sample, size = 1),3)
      pressure_component <- if_else(pressureplayers>=1, "pressure", "no_pressure")
      ttt_component <- if_else(ttt<2.5, "less", "more")
      if(tgtroute %in% c("ANGLE", "FLAT", "IN", "OUT", "SLANT")){
        distance_component <- "short"
      } else if(tgtroute %in% c("CORNER", "CROSS", "HITCH")){
        distance_component <- "medium"
      } else if(tgtroute %in% c("GO", "POST", "WHEEL")){
        distance_component <- "deep"
      } else{
        distance_component <- "behind_los"
      }
      passresult <- sample(c("Complete", "Fumble", "Incomplete", "Interception"), 1,
                           prob = predict(passresultmodel, dummydf, type = "probs"))
      if(passresult=="Complete"){
        pass_yards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Complete", yards = pass_yards)
      } else if(passresult=="Interception"){
        pass_yards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        int_yards <- round(apply(posterior_predict(int_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        list(runpass = "Pass", result = "Interception", yards = pass_yards - int_yards)
      } else if(passresult=="Incomplete"){
        list(runpass = "Pass", result = "Incomplete", yards = 0)
      } else{
        fumblepos <- sample(c("QB", output$tgt_pos), 1, prob = c(.6, .4))
        if(fumblepos=="QB"){
          fumbleprob <- fumblostdf$lostprob[fumblostdf$fumbleposition=="QB"]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Sack_Fumble_Retained"
          } else{
            fumbresult <- "Sack_Fumble_Lost"
          }
          fumbleyards <- -5
        } else{
          fumbleprob <- fumblostdf$lostprob[fumblostdf$fumbleposition==output$tgt_pos]
          fumblost <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
          if(fumblost=="Yes"){
            fumbresult <- "Complete_Fumble_Retained"
          } else{
            fumbresult <- "Complete_Fumble_Lost"
          }
          fumbleyards <- round(apply(posterior_predict(passing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
        }
        list(runpass = "Pass", result = fumbresult, yards = fumbleyards)
      }
    }
  } else if(output$runorpass=="Run"){
    rushingpos <- output$rushpos
    olinedf <- whole_offense |>
      filter(pff_id %in% output$oline)
    olineblockgrade <- mean(mapply(rnorm, n=1, mean = olinedf$run_block_grade_mean,
                                   sd = olinedf$run_block_grade_sd))
    dummydf <- data.frame(
      down = down,
      simple_personnel = output$pers,
      simple_def_personnel = output$def_pers,
      def_simple_coverage = output$coverage,
      off_form = output$form,
      YdstoEZBef = YdsBef,
      yardsToGo = togo,
      possessionTeam = posstm,
      defensiveTeam = deftm,
      runorpass = output$runorpass,
      oline_brocking_grade_mean = olineblockgrade,
      rushingposition = rushingpos
    )

    runningplayerdf <- whole_offense |>
      filter(pff_id==output$rushplayer)
    runninggrade <- rnorm(1, mean = runningplayerdf$rush_grade_mean,
                          sd = runningplayerdf$rush_grade_sd)
    othersdf <- whole_offense |>
      filter(pff_id %in% output$rest_of_offense)
    othersgrade <- mean(mapply(rnorm, 1, mean = othersdf$run_block_grade_mean,
                               sd = othersdf$run_block_grade_sd))
    ### NOTE, I NEED TO REDO FEATURE ENGINEERING FOR HOW THE GRADES CORRELATE TO
    ### RUSH YARDS. I HOPE TO DO THAT SOON (9/3/25)
    totaloffensegrade <- (4*olineblockgrade + 3*runninggrade + 2*othersgrade)/9
    ###

    defensedf <- whole_defense |>
      filter(pff_id %in% output$defense)
    rundefgrade <- mean(mapply(rnorm, 1, mean = defensedf$def_run_defense_grade_mean,
                               sd = defensedf$def_run_defense_grade_sd))
    totaldefensegrade <- rundefgrade
    ### SAME THING ABOUT FEATURE ENGINEERING
    dummydf$pff_player_grade_component <- totaloffensegrade - totaldefensegrade
    ###
    rush_yards <- round(apply(posterior_predict(rushing_yds_model, newdata = dummydf), 2, sample, size = 1),0)
    fumbleprob <- predict(rushfumble_model, newdata = dummydf, type = "response")
    fumble <- sample(c("Yes", "No"), 1, prob = c(fumbleprob, 1-fumbleprob))
    if(fumble=="No"){
      result <- "No_Fumble"
    } else{
      fldf <- fumblostdf |> filter(fumbleposition==rushingpos)
      fumblelost <- sample(c("Yes", "No"), 1, prob = c(fldf$lostprob, 1-fldf$lostprob))
      if(fumblelost=="No"){
        result <- "Fumble_Retained"
      } else{
        result <- "Fumble_Lost"
      }
    }
    list(runpass = "Run", result = result, yards = rush_yards)
  }
}

safeyardsgained <- function(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                            off_dat, def_dat){
  safeydsfun <- safely(yardsgained)
  out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                     off_dat, def_dat))
  while(!is.null(out$error) | is.null(out$result) | any(is.na(out$result))){
    out <- suppressWarnings(safeydsfun(posstm, deftm, down, togo, YdsBef, posstmdiff, quarter_secs, quarter,
                                       off_dat, def_dat))
  }
  out$result
}


simulator <- function(team1, team2, year = 2025, track = "NO") {
  track <- track
  play <- list(yards = 0, runpass = "Run", result = "Default")
  # Initialize all game state variables
  game_state <- list(
    quarter = 1,
    secondsleft = 3600,
    quartersecondsleft = 900,
    down = 1,
    togo = 10,
    ydsbef = 70,
    teams = c(team1, team2),
    scoredf = data.frame(x1 = 0, x2 = 0),
    posstm = NULL,
    deftm = NULL,
    posstmmargin = 0,
    playtime = NULL,
    afterplaytime = NULL,
    option = NULL
  )

  whole_team1 <- full_PFF(team1, year)
  whole_team2 <- full_PFF(team2, year)

  colnames(game_state$scoredf) <- c(team1, team2)
  game_state$posstm <- sample(game_state$teams, 1, prob = c(.5, .5))
  game_state$deftm <- game_state$teams[which(c(team1, team2) != game_state$posstm)]
  game_state$startoffteam <- game_state$posstm
  game_state$startdefteam <- game_state$deftm

  # Helper functions (all take and return game_state)
  timeupdater <- function(game_state, playtime, afterplaytime) {
    game_state$secondsleft <- round(game_state$secondsleft - playtime - afterplaytime)
    game_state$quartersecondsleft <- round(game_state$quartersecondsleft - playtime - afterplaytime)
    return(game_state)
  }

  playtimecorrector <- function(game_state) {
    if(is.null(game_state$playtime) || game_state$playtime < 3) {
      game_state$playtime <- 3
    }
    return(game_state)
  }

  togocorrector <- function(game_state) {
    if(game_state$togo > game_state$ydsbef) {
      game_state$togo <- game_state$ydsbef
    }
    return(game_state)
  }

  posschange <- function(game_state) {
    temp <- game_state$posstm
    game_state$posstm <- game_state$deftm
    game_state$deftm <- temp
    return(game_state)
  }

  touchdown <- function(game_state) {
    if(game_state$posstmmargin %in% c(-1, -5, -8, -11, -15)) {
      mypoints <- sample(c(6,8), 1, prob = c(.49, .51))
    } else {
      mypoints <- sample(c(6,7), 1, prob = c(.025, .975))
    }
    return(mypoints)
  }

  afterplaytimegenerator <- function(game_state) {
    if(game_state$posstmmargin < 0 & game_state$secondsleft <= 240) {
      game_state$afterplaytime <- round(rnorm(1, 8.5, 1))
    } else {
      game_state$afterplaytime <- round(rnorm(1, 30, 3.5))
    }
    return(game_state)
  }

  posstmmargin_updater <- function(game_state) {
    game_state$posstmmargin <- as.numeric(game_state$scoredf[[game_state$posstm]] -
                                            game_state$scoredf[[game_state$deftm]])
    return(game_state)
  }

  quartercheck <- function(game_state) {
    if(game_state$quartersecondsleft <= 0) {
      game_state$quartersecondsleft <- 900
      game_state$secondsleft <- 3600 - (game_state$quarter * 900)
      game_state$quarter <- game_state$quarter + 1
      if(game_state$quarter == 3) {
        game_state$posstm <- game_state$startdefteam
        game_state$deftm <- game_state$startoffteam
        game_state$down <- 1
        game_state$togo <- 10
        game_state$ydsbef <- 70
        game_state <- posstmmargin_updater(game_state)
      }
    }
    return(game_state)
  }

  deftd <- function(game_state, playtimemean, playtimesd) {
    game_state$scoredf[[game_state$deftm]] <-
      as.numeric(game_state$scoredf[[game_state$deftm]]) + touchdown(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  touchback <- function(game_state, playtimemean, playtimesd) {
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 80
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  reg_turnover <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - as.numeric(game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  safety <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$deftm]] <-
      as.numeric(game_state$scoredf[[game_state$deftm]]) + 2
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 65
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  offtd <- function(game_state, playtimemean, playtimesd){
    game_state$scoredf[[game_state$posstm]] <-
      as.numeric(game_state$scoredf[[game_state$posstm]]) + touchdown(game_state)
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  firstdown <- function(game_state, playtimemean, playtimesd){
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  turnover_on_downs <- function(game_state, playtimemean, playtimesd){
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 100 - (game_state$ydsbef - play$yards)
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  regular_gain <- function(game_state, playtimemean, playtimesd){
    game_state$down <- game_state$down + 1
    game_state$togo <- game_state$togo - play$yards
    game_state$ydsbef <- game_state$ydsbef - play$yards
    game_state <- togocorrector(game_state)
    game_state$playtime <- round(rnorm(1, playtimemean, playtimesd))
    game_state <- playtimecorrector(game_state)
    game_state <- afterplaytimegenerator(game_state)
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  made_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state$scoredf[[game_state$posstm]] <-
      as.numeric(game_state$scoredf[[game_state$posstm]]) + 3
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- 70
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  missed_fg <- function(game_state){
    game_state$option <- "field goal attempt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) - 5
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 5, 1))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  punt <- function(game_state){
    game_state$option <- "punt"
    game_state <- posschange(game_state)
    game_state$down <- 1
    game_state$togo <- 10
    game_state$ydsbef <- (100-game_state$ydsbef) + round(rnorm(1, 45, 5))
    ### TOUCHBACK
    if(game_state$ydsbef>=100){
      game_state$ydsbef <- 80
    }
    game_state <- togocorrector(game_state)
    game_state <- posstmmargin_updater(game_state)
    game_state$playtime <- round(rnorm(1, 9, 1.5))
    game_state <- playtimecorrector(game_state)
    game_state$afterplaytime <- 0
    game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
    game_state <- quartercheck(game_state)
    return(game_state)
  }

  game_state <- posstmmargin_updater(game_state)

  # Main game loop
  while(game_state$secondsleft > 0) {
    game_state$scoredf$Quarter <- game_state$quarter
    game_state$scoredf$SecondsLeft <- game_state$quartersecondsleft
    game_state$scoredf$Possession <- game_state$posstm
    game_state$scoredf$Down <- game_state$down
    game_state$scoredf$ToGo <- game_state$togo
    game_state$scoredf$YdstoEZ <- game_state$ydsbef

    if(game_state$posstm==team1){
      off_dat <- whole_team1
      def_dat <- whole_team2
    } else if(game_state$posstm==team2){
      off_dat <- whole_team2
      def_dat <- whole_team1
    }


    if(game_state$down==4){
      if(game_state$ydsbef>40){
        ### GO FOR IT ON FOURTH
        if(game_state$quartersecondsleft<=240 & game_state$posstmmargin<0
           & game_state$togo<=10){
          game_state$option <- "goforit"
        } else{ ### PUNT
          game_state <- punt(game_state)
        }
      } else{
        ### GO FOR IT ON FOURTH
        if((game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=240 & game_state$
            posstmmargin<0 & game_state$togo<=3) |
           (game_state$quarter %in% c(2,4) & game_state$quartersecondsleft<=120 &
            game_state$posstmmargin<0)){
          game_state$option <- "goforit"
        } else{ ### FIELD GOAL
          game_state$option <- "field goal attempt"
          fgmakedf <- fgmakedf
          fgpredvardf <- data.frame(
            FGDist = game_state$ydsbef + 17,
            Quarter = game_state$quarter,
            Time2 = game_state$quartersecondsleft,
            PossTmMargin = game_state$posstmmargin,
            Down = game_state$down,
            ToGo = game_state$togo
          )
          fgattprob <- .9 # temporary to fix later (model was too big)
          fgattselection <- sample(c("Yes", "No"), 1, prob = c(fgattprob, 1-fgattprob))
          ### FGATT
          if(fgattselection=="Yes"){
            game_state$option <- "field goal attempt"
            fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
            fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
            ### MADE FIELD GOAL
            if(fgmake=="Yes"){
              game_state <- made_fg(game_state)
              ### MISSED FIELD GOAL
            } else{
              game_state <- missed_fg(game_state)
            }
            ### GO FOR IT ON FOURTH
          } else{
            game_state$option <- "goforit"
          }
        }
      }
    } else{
      game_state$option <- "regular"
    }
    ### OTHER field goal scenario
    if((game_state$secondsleft<=10 & game_state$posstmmargin>=-3 &
        game_state$posstmmargin<=0) |
       (game_state$quarter==2 & game_state$quartersecondsleft<=10)){
      game_state$option <- "field goal attempt"
      fgmakeprob <- fgmakedf$make_prob[fgmakedf$FGDist==game_state$ydsbef+17]
      fgmake <- sample(c("Yes", "No"), 1, prob = c(fgmakeprob, 1 - fgmakeprob))
      ### MADE FIELD GOAL
      if(fgmake=="Yes"){
        game_state <- made_fg(game_state)
        ### MISSED FIELD GOAL
      } else{
        game_state <- missed_fg(game_state)
      }
    }

    if((game_state$down!=4 | game_state$option=="goforit") &
       game_state$option!="field goal attempt"){

      play <- safeyardsgained(game_state$posstm, game_state$deftm, game_state$down,
                              game_state$togo, game_state$ydsbef, game_state$posstmmargin,
                              game_state$quartersecondsleft, game_state$quarter,
                              off_dat = off_dat, def_dat = def_dat)
    }
    if(play$result=="Sack" & play$yards > 0){
      play$yards <- -5
    }
    if(play$yards < -100){
      play$yards <- -100
    }
    if(play$yards > 100){
      play$yards <- 100
    }
    newyardsbef <- game_state$ydsbef - play$yards
    ################ PASS
    if(play$runpass=="Pass"){
      # INTERCEPTION
      if(play$result=="Interception"){
        ### PICK SIX
        if(newyardsbef>=100){
          game_state <- deftd(game_state, 8.5, .85)
          ### INTERCEPTION TOUCHBACK
        } else if(newyardsbef <= 0){
          game_state <- touchback(game_state, 5.5, .55)
          ### REGULAR INTERCEPTION
        } else{
          game_state <- reg_turnover(game_state, 6.5, .65)
        }
        # FUMBLE
      } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained",
                                   "Sack_Fumble_Lost", "Complete_Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result %in% c("Sack_Fumble_Lost", "Complete_Fumble_Lost")){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result %in% c("Sack_Fumble_Retained", "Complete_Fumble_Retained")){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 5.5, .5)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 7.5, .75)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 6, .6)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 6, .6)
          } else{
            print("FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # SACK
      } else if(play$result=="Sack"){
        ### SAFETY
        if(newyardsbef >= 100){
          game_state <- safety(game_state, 4.5, .45)
          ### TURNOVER ON DOWNS
        } else if(game_state$down==4 & (newyardsbef < 100)){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### REGULAR SACK
        } else if(game_state$down!=4 & (newyardsbef < 100)){
          game_state <- regular_gain(game_state, 4.5, .45)
        } else{
          print("SACK SCENARIO NOT CAPTURED")
        }
        # COMPLETE PASSES
      } else if(play$result=="Complete"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 5, .5)
          ### OFF TOUCHDOWN
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 7.5, .75)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6.5, .65)
          ### REGULAR NON FOURTH DOWN NO FIRST DOWN NO TOUCHDOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 6.5, .65)
        } else{
          print("COMPLETE PASSES SCENARIO NOT CAPTURED")
        }
        # INCOMPLETE PASSES
      } else if(play$result=="Incomplete"){
        ### TURNOVER ON DOWNS
        if(game_state$down==4){
          game_state <- turnover_on_downs(game_state, 6, .6)
          ### REGULAR INCOMPLETION
        } else if(game_state$down!=4){
          game_state$down <- game_state$down + 1
          game_state$playtime <- round(rnorm(1, 6, .6))
          game_state <- playtimecorrector(game_state)
          game_state$afterplaytime <- 0
          game_state <- timeupdater(game_state, game_state$playtime, game_state$afterplaytime)
          game_state <- quartercheck(game_state)
        } else{
          print("INCOMPLETE PASSES SCENARIO NOT CAPTURED")
        }
      } else{
        print("PASS SCENARIO NOT CAPTURED")
      }
      ################ RUN
    } else if(play$runpass=="Run"){
      # FUMBLE
      if(play$result %in% c("Fumble_Retained", "Fumble_Lost")){
        ## FUMBLE LOST
        if(play$result=="Fumble_Lost"){
          ### DEF TD
          if(newyardsbef>=100){
            game_state <- deftd(game_state, 5, .5)
            ### TOUCHBACK
          } else if(newyardsbef<=0){
            game_state <- touchback(game_state, 5, .5)
            ### REGULAR TURNOVER
          } else{
            game_state <- reg_turnover(game_state, 5, .5)
          }
          ## FUMBLE RETAINED
        } else if(play$result=="Fumble_Retained"){
          ### SAFETY
          if(newyardsbef>=100){
            game_state <- safety(game_state, 4.5, .45)
            ### OFF TD
          } else if(newyardsbef<=0){
            game_state <- offtd(game_state, 7.5, .75)
            ### FIRST DOWN
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
            game_state <- firstdown(game_state, 6.5, .65)
            ### TURNOVER ON DOWNS
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down==4){
            game_state <- turnover_on_downs(game_state, 4.5, .45)
            ### NON FIRST DOWN REGULAR PLAY
          } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                    & game_state$down!=4){
            game_state <- regular_gain(game_state, 5, .5)
          } else{
            print("RUN FUMBLE RETAINED SCENARIO NOT CAPTURED")
          }
        }
        # NON FUMBLE RUNS
      } else if(play$result=="No_Fumble"){
        ### SAFETY
        if(newyardsbef>=100){
          game_state <- safety(game_state, 4.5, .45)
          ### OFF TD
        } else if(newyardsbef<=0){
          game_state <- offtd(game_state, 7.5, .75)
          ### FIRST DOWN
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards>=game_state$togo)){
          game_state <- firstdown(game_state, 6, .6)
          ### TURNOVER ON DOWNS
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down==4){
          game_state <- turnover_on_downs(game_state, 4.5, .45)
          ### NON FIRST DOWN REGULAR PLAY
        } else if((newyardsbef>0) & (newyardsbef<100) & (play$yards<game_state$togo)
                  & game_state$down!=4){
          game_state <- regular_gain(game_state, 5, .5)
        } else{
          print("NON FUMBLE RUN SCENARIO NOT CAPTURED")
        }
      }
      else{
        print("RUN SCENARIO NOT CAPTURED")
      }
    }
    else{
      print("RUNPASS IS NOT A RUN OR PASS")
    }
    game_state$scoredf$Detail <- paste0(play$result, "; Yards: ", play$yards)
    game_state$scoredf <- game_state$scoredf |> relocate((Quarter:YdstoEZ), .before = all_of(team1))
    if(track %in% c("YES", "Y", "Yes", "yes", "y")){
      print(game_state$scoredf)
    }
  }
  return(game_state$scoredf)
}

multiple_simulations <- function(team1, team2, n = 100, max_attempts = 3) {
  resultlist <- vector("list", n)
  successful_runs <- 0
  attempts <- 0

  while(successful_runs < n && attempts < n * max_attempts) {
    attempts <- attempts + 1
    start <- Sys.time()
    message(paste0("Attempt ", attempts, " Start Time: ", start))

    # Initialize res as NULL before the try block
    res <- NULL
    timed_out <- FALSE

    # Try with time limit
    try_result <- try({
      setTimeLimit(elapsed = 240, transient = TRUE)  # 4 minutes = 240 seconds
      res <- simulator(team1, team2)
      setTimeLimit(elapsed = Inf, transient = TRUE)  # Reset time limit
    }, silent = TRUE)

    if(inherits(res, "try-error")) {
      # Check if the error was due to a timeout
      if(grepl("reached elapsed time limit", try_result[1])) {
        timed_out <- TRUE
        message("Simulation timed out after 4 minutes - retrying...")
      } else {
        message(sprintf("Simulation failed with error: %s", try_result[1]))
      }
      next
    }

    successful_runs <- successful_runs + 1
    resultlist[[successful_runs]] <- res
    message(paste0("Completed ", successful_runs, "/", n, " in ",
                   difftime(Sys.time(), start, units = "secs"), " secs"))
  }

  if(successful_runs == 0) {
    warning("All simulations failed")
    return(NULL)
  }

  # Combine only successful runs
  combined_results <- do.call(rbind, resultlist[1:successful_runs])

  data.frame(
    samplesize = successful_runs,
    team1 = team1,
    team1wins = sum(combined_results[[team1]] > combined_results[[team2]]),
    team1mean = mean(as.numeric(combined_results[[team1]])),
    team1sd = sd(as.numeric(combined_results[[team1]])),
    team2 = team2,
    team2wins = sum(combined_results[[team2]] > combined_results[[team1]]),
    team2mean = mean(as.numeric(combined_results[[team2]])),
    team2sd = sd(as.numeric(combined_results[[team2]]))
  )
}

whole_week_simulations <- function(year, weeknum, tm1vec = c(), tm2vec = c()){
  if(is_empty(tm1vec) & is_empty(tm2vec)){
    weekdf <- nflreadr::load_schedules() |>
      filter(season == year & week == weeknum) |>
      select(season, week, away_team, home_team)
    tm1 <- weekdf$away_team
    tm2 <- weekdf$home_team
  } else{
    tm1 <- tm1vec
    tm2 <- tm2vec
  }
  all_simulations <- list()
  for(i in 1:length(tm1)){
    print(paste0("Team 1: ", tm1[i], " vs. Team 2: ", tm2[i]))
    simulation <- multiple_simulations(team1 = tm1[i], team2 = tm2[i])
    all_simulations <- list.append(all_simulations, simulation)
    print(simulation)
  }
  all_simulations
}


