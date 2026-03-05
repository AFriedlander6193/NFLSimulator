
#' All Offensive Personnel
#'
#' A data set containing offensive personnel data from the Big Data Bowl 2025
#'
#' @format A data frame with 15606 rows and 296 variables:

"offdf"

#' Angle Model
#'
#' A model that predicts the number of Angle routes on a given play
#'
#' @format A multinomial model
#'
"angle_model"


#' Block Snaps Model
#'
#' A model that predicts an OLineman's snaps for a given week
#'
#' @format A linear model
#'
"block_model"

#' Corner Model
#'
#' A model that predicts the number of Corner routes on a given play
#'
#' @format A multinomial model
#'
"corner_model"

#' Cover Players Model
#'
#' A model that predicts the number of defensive players in coverage on a given play
#'
#' @format A Bayesian skew_normal model
#'
"coverplayersmodel"

#' Coverage Depth Snaps
#'
#' A data set containing average coverage snaps for front seven (F7) and secondary
#' (DB) players based on their position in the depth chart.
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{depth}{Position and spot on the depth chart}
#'   \item{count}{Number of observations (players snaps in a given week)}
#'   \item{mean_snaps}{The mean number of coverage snaps for that position + depth}
#' }
"checkcov"

#' Cross Model
#'
#' A model that predicts the number of Cross routes on a given play
#'
#' @format A multinomial model
#'
"cross_model"

#' Defensive Coverage Model
#'
#' A model that predicts the type of defensive coverage on a given play:
#' Cover 0 , Cover 1, Cover 2, Cover 3, Cover 4, Cover 6, Cover2Man, GoalLine,
#' Prevent, or RedZone
#'
#' @format A multinomial regression model
#'
"defcoveragemodel"

#' Defensive Coverage Snaps Model
#'
#' A model that predicts a defensive player's coverage snaps for a given week
#'
#' @format A linear model
#'
"def_cover_model"

#' Defensive Pass Rush Snaps Model
#'
#' A model that predicts a defensive player's pass rush snaps for a given week
#'
#' @format A linear model
#'
"def_pass_rush_model"

#' Defensive Personnel Model
#'
#' A model that predicts the type of defensive personnel on a given play:
#' 1-5, 2-4, 2-5, 3-3, 3-4, 4-2, 4-3, 5-1, 5-2, 6-1, Nickel, Dime, Heavy, or Goalline
#'
#' @format A multinomial regression model
#'
"defpersonnelmodel"

#' Field Goal Make Probability
#'
#' A data set containing the probability of making a field goal from distances
#' 17-99 yards
#'
#' @format A data frame with 101 rows and 2 variables:
#' \describe{
#'   \item{FGDist}{Distance of Field Goal Attempt}
#'   \item{make_prob}{Probability of making field goal from given distance}
#' }
"fgmakedf"

#' Flat Model
#'
#' A model that predicts the number of Flat routes on a given play
#'
#' @format A multinomial model
#'
"flat_model"

#' Fumble Lost Probability
#'
#' A data set containing the probability that a fumble was lost by a FB, QB, RB,
#' TE, or WR
#'
#' @format A data frame with 5 rows and 4 variables:
#'
"fumblostdf"

#' Go Model
#'
#' A model that predicts the number of Go routes on a given play
#'
#' @format A multinomial model
#'
"go_model"

#' Hitch Model
#'
#' A model that predicts the number of Hitch routes on a given play
#'
#' @format A multinomial model
#'
"hitch_model"

#' In Model
#'
#' A model that predicts the number of In routes on a given play
#'
#' @format A multinomial model
#'
"in_model"

#' Interception Model
#'
#' A model that predicts the probability of a passing play resulting in an interception
#'
#' @format A logistic regression model
#'
"interception_model"

#' Interception Yards Model
#'
#' A model that predicts a distribution of possible interception yard values
#'
#' @format A Bayesian skew_normal model
#'
"int_yds_model"

#' OLine Depth Snaps
#'
#' A data set containing average blocking snaps for OLinemen (OL) based on their
#' position in the depth chart.
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{depth}{Position and spot on the depth chart}
#'   \item{count}{Number of observations (players snaps in a given week)}
#'   \item{mean_snaps}{The mean number of blocking snaps for that position + depth}
#' }
"checkblock"

#' Out Model
#'
#' A model that predicts the number of Out routes on a given play
#'
#' @format A multinomial model
#'
"out_model"

#' Passing Yards Model
#'
#' A model that provides a distribution of possible yards on a given passing play
#'
#' @format A Bayesian skew_normal model
#'
"passing_yds_model"

#' Pass Play Result Model
#'
#' A model that predicts the outcome of a passing play:
#' COMPLETION, INCOMPLETION, SACK, FUMBLE, or INTERCEPTION
#'
#' @format A multinomial regression model
#'
"passresultmodel"

#' Pass Rush Depth Snaps
#'
#' A data set containing average pass rush snaps for front seven (F7) and secondary
#' (DB) players based on their position in the depth chart.
#'
#' @format A data frame with 3 rows and 3 variables:
#' \describe{
#'   \item{depth}{Position and spot on the depth chart}
#'   \item{count}{Number of observations (players snaps in a given week)}
#'   \item{mean_snaps}{The mean number of pass rush snaps for that position + depth}
#' }
"checkpassrush"

#' Offensive Personnel Model
#'
#' A model that predicts the type of offensive personnel on a given play:
#' 00, 01, 02, 03, 10, 11, 12, 13, 20, 21, 22, or 23
#'
#' @format A multinomial regression model
#'
"off_personnel_model"

#' PFF Defensive Coverage Grade Mean Model
#'
#' A model that predicts the defensive coverage grade of a DB or F7 for a week
#'
#' @format A linear model
#'
"PFF_def_coverage_grade_mean_model"

#' PFF Defensive Pass Rush Grade Mean Model
#'
#' A model that predicts the defensive pass rush grade of a DB or F7 for a week
#'
#' @format A linear model
#'
"PFF_def_pass_rush_grade_mean_model"

#' PFF Defensive Run Defense Grade Mean Model
#'
#' A model that predicts the defensive run defense grade of a DB or F7 for a week
#'
#' @format A linear model
#'
"PFF_def_run_grade_mean_model"

#' PFF Pass Blocking Grade Mean Model
#'
#' A model that predicts the pass blocking grade of a RB, WR, TE, or OL for a week
#'
#' @format A linear model
#'
"PFF_pass_blocking_grade_mean_model"

#' PFF Passing Grade Mean Model
#'
#' A model that predicts the passing grade of an NFL Quarterback for a week
#'
#' @format A linear model
#'
"PFF_passing_grade_mean_model"

#' PFF QB Rushing Grade Mean Model
#'
#' A model that predicts the rushing grade of an NFL Quarterback for a week
#'
#' @format A linear model
#'
"PFF_QB_rushing_grade_mean_model"

#' PFF Receiving Grade Mean Model
#'
#' A model that predicts the receiving grade of an NFL RB, WR, or TE for a week
#'
#' @format A linear model
#'
"PFF_receiving_grade_mean_model"

#' PFF Run Blocking Grade Mean Model
#'
#' A model that predicts the run blocking grade of an NFL RB, WR, TE, or OL for a week
#'
#' @format A linear model
#'
"PFF_run_blocking_grade_mean_model"

#' PFF Rushing Grade Mean Model
#'
#' A model that predicts the rushing grade of an NFL Runningback for a week
#'
#' @format A linear model
#'
"PFF_rushing_grade_mean_model"

#' PFF Blocking Data 2024
#'
#' A data set containing NFL player information on Blocking grades from 2024
#'
#' @format: TEMP HOLDER
"PFF_Blocking_2024"

#' PFF Blocking Data 2025
#'
#' A data set containing NFL player information on Blocking grades from 2025
#'
#' @format: TEMP HOLDER
"PFF_Blocking_2025"

#' PFF Blocking Snaps Data 2024
#'
#' A data set containing NFL offensive line player snap information for 2024.
#'
#' @format A data frame with 339 rows and 3 variables:
#' \describe{
#'   \item{player}{An NFL player}
#'   \item{position}{All are OL (abbreviation of Offensive Line)}
#'   \item{mean_snaps}{The mean number of snaps for the player in 2024}
#' }
"blockdf"

#' PFF Defense Data 2024
#'
#' A data set containing NFL player information on Defense grades from 2024
#'
#' @format: TEMP HOLDER
"PFF_Defense_2024"

#' PFF Defense Data 2025
#'
#' A data set containing NFL player information on Defense grades from 2025
#'
#' @format: TEMP HOLDER
"PFF_Defense_2025"

#' PFF Defense Snaps Data 2024
#'
#' A data set containing NFL defensive player information on coverage snaps,
#' pass rush snaps, and run defense snaps.
#'
#' @format A data frame with 1010 rows and 5 variables:
#' \describe{
#'   \item{player}{An NFL player}
#'   \item{position}{F7 (for front seven player) or DB (for secondary player)}
#'   \item{mean_cov_snaps}{The mean number of coverage snaps for the player in 2024}
#'   \item{mean_run_def_snaps}{The mean number of run defense snaps for the player in 2024}
#'   \item{mean_pass_rush_snaps}{The mean number of pass rush snaps for the player in 2024}
#' }
"defdf"

#' PFF Passing Data 2024
#'
#' A data set containing NFL player information on QB grades from 2024
#'
#' @format: TEMP HOLDER
"PFF_QBs_2024"

#' PFF Passing Data 2025
#'
#' A data set containing NFL player information on QB grades from 2025
#'
#' @format: TEMP HOLDER
"PFF_QBs_2025"

#' PFF Receiving Data 2024
#'
#' A data set containing NFL player information on Receiving grades from 2024
#'
#' @format: TEMP HOLDER
"PFF_Receiving_2024"

#' PFF Receiving Data 2025
#'
#' A data set containing NFL player information on Receiving grades from 2025
#'
#' @format: TEMP HOLDER
"PFF_Receiving_2025"

#' PFF Receiving Snaps Data 2024
#'
#' A data set containing mean number of routes run for NFL WRs and TEs in 2024.
#'
#' @format A data frame with 390 rows and 3 variables:
#' \describe{
#'   \item{player}{An NFL player}
#'   \item{position}{WR (for wide receiver) or TE (for tight end)}
#'   \item{mean_snaps}{The mean number of routes run for the player in 2024}
#' }
"recdf"

#' PFF Rushing Data 2024
#'
#' A data set containing NFL player information on Rushing grades from 2024
#'
#' @format: TEMP HOLDER
"PFF_Rushing_2024"

#' PFF Rushing Data 2025
#'
#' A data set containing NFL player information on Rushing grades from 2025
#'
#' @format: TEMP HOLDER
"PFF_Rushing_2025"

#' PFF Rushing Snaps Data 2024
#'
#' A data set containing mean number of rushing attempts for NFL RBs in 2024.
#'
#' @format A data frame with 138 rows and 3 variables:
#' \describe{
#'   \item{player}{An NFL player}
#'   \item{position}{All RB (for running back)}
#'   \item{mean_snaps}{The mean number of rush attempts for the player in 2024}
#' }
"rushdf"

#' Position Route Run Probability Data
#'
#' A data set containing probability that a route was run by a certain position.
#'
#' @format A data frame:
#'
"posroutedf"

#' Post Model
#'
#' A model that predicts the number of Post routes on a given play
#'
#' @format A multinomial model
#'
"post_model"

#' Pressure Players Model
#'
#' A model that predicts the number of players putting pressure on the QB on
#' a given play
#'
#' @format An ordinal regression model
#'
"pressureplayersmodel"

#' PFF QB Rushing Snaps Data 2024
#'
#' A data set containing mean number of rushing attempts for NFL QBs in 2024.
#'
#' @format A data frame
#'
#' }
"qbsnapsdf2024"


#' Run Defense Depth Snaps
#'
#' A data set containing average run defense snaps for front seven (F7) and secondary
#' (DB) players based on their position in the depth chart.
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{depth}{Position and spot on the depth chart}
#'   \item{count}{Number of observations (players snaps in a given week)}
#'   \item{mean_snaps}{The mean number of run defense snaps for that position + depth}
#' }
"checkrundef"

#' Receiver Route Model
#'
#' A model that predicts a recievers (WR or TE) routes ran for a given week
#'
#' @format A linear model
#'
"rec_route_model"

#' Route Count Model
#'
#' A model that predicts the number of routes on a given play
#'
#' @format A multinomial model
#'
"route_count_model"

#' Run Defense Snaps Model
#'
#' A model that predicts a defensive player's run defense snaps for a given week
#'
#' @format A linear model
#'
"def_run_model"

#' Run or Pass Model
#'
#' A model that predicts whether a given play will be a run or a pass
#'
#' @format A logistic regression model
#'
"runpass_model"

#' Rush Attempts Model
#'
#' A model that predicts a running back's rush attempts for a given week
#'
#' @format A linear model
#'
"rush_att_model"

#' Rushing Play Fumble Model
#'
#' A model that predicts the probability that a rushing play will result in a
#' fumble
#'
#' @format A logistic regression model
#'
"rushfumble_model"

#' Rushing Yards Model
#'
#' A model that provides a distribution for rushing yards on a given play
#'
#' @format A Bayesian skew_normal model
#'
"rushing_yds_model"

#' Sack Model
#'
#' The probability that a passing play results in a sack
#'
#' @format A logistic regression model
#'
"sack_model"

#' Sack Yards Model
#'
#' A model that provides a distribution for sack yards on a given play that
#' resulted in a sack
#'
#' @format A Bayesian gaussian model
#'
"sack_yards_model"

#' Screen Model
#'
#' A model that predicts the number of Screen routes on a given play
#'
#' @format A multinomial model
#'
"screen_model"

#' Slant Model
#'
#' A model that predicts the number of Slant routes on a given play
#'
#' @format A multinomial model
#'
"slant_model"

#' Targetted Route Model
#'
#' A model that predicts the route which will be targetted on a given play
#'
#' @format A multinomial regression model
#'
"targetted_route_model"

#' Team Position Rushing Probabilites
#'
#' A data set containing the probability of a certain position rushing the ball
#' for each NFL team
#'
#' @format A dataframe with 113 rows and 4 variables
#'
"teamrushposdf"

#' Time to Throw Model
#'
#' A model that provides a distribution for the time a quarterback will have
#' to throw the ball on a given play.
#'
#' @format A Bayesian skew_normal model
#'
"timeToThrowmodel"

#' Wheel Model
#'
#' A model that predicts the number of Wheel routes on a given play
#'
#' @format A multinomial model
#'
"wheel_model"

#' Receiver Depth Routes
#'
#' A data set containing average routes ran for wide receivers (WR) and tight
#' ends (TE) based on their position in the depth chart.
#'
#' @format A data frame with 6 rows and 3 variables:
#' \describe{
#'   \item{depth}{Position and spot on the depth chart}
#'   \item{count}{Number of observations (players snaps in a given week)}
#'   \item{mean_snaps}{The mean number of routes ran for that position + depth}
#' }
"checkrec"



