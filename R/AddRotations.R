#' Add Rotations
#'
#' Creates an additional set of four columns indicating the rotation numbers for the home team, visiting team, team performing the skill, and opposing team
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with four new columns (home_rotation, visiting_rotation, team_rotation, opponent_rotation)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom magrittr %>%
#' @importFrom forcats fct_recode
#' @importFrom rlang .data
#' @export

AddRotations <- function(plays){
  plays %>% mutate(home_rotation = fct_recode(as.factor(.data$home_setter_position), `1` = "1", `2` = "6", `3` = "5", `4` = "4", `5` = "3", `6` = "2"),
                   visiting_rotation = fct_recode(as.factor(.data$visiting_setter_position), `1` = "1", `2` = "6", `3` = "5", `4` = "4", `5` = "3", `6` = "2"),
                   team_rotation = case_when(.data$team == .data$home_team ~ .data$home_rotation,
                                             .data$team == .data$visiting_team ~ .data$visiting_rotation),
                   opponent_rotation = case_when(.data$team == .data$home_team ~ .data$visiting_rotation,
                                                 .data$team == .data$visiting_team ~ .data$home_rotation))

}
