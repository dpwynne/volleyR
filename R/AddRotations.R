#' Add Rotations
#'
#' Creates an additional set of four columns indicating the rotation numbers for the home team, visiting team, team performing the skill, and opposing team
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with four new columns (home_rotation, visiting_rotation, team_rotation, opponent_rotation)
#'
#' @export

AddRotations <- function(plays){
  plays %>% mutate(home_rotation = fct_recode(home_setter_position, `1` = 1, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2),
                   visiting_rotation = fct_recode(visiting_setter_position, `1` = 1, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2),
                   team_rotation = case_when(team == home_team ~ home_rotation,
                                             team == visiting_team ~ visiting_rotation),
                   opponent_rotation = case_when(team == home_team ~ visiting_rotation,
                                                 team == visiting_team ~ home_rotation))

}
