#' Add Opponent
#'
#' Adds the opponent to a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return A dv_plays object or data frame with a new column showing the opponent
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#'
#' @export

AddOpponent <- function(plays){
  # adds a column to the plays object with the opponent

  team_opp_df <- plays %>% mutate(opponent = if_else(.data$team == .data$home_team, .data$visiting_team, .data$home_team))
  return(team_opp_df)
}
