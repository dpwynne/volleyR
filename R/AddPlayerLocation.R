#' Add Player Location
#'
#' Creates an additional set of two columns indicating the court location and presumed position of the player
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with two new columns (player_location, player_position)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @importFrom forcats fct_relevel
#' @importFrom rlang .data
#' @export

AddPlayerLocation <- function(plays){

  if(!("team_rotation" %in% names(plays))) plays <- AddRotations(plays)

  plays <- plays %>% mutate(team_rotation = fct_relevel(.data$team_rotation, c("1", "2", "3", "4", "5", "6")))

  player_location <- plays %>% mutate(
    player_location = if_else(
      .data$team == .data$home_team,
      case_when(.data$player_number == .data$home_p1 ~ "1",
                .data$player_number == .data$home_p2 ~ "2",
                .data$player_number == .data$home_p3 ~ "3",
                .data$player_number == .data$home_p4 ~ "4",
                .data$player_number == .data$home_p5 ~ "5",
                .data$player_number == .data$home_p6 ~ "6",
                !is.na(.data$player_number) ~ "L"),
      case_when(.data$player_number == .data$visiting_p1 ~ "1",
                .data$player_number == .data$visiting_p2 ~ "2",
                .data$player_number == .data$visiting_p3 ~ "3",
                .data$player_number == .data$visiting_p4 ~ "4",
                .data$player_number == .data$visiting_p5 ~ "5",
                .data$player_number == .data$visiting_p6 ~ "6",
                !is.na(.data$player_number) ~ "L")
    ),
    player_position = case_when(
      .data$player_location == "1" ~ case_when(
        .data$team_rotation == "1" ~ "Setter",
        .data$team_rotation == "2" ~ "OH_Back",
        .data$team_rotation == "3" ~ "MB_Back",
        .data$team_rotation == "4" ~ "Oppo",
        .data$team_rotation == "5" ~ "OH_Back",
        .data$team_rotation == "6" ~ "MB_Back"
      ),
      .data$player_location == "2" ~ case_when(
        .data$team_rotation == "1" ~ "OH_Front",
        .data$team_rotation == "2" ~ "MB_Front",
        .data$team_rotation == "3" ~ "Oppo",
        .data$team_rotation == "4" ~ "OH_Front",
        .data$team_rotation == "5" ~ "MB_Front",
        .data$team_rotation == "6" ~ "Setter"
      ),
      .data$player_location == "3" ~ case_when(
        .data$team_rotation == "1" ~ "MB_Front",
        .data$team_rotation == "2" ~ "Oppo",
        .data$team_rotation == "3" ~ "OH_Front",
        .data$team_rotation == "4" ~ "MB_Front",
        .data$team_rotation == "5" ~ "Setter",
        .data$team_rotation == "6" ~ "OH_Front"
      ),
      .data$player_location == "4" ~ case_when(
        .data$team_rotation == "1" ~ "Oppo",
        .data$team_rotation == "2" ~ "OH_Front",
        .data$team_rotation == "3" ~ "MB_Front",
        .data$team_rotation == "4" ~ "Setter",
        .data$team_rotation == "5" ~ "OH_Front",
        .data$team_rotation == "6" ~ "MB_Front"
      ),
      .data$player_location == "5" ~ case_when(
        .data$team_rotation == "1" ~ "OH_Back",
        .data$team_rotation == "2" ~ "MB_Back",
        .data$team_rotation == "3" ~ "Setter",
        .data$team_rotation == "4" ~ "OH_Back",
        .data$team_rotation == "5" ~ "MB_Back",
        .data$team_rotation == "6" ~ "Oppo"
      ),
      .data$player_location == "6" ~ case_when(
        .data$team_rotation == "1" ~ "MB_Back",
        .data$team_rotation == "2" ~ "Setter",
        .data$team_rotation == "3" ~ "OH_Back",
        .data$team_rotation == "4" ~ "MB_Back",
        .data$team_rotation == "5" ~ "Oppo",
        .data$team_rotation == "6" ~ "OH_Back"
      ),
      .data$player_location == "L" ~ "Libero"
    )
  )
  return(player_location)
}
