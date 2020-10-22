#' Add Attack System
#'
#' Creates an additional column indicating whether the attack is a high ball (Out of System) or not (In System)
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with a new column (attack_system)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_sub
#' @export

AddAttackSystem <- function(plays){

  if("attack_system" %in% names(plays)){  # if we already have attack_system don't add it again
    return(plays)
  } else {
    return(
      plays %>% mutate(attack_system = if_else(
        .data$skill == "Attack", str_sub(.data$code, 5, 5),
        as.character(NA)
      )) %>%
        mutate(attack_system = case_when(
          .data$attack_system == "H" ~ "Out of System",
          .data$attack_system %in% c("M", "N", "O", "Q", "S","T", "U") ~ "In System",
          is.na(.data$attack_system) ~ as.character(NA)
        ))
    )
  }

}
