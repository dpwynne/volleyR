#' FirstBallAttacks
#'
#' Joins serve receptions in a DataVolley play-by-play file with the resulting attack (if any)
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return A grouped data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr slice
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#'
#' @export
FirstBallAttacks <- function(plays){

  reception_ids <- which(plays$skill == "Reception")
  FB_attack_ids <- which(plays$skill == "Attack" & plays$phase == "Reception") # Row numbers of all attacks

  receptions <- plays %>% slice(reception_ids)
  FB_attacks <- plays %>% slice(FB_attack_ids) %>% select(.data$match_id,
                                                          .data$point_id,
                                                          attack_team = .data$team,
                                                          attack_result = .data$evaluation)
  reception_attacks <- left_join(receptions, FB_attacks, by = c("match_id", "point_id"))

  return(reception_attacks)
}
