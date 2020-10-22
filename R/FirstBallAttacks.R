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
#' @importFrom dplyr lag
#' @importFrom rlang .data
#'
#' @export
FirstBallAttacks <- function(plays){

  reception_ids <- which(plays$skill == "Reception")
  FB_attack_ids <- which(plays$skill == "Attack" & plays$phase == "Reception") # Row numbers of all attacks

  receptions <- plays %>% slice(reception_ids)
  FB_attacks <- plays %>% slice(FB_attack_ids) %>% AddAttackSystem() %>%
    select(.data$match_id,
           .data$point_id,
           attack_team = .data$team,
           attack_result = .data$evaluation)

  # each reception can only have one attack
  duplicated_ids <- which(FB_attacks$match_id == lag(FB_attacks$match_id) & FB_attacks$point_id == lag(FB_attacks$point_id))
  if(length(duplicated_ids) > 0){
   FB_attacks <- FB_attacks[-duplicated_ids,]
  }

  # if we have already included the attack system then we need to remove it
  if("attack_system" %in% colnames(receptions)) receptions <- receptions %>% select(-.data$attack_system)

  reception_attacks <- left_join(receptions, FB_attacks, by = c("match_id", "point_id"))

  return(reception_attacks)
}
