#' Add Post-Attack Touches
#'
#' Adds information about whether the current (previous) attack was touched on the block and/or dig attempt
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with two more columns (attack_block, attack_dig).
#' For attack skills, the columns indicate the result of the block attempt and dig/cover attempt.
#' For other skills, the columns indicate whether the previous attack was block-touched and/or dug/covered.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom tidyr fill
#' @importFrom rlang .data
#'
#' @export
AddPostAttackTouches <- function(plays){

  attack_ids <- which(plays$skill == "Attack")
  attack1 <- attack_ids + 1
  attack2 <- attack_ids + 2

  freeball_ids <- which(plays$skill == "Freeball") ## Possession has changed but no attack

  block_touched <- plays$skill[attack1] == "Block" & plays$team[attack1] != plays$team[attack_ids]
  other_touch1 <- !is.na(plays$skill[attack1]) & !(plays$skill[attack1] %in% c("Block", "Dig"))
  blocked <- plays$evaluation[attack1] == "Winning block"
  tooled <- plays$skill[attack1] == "Block" & plays$evaluation[attack1] == "Error"
  dig_touched1 <- (plays$skill[attack1] == "Dig" & plays$team[attack1] != plays$team[attack_ids])
  dig_touched2 <- (plays$skill[attack1] == "Block" & plays$skill[attack2] == "Dig" & plays$team[attack2] != plays$team[attack_ids])
  dug1 <- (plays$skill[attack1] == "Dig" & plays$team[attack1] != plays$team[attack_ids] & plays$evaluation[attack1] != "Error")
  dug2 <- (plays$skill[attack1] == "Block" & plays$skill[attack2] == "Dig" & plays$team[attack2] != plays$team[attack_ids] & plays$evaluation[attack2] != "Error")

  other_touch2 <- plays$skill[attack1] == "Block" & !is.na(plays$skill[attack2]) & plays$skill[attack2] != "Dig"

  cover_touched <- (plays$skill[attack1] == "Block" & plays$skill[attack2] == "Dig" & plays$team[attack2] == plays$team[attack_ids])
  covered <- (plays$skill[attack1] == "Block" & plays$skill[attack2] == "Dig" & plays$team[attack2] == plays$team[attack_ids] & plays$evaluation[attack2] != "Error")

  block_touched[is.na(block_touched)] <- FALSE
  tooled[is.na(tooled)] <- FALSE
  dig_touched1[is.na(dig_touched1)] <- FALSE
  dug1[is.na(dug1)] <- FALSE
  dig_touched2[is.na(dig_touched2)] <- FALSE
  dug2[is.na(dug2)] <- FALSE
  cover_touched[is.na(cover_touched)] <- FALSE
  covered[is.na(covered)] <- FALSE

  dig_touched <- dig_touched1 | dig_touched2
  dug <- dug1 | dug2

  block_result <- rep(NA_character_, nrow(plays))
  block_result[attack_ids[!block_touched & !other_touch1]] <- "Untouched"
  block_result[attack_ids[other_touch1]] <- "Other touch"
  block_result[attack_ids[block_touched & blocked]] <- "Blocked"
  block_result[attack_ids[block_touched & tooled]] <- "Tooled"
  block_result[attack_ids[block_touched & !(blocked | tooled)]] <- "Block touched"
  block_result[freeball_ids] <- "Freeball, no block"

  dig_result <- rep(NA_character_, nrow(plays))
  dig_result[attack_ids[!(dig_touched | cover_touched)]] <- "Untouched"
  dig_result[attack_ids[other_touch2]] <- "Other touch"
  dig_result[attack_ids[dig_touched & dug]] <- "Dug"
  dig_result[attack_ids[dig_touched & !dug]] <- "Dig touched"
  dig_result[attack_ids[cover_touched & !covered]] <- "Cover touched"
  dig_result[attack_ids[cover_touched & covered]] <- "Covered"
  dig_result[freeball_ids] <- "Freeball"

  output <- plays %>% mutate(attack_block = block_result, attack_dig = dig_result) %>%
    group_by(.data$match_id, .data$point_id) %>%
    fill(.data$attack_block, .data$attack_dig, .direction = "down") %>%
    ungroup()
  output[which(is.na(output$skill)), c("attack_block", "attack_dig")] <- NA_character_

  return(output)
}

