#' Serve Summary
#'
#' Summarizes all serves in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @importFrom datavolley findnext
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom stats na.omit
#' @importFrom rlang .data
#'
#' @export
ServeSummary <- function(plays, ...){

  serve_ids <- which(plays$skill == "Serve")   # Row numbers of all serves

  # have to remove attacks being in or out of system before creating FB_attacks, because
  if("attack_system" %in% colnames(plays)) plays <- plays %>% select(-.data$attack_system)

  FB_attacks <-   plays %>%
    FirstBallAttacks() %>% select(.data$match_id, .data$point_id, .data$attack_result, .data$attack_system)

  serve_attacks <- plays %>% slice(serve_ids) %>% left_join(FB_attacks, by = c("match_id", "point_id"))
  # Since only one serve on each point this should join perfectly

  is_received <- plays$skill[serve_ids+1] == "Reception"  # Logical vector: TRUE if the serve is recieved

  serves <- serve_attacks %>%
    mutate(is_received = is_received,
           is_attack = !is.na(.data$attack_result), # Logical vector: TRUE if the serve led to an attack
           is_FBSO = .data$attack_result == "Winning attack") # Logical vector: TRUE if the serve leads to the opponent's getting a kill.

  output <- suppressMessages(serves %>%
                               group_by(...) %>%
                               summarize(Attempts = n(),
                                         Aces = Aces(.data$evaluation),
                                         `Ace%` = .data$Aces/.data$Attempts,
                                         Errors = Errors(.data$evaluation),
                                         `Error%` = .data$Errors/.data$Attempts,
                                         `Ace:Error` = .data$Aces/.data$Errors,
                                         Opp_FB_Attacks = sum(na.omit(.data$is_attack)),
                                         Opp_FB_In_System_Attacks = sum(na.omit(.data$attack_system == "In System")),
                                         `Opp_Out_of_System%` = (.data$Attempts - .data$Errors - .data$Opp_FB_In_System_Attacks)/(.data$Attempts),
                                         Opp_FB_Kills = sum(na.omit(.data$is_FBSO)),
                                         Opp_FB_Errors = sum(na.omit(.data$attack_result %in% c("Error", "Blocked"))),
                                         Opp_FB_Efficiency = (.data$Opp_FB_Kills - .data$Opp_FB_Errors)/.data$Opp_FB_Attacks,
                                         Opp_FB_In_System_Kills = sum(na.omit(.data$attack_system == "In System" & .data$is_FBSO)),
                                         Opp_FB_In_System_Errors = sum(na.omit(.data$attack_system == "In System" & .data$attack_result %in% c("Error", "Blocked"))),
                                         Opp_FB_In_System_Efficiency = (.data$Opp_FB_In_System_Kills - .data$Opp_FB_In_System_Errors)/.data$Opp_FB_In_System_Attacks,
                                         `Opp_FBSO%` = sum(na.omit(.data$is_FBSO))/sum(na.omit(.data$is_received)),
                                         Points = sum(.data$point_won_by == .data$team),
                                         Sideouts = .data$Attempts - .data$Points,
                                         `Point_Scoring%` = .data$Points/.data$Attempts)
  )

  return(output)
}
