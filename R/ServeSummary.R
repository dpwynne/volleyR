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
  attack_ids <- which(plays$skill == "Attack") # Row numbers of all attacks

  is_received <- plays$skill[serve_ids+1] == "Reception"  # Logical vector: TRUE if the serve is recieved
  is_returned <- plays[findnext(serve_ids, attack_ids),]$team_run == plays[serve_ids+1,]$team_run # Logical vector: TRUE if the serve allows the opponent to attack
  is_side_out <- plays[findnext(serve_ids, attack_ids),]$evaluation_code == "#" # Logical vector: TRUE if the next attack is a kill

  serves <- plays %>%
    filter(.data$skill == "Serve") %>%
    mutate(is_received = is_received,
           is_FBSO = is_received & is_returned & is_side_out) # Logical vector: TRUE if the serve leads to the opponent's getting a kill.


  output <- serves %>%
    group_by(...) %>%
    summarize(Attempts = n(),
              Aces = Aces(.data$evaluation),
              `Ace%` = .data$Aces/.data$Attempts,
              Errors = Errors(.data$evaluation),
              `Error%` = .data$Errors/.data$Attempts,
              `Ace:Error` = .data$Aces/.data$Errors,
              `Opp_FBSO%` = sum(is_FBSO)/sum(na.omit(is_received)),
              Points = sum(.data$point_won_by == .data$team),
              Sideouts = .data$Attempts - .data$Points,
              `Point_Scoring%` = .data$Points/.data$Attempts)

  return(output)
}
