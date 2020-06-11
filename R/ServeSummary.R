#' Serve Summary
#'
#' Summarizes all serves in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @export
ServeSummary <- function(plays, ...){
  serve_ids <- which(plays$skill == "Serve")   # Row numbers of all serves
  attack_ids <- which(plays$skill == "Attack") # Row numbers of all attacks

  is_received <- plays[serve_ids+1,]$skill == "Reception"  # Logical vector: TRUE if the serve is recieved
  is_returned <- plays[findnext(serve_ids, attack_ids),]$team_run == plays[serve_ids+1,]$team_run # Logical vector: TRUE if the serve allows the opponent to attack
  is_side_out <- plays[findnext(serve_ids, attack_ids),]$evaluation_code == "#" # Logical vector: TRUE if the next attack is a kill

  serves <- plays %>%
    filter(skill == "Serve") %>%
    mutate(is_received = is_received,
           is_FBSO = is_received & is_returned & is_side_out) # Logical vector: TRUE if the serve leads to the opponent's getting a kill.


  output <- serves %>%
    group_by(...) %>%
    summarise(Attempts = n(),
              Aces = sum(evaluation_code == "#"),
              `Ace%` = Aces/Attempts,
              Errors = sum(evaluation_code == "="),
              `Error%` = Errors/Attempts,
              `Ace:Error` = Aces/Errors,
              `Opp_FBSO%` = sum(is_FBSO)/sum(na.omit(is_received)),
              Points = sum(point_won_by == team),
              Sideouts = Attempts - Points,
              `Point_Scoring%` = Points/Attempts)

  return(output)
}
