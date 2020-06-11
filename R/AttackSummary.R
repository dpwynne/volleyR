#' Attack Summary
#'
#' Summarizes all attacks in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @export
AttackSummary <- function(plays, ...){
  attacks <- plays %>%
    filter(skill == "Attack")

  output <- attacks %>%
    group_by(...) %>%
    summarise(Attempts = n(),
              Kills = sum(evaluation_code == "#"),
              `Kill%` = Kills/Attempts,
              Stuffs = sum(evaluation_code == "/"),
              `Stuff%` = Stuffs/Attempts,
              Errors = sum(evaluation_code == "="),
              `Error%` = Errors/Attempts,
              P0 = sum(evaluation_code == "+"),
              `P0%` = sum(evaluation_code == "+")/Attempts,
              M0 = sum(evaluation_code == "-"),
              `M0%` = sum(evaluation_code == "-")/Attempts,
              Efficiency = (Kills - Errors - Stuffs)/Attempts,
              Efficacy = (Kills + P0 - Errors - Stuffs - M0)/Attempts,
              FBSO = (sum(evaluation_code == "#" & phase == "Reception") - sum(evaluation_code == "/" & phase == "Reception") - sum(evaluation_code == "=" & phase == "Reception"))/sum(phase == "Reception"),
              TRANS = (sum(evaluation_code == "#" & phase == "Transition") - sum(evaluation_code == "/" & phase == "Transition") - sum(evaluation_code == "=" & phase == "Transition"))/sum(phase == "Transition"))

  return(output)
}
