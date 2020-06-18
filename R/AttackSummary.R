#' Attack Summary
#'
#' Summarizes all attacks in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom rlang .data
#' @export
AttackSummary <- function(plays, ...){
  attacks <- plays %>%
    filter(.data$skill == "Attack")

  ## NOTE: two things we have to do
  # 1. switch from evaluation_code to evaluation for consistency with other packages
  # 2. abstract the more complex functions to their own function
  output <- attacks %>%
    group_by(...) %>%
    summarize(Attempts = n(),
              Kills = Kills(.data$evaluation),
              `Kill%` = .data$Kills/.data$Attempts,
              Stuffs = AttackStuffs(.data$evaluation),
              `Stuff%` = .data$Stuffs/.data$Attempts,
              Errors = Errors(.data$evaluation),
              `Error%` = .data$Errors/.data$Attempts,
              P0 =  P0(.data$evaluation),
              `P0%` = .data$P0/.data$Attempts,
              M0 = M0(.data$evaluation),
              `M0%` = .data$M0/.data$Attempts,
              Efficiency = AttackEff(.data$Attempts, .data$Kills, .data$Stuffs, .data$Errors),
              Efficacy = AttackEff(.data$Attempts, .data$Kills, .data$Stuffs, .data$Errors, .data$P0, .data$M0),
              FBSO = PhaseAttackEff(.data$evaluation, .data$phase, "Reception"),
              TRANS = PhaseAttackEff(.data$evaluation, .data$phase, "Transition"))

  return(output)
}
