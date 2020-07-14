#' Reception Summary
#'
#' Summarizes all serve receptions in a DataVolley play-by-play file
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
#' @importFrom dplyr case_when
#' @importFrom forcats fct_recode
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom rlang .data
#'
#' @export
ReceptionSummary <- function(plays, ...){



  receptions <- plays %>%
    FirstBallAttacks() %>%
    mutate(receive_rotation = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                        .data$team != .data$home_team ~ .data$visiting_setter_position)) %>%
    mutate(receive_rotation = fct_recode(as.factor(.data$receive_rotation), `1` = "1", `2` = "6", `3` = "5", `4` = "4", `5` = "3", `6` = "2"))

  output <- receptions %>%
    group_by(...) %>%
    summarize(Attempts = n(),
              Passing_Grade_3 = ReceptionGrade(.data$evaluation, c(0, 0.5, 1, 2, 3, 3))/.data$Attempts,
              Passing_Grade_4 = ReceptionGrade(.data$evaluation, c(0, 0.5, 1, 2, 3, 4))/.data$Attempts,
              Good_Passes = ReceptionGood(.data$evaluation),
              `GoodPass%` = .data$Good_Passes/.data$Attempts,
              `EandO%` = (Errors(.data$evaluation) + ReceptionOverpass(.data$evaluation))/.data$Attempts,
              FB_Attacks = sum(!is.na(.data$attack_result)),
              FB_CRT = .data$FB_Attacks/.data$Attempts,
              FB_Kills = Kills(.data$attack_result),
              FB_Stuffs = AttackStuffs(.data$attack_result),
              FB_Errors = Errors(.data$attack_result),
              `FBSO%` = .data$FB_Kills/.data$Attempts,
              FB_Efficiency = AttackEff(.data$FB_Attacks, .data$FB_Kills, .data$FB_Stuffs, .data$FB_Errors),
              Jump_Serves = sum(.data$skill_type == "Jump serve reception"),
              J_pct = ReceptionGood(.data$evaluation[.data$skill_type == "Jump serve reception"])/.data$Jump_Serves,
              Jump_Floats = sum(.data$skill_type == "Jump-float serve reception"),
              JF_pct = ReceptionGood(.data$evaluation[.data$skill_type == "Jump-float serve reception"])/.data$Jump_Floats,
              Standing_Floats = sum(.data$skill_type == "Topspin serve reception"),
              SF_pct = ReceptionGood(.data$evaluation[.data$skill_type == "Topspin serve reception"])/.data$Standing_Floats,
              R1_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "1"),
              R2_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "2"),
              R3_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "3"),
              R4_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "4"),
              R5_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "5"),
              R6_pct = ReceptionRotationScore(.data$evaluation, .data$receive_rotation, "6"))

  return(output)
}

