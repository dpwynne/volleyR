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
#'
#' @export
ReceptionSummary <- function(plays, ...){

  reception_ids <- which(plays$skill == "Reception") # Row numbers of all receptions
  attack_ids <- which(plays$skill == "Attack") # Row numbers of all attacks
  is_same_run <- plays[reception_ids,]$team_run == plays[findnext(reception_ids, attack_ids),]$team_run  # Logical vector that takes TRUE if there is an attack in the same run as the reception.
  is_side_out <- plays[findnext(reception_ids, attack_ids),]$evaluation_code == "#"  # Logical vector that takes TRUE if the next attack is a kill

  receptions <- plays %>%
    filter(.data$skill == "Reception") %>%
    mutate(receive_rotation = case_when(.data$team == .data$home_team ~ .data$home_setter_position,
                                        .data$team != .data$home_team ~ .data$visiting_setter_position)) %>%
    mutate(receive_rotation = fct_recode(as.factor(.data$receive_rotation), `1` = "1", `2` = "6", `3` = "5", `4` = "4", `5` = "3", `6` = "2")) %>%
    mutate(is_FBSO = is_same_run & is_side_out) # Logical vector that takes TRUE if the reception leads to a kill

  output <- receptions %>%
    group_by(...) %>%
    summarize(Attempts = n(),
              Passing_Grade_3 = ReceptionGrade(.data$evaluation, c(0, 0.5, 1, 2, 3, 3))/.data$Attempts,
              Passing_Grade_4 = ReceptionGrade(.data$evaluation, c(0, 0.5, 1, 2, 3, 4))/.data$Attempts,
              Good_Passes = ReceptionGood(.data$evaluation),
              `GoodPass%` = .data$Good_Passes/.data$Attempts,
              `EandO%` = (Errors(.data$evaluation) + ReceptionOverpass(.data$evaluation))/.data$Attempts,
              `FBSO%` = sum(.data$is_FBSO)/.data$Attempts,
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
