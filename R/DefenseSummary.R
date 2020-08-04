#' Dig Summary
#'
#' Summarizes all non-cover digs in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom rlang .data
#'
#' @export
DigSummary <- function(plays, ...){

  ### Find all dig attempts that lead to an attack (CRT)
  # For a play to be counted towards CRT, it must meet the following criteria:
  # (1) It must be a dig attempt (spike covers do not count)
  # (2) There must be an attack within the next two plays
  # (3) The attack must be by the same team that made the dig
  # (4) The dig must allow for a "structured" attack, in other words, it must be in system
  # Note: In theory, the attack can come on the play right after the dig. In practice, those plays almost universally violate criterion (4).

  attempt_ids <- which(plays$skill == "Dig" & (plays$skill_subtype != "Spike cover" | is.na(plays$skill_subtype)) & plays$evaluation_code != "-")

  CRT_1 <- which(rownames(plays) %in% (attempt_ids+1) & plays$skill == "Attack") - 1
  CRT_1 <- CRT_1[rank(CRT_1) %in% which(plays$team[CRT_1] == lead(plays$team[CRT_1]))]
  CRT_2 <- which(rownames(plays) %in% (attempt_ids+2) & plays$skill == "Attack") - 2
  CRT_2 <- CRT_2[rank(CRT_2) %in% which(plays$team[CRT_2] == lead(plays$team[CRT_2], 2))]

  CRT_ids <- unique(c(CRT_1, CRT_2))

  ### Find all dig attempts that lead to a kill (CNT)
  # These plays have the same critera as CRT but the attack must be a kill
  # In attacks, the point can be recorded immediately, after a reception error, or after a poor reception that leads to a setting error. Here, only the first two options are allowed.
  CNT_1 <- which(rownames(plays) %in% (attempt_ids+1) & plays$skill == "Attack" & plays$evaluation_code == "#") - 1
  CNT_1 <- CNT_1[rank(CNT_1) %in% which(plays$team[CNT_1] == plays$team[CNT_1 + 1])]
  CNT_2 <- which(rownames(plays) %in% (attempt_ids+2) & plays$skill == "Attack" & plays$evaluation_code == "#") - 2
  CNT_2 <- CNT_2[rank(CNT_2) %in% which(plays$team[CNT_2] == plays$team[CNT_2 + 2])]

  CNT_ids <- c(CNT_1, CNT_2) %>% unique()
  CNT_ids <- which(rownames(plays) %in% (CNT_ids + 3) & (plays$evaluation_code != "!" | is.na(plays$evaluation_code))) - 3

  CRT <- plays[CRT_ids,] %>%
    group_by(...) %>%
    summarize(CRT = n())

  CNT <- plays[CNT_ids,] %>%
    group_by(...) %>%
    summarize(CNT = n())


  ### Produce output
  defense <- plays %>%
    filter(.data$skill == "Dig") %>%
    filter(.data$skill_subtype != "Spike cover" | is.na(.data$skill_subtype))

  # Note: suppress messages because we don't know exactly what to join by, it's in the ...
  output <- suppressMessages({
    defense %>%
    group_by(...) %>%
    summarize(Touched = n(),
              Digs = sum(.data$evaluation != "Error")) %>%
    left_join(CRT) %>%
    left_join(CNT) %>%
    mutate(`T%` = .data$Touched/sum(.data$Touched),
           `Digs%` = .data$Digs/.data$Touched,
           `CRT%` = .data$CRT/.data$Touched,
           `CNT%` = .data$CNT/.data$Touched) %>%
    select(..., .data$Touched, .data$`T%`, .data$Digs, .data$`Digs%`, .data$CRT, .data$`CRT%`, .data$CNT, .data$`CNT%`)

  })
  # Note: the select function may not work properly

  return(output)
}
