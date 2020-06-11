#' Dig Summary
#'
#' Summarizes all non-cover digs in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
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
  CRT_1 <- CRT_1[rank(CRT_1) %in% which(plays[CRT_1,]$team == plays[CRT_1 + 1,]$team)]
  CRT_2 <- which(rownames(plays) %in% (attempt_ids+2) & plays$skill == "Attack") - 2
  CRT_2 <- CRT_2[rank(CRT_2) %in% which(plays[CRT_2,]$team == plays[CRT_2 + 2,]$team)]

  CRT_ids <- c(CRT_1, CRT_2) %>% unique()

  ### Find all dig attempts that lead to a kill (CNT)
  # These plays have the same critera as CRT but the attack must be a kill
  # Note: VM seems to be using a different definition of a kill here than they used in the Attack section. In attacks, the point can be recorded immediately, after a reception error, or after a poor reception that leads to a setting error. Here, only the first two options are allowed.
  CNT_1 <- which(rownames(plays) %in% (attempt_ids+1) & plays$skill == "Attack" & plays$evaluation_code == "#") - 1
  CNT_1 <- CNT_1[rank(CNT_1) %in% which(plays[CNT_1,]$team == plays[CNT_1 + 1,]$team)]
  CNT_2 <- which(rownames(plays) %in% (attempt_ids+2) & plays$skill == "Attack" & plays$evaluation_code == "#") - 2
  CNT_2 <- CNT_2[rank(CNT_2) %in% which(plays[CNT_2,]$team == plays[CNT_2 + 2,]$team)]

  CNT_ids <- c(CNT_1, CNT_2) %>% unique()
  CNT_ids <- which(rownames(plays) %in% (CNT_ids + 3) & (plays$evaluation_code != "!" | is.na(plays$evaluation_code))) - 3



  CRT <- plays[CRT_ids,] %>%
    group_by(...) %>%
    summarise(CRT = n())

  CNT <- plays[CNT_ids,] %>%
    group_by(...) %>%
    summarise(CNT = n())


  ### Produce output
  defense <- plays %>%
    filter(skill == "Dig") %>%
    filter(skill_subtype != "Spike cover" | is.na(skill_subtype))

  output <- defense %>%
    group_by(...) %>%
    summarise(Touched = n(),
              Digs = sum(evaluation_code != "=")) %>%
    left_join(CRT) %>%
    left_join(CNT) %>%
    mutate(`T%` = Touched/sum(Touched),
           `Digs%` = Digs/Touched,
           `CRT%` = CRT/Touched,
           `CNT%` = CNT/Touched) %>%
    select(..., Touched, `T%`, Digs, `Digs%`, CRT, `CRT%`, CNT, `CNT%`)

  return(output)
}
