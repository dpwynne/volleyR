#' Block Summary
#'
#' Summarizes all blocks in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
#' @export
BlockSummary <- function(plays, ...){
  blocks <- plays %>%
    filter(~ skill == "Block")

  # Same thing here: switch the evaluation code to evaluation
  output <- blocks %>%
    group_by(...) %>%
    summarize(Stuffs = ~ sum(evaluation_code == "#"),
              Touches = ~ n(),
              GT = ~ sum(evaluation_code %in% c("+", "#")),
              `GT%` = ~ GT/Touches,
              Errors = ~ sum(evaluation_code == "="),
              `Error%` = ~ Errors/Touches)

  return(output)
}
