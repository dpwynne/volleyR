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
    filter(.data$skill == "Block")

  output <- blocks %>%
    group_by(...) %>%
    summarize(Touches =  n(),
              Stuffs = BlockStuffs(.data$evaluation),
              GT = .data$Stuffs + BlockPlus(.data$evaluation),
              `GT%` = .data$GT/.data$Touches,
              Errors = Errors(.data$evaluation),
              `Error%` = .data$Errors/.data$Touches)

  return(output)
}
