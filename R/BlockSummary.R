#' Block Summary
#'
#' Summarizes all blocks in a DataVolley play-by-play file
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#' @param ... Variables to group by.
#'
#' @return A grouped data frame
#'
#' @export
BlockSummary <- function(plays, ...){
  blocks <- plays %>%
    filter(skill == "Block")

  output <- blocks %>%
    group_by(...) %>%
    summarise(Stuffs = sum(evaluation_code == "#"),
              Touches = n(),
              GT = sum(evaluation_code %in% c("+", "#")),
              `GT%` = GT/Touches,
              Errors = sum(evaluation_code == "="),
              `Error%` = Errors/Touches)

  return(output)
}
