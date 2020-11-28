#' Add Cover
#'
#' Replaces the "Dig" skill with "Cover" when the dig is classified as a spike cover
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with "Cover" listed as a skill rather than skill_subtype
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

AddCover <- function(plays){

  output <- plays %>% mutate(skill = case_when(
    .data$skill_subtype == "Spike cover" ~ "Cover",
    is.na(.data$skill_subtype) ~ .data$skill,
    TRUE ~ .data$skill)
  )

  return(output)
}
