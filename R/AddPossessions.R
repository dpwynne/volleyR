#' Add Possessions
#'
#' Adds the possession number and contact number within possession
#'
#' @param plays a dv_plays object or data frame containing play-by-play data
#'
#' @return The same object, with three new columns:
#'  possession_start - whether the touch starts a new possession,
#'  possession_number - the number of the possession - Serve = 1, first ball = 2, then incremented
#'  contact_number - the number of the contact within the possession
#'
#'  Contact number 4 is common (usually corresponding to a block touch), contact numbers above 4 are rare but do occasionally happen
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr if_else
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

AddPossessions <- function(plays){

  output <- plays %>% group_by(match_id, point_id) %>%  # grab match-points
    mutate(team_switch = (team != lag(team) | is.na(lag(team))),  # find where the team changes
           possession_start = case_when(  # find touch where possession starts
             is.na(skill) ~ NA,  # if skill is NA then no touch
             skill %in% c("Dig", "Reception", "Freeball") & evaluation == "Error" ~ FALSE,
             lag(skill) == "Block" ~ TRUE,  # possession always changes after a block
             team_switch & skill != "Block" ~ TRUE, # possession changes if new team touches unless it's a block
             TRUE ~ FALSE  # if otherwise, it's not a possession-starting touch
           ),
           possession_number = cumsum(possession_start & !is.na(possession_start))  # counts possessions in the point; serve = 1, reception/first ball = 2
    ) %>% group_by(match_id, point_id, possession_number) %>%
    mutate(contact_number = seq(1, n())
           ) %>% ungroup() %>%
    mutate(contact_number = if_else(is.na(skill), NA_integer_, contact_number)) %>%
    select(-team_switch)  # get rid of confusing team_switch variable

  return(output)

}

