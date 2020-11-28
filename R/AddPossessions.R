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

