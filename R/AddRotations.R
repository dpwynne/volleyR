AddRotations <- function(plays){
  plays %>% mutate(home_rotation = recode(home_setter_position, `1` = 1, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2),
                   visiting_rotation = recode(visiting_setter_position, `1` = 1, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2),
                   team_rotation = case_when(team == home_team ~ home_rotation,
                                             team == visiting_team ~ visiting_rotation),
                   opponent_rotation = case_when(team == home_team ~ visiting_rotation,
                                                 team == visiting_team ~ home_rotation))
  
}
