prep_plays <- function(plays){
  prepped <- plays %>%
    mutate(team = team_short(team),
           home_team = team_short(home_team),
           visiting_team = team_short(visiting_team),
           point_won_by = team_short(point_won_by),
           serving_team = team_short(serving_team),
           match = paste0(visiting_team, " @ ", home_team),
           team_run = find_runs(plays, "team")$run_id)

  return(prepped)
}
