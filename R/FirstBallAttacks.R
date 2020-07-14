FirstBallAttacks <- function(plays){
  reception_ids <- which(plays$skill == "Reception")
  FB_attack_ids <- which(plays$skill == "Attack" & plays$phase == "Reception") # Row numbers of all attacks

  receptions <- plays %>% slice(reception_ids)
  FB_attacks <- plays %>% slice(FB_attack_ids) %>% select(point_id,
                                                          attack_team = team,
                                                          attack_result = evaluation)
  reception_attacks <- left_join(receptions, FB_attacks, by = "point_id")

  return(reception_attacks)
}
