library(datavolley)
library(tidyverse)

ReceptionSummary <- function(plays, ...){

  reception_ids <- which(plays$skill == "Reception") # Row numbers of all receptions
  attack_ids <- which(plays$skill == "Attack") # Row numbers of all attacks
  is_same_run <- plays[reception_ids,]$team_run == plays[findnext(reception_ids, attack_ids),]$team_run  # Logical vector that takes TRUE if there is an attack in the same run as the reception. 
  is_side_out <- plays[findnext(reception_ids, attack_ids),]$evaluation_code == "#"  # Logical vector that takes TRUE if the next attack is a kill
  
  receptions <- plays %>%
    filter(skill == "Reception") %>% 
    mutate(receive_rotation = case_when(team == home_team ~ home_setter_position,
                                        team != home_team ~ visiting_setter_position)) %>% 
    mutate(receive_rotation = recode(receive_rotation, `1` = 1, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2)) %>% 
    mutate(is_FBSO = is_same_run & is_side_out) # Logical vector that takes TRUE if the reception leads to a kill
  
  output <- receptions %>%
    group_by(...) %>%
    summarise(Attempts = n(),
              Passing_Grade_3 = (0.5*sum(evaluation_code == "/") +
                                   1*sum(evaluation_code == "-") +
                                   2*sum(evaluation_code == "!") +
                                   3*sum(evaluation_code %in% c("+", "#")))/Attempts,
              Passing_Grade_4 = (0.5*sum(evaluation_code == "/") +
                                   1*sum(evaluation_code == "-") +
                                   2*sum(evaluation_code == "!") +
                                   3*sum(evaluation_code == "+") +
                                   4*sum(evaluation_code == "#"))/Attempts,
              Good_Passes = sum(evaluation_code %in% c("#", "+")),
              `GoodPass%` = Good_Passes/Attempts,
              `EandO%` = sum(evaluation_code %in% c("=", "/"))/Attempts,
              `FBSO%` = sum(is_FBSO)/Attempts,
              Jump_Serves = sum(skill_type == "Jump serve reception"),
              J_pct = sum(evaluation_code %in% c("#", "+") & skill_type == "Jump serve reception")/Jump_Serves,
              Jump_Floats = sum(skill_type == "Jump-float serve reception"),
              JF_pct = sum(evaluation_code %in% c("#", "+") & skill_type == "Jump-float serve reception")/Jump_Floats,
              Standing_Floats = sum(skill_type == "Topspin serve reception"),
              SF_pct = sum(evaluation_code %in% c("#", "+") & skill_type == "Topspin serve reception")/Standing_Floats,
              R1_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 1))/sum(receive_rotation == 1),
              R2_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 2))/sum(receive_rotation == 2),
              R3_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 3))/sum(receive_rotation == 3),
              R4_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 4))/sum(receive_rotation == 4),
              R5_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 5))/sum(receive_rotation == 5),
              R6_pct = (sum(evaluation_code %in% c("#", "+") & receive_rotation == 6))/sum(receive_rotation == 6))
  
  return(output)
}
