Kills <- function(x){
  sum(x == "Winning attack")
}

AttackStuffs <- function(x){
  sum(x == "Blocked")
}

Errors <- function(x){
  # This is always an error code
  sum(x == "Error")
}

P0 <- function(x){
  # x is character vector of attack evaluations
  sum(x == "Positive, good attack")
}

M0 <- function(x){
  sum(x == "Poor, easily dug")
}

AttackEff <- function(Attempts, Kills, Stuffs, Errors, P0 = 0, M0 = 0){
  # If P0 = 0 and M0 = 0 this is efficiency
  # If P0 and M0 are given this is efficacy
  (Kills - Stuffs - Errors + P0 - M0)/Attempts
}

PhaseAttackEff <- function(evaluation, phase_vector, phase = "Reception"){
  evaluations_to_code <- evaluation[phase_vector == phase]
  Attempts <- length(evaluations_to_code)
  Kills <- Kills(evaluations_to_code)
  Stuffs <- AttackStuffs(evaluations_to_code)
  Errors <- Errors(evaluations_to_code)
  return(AttackEff(Attempts, Kills, Stuffs, Errors))
}

BlockStuffs <- function(x){
  sum(x == "Winning block")
}

BlockPlus <- function(x){
  sum(x == "Positive, block touch")
}

BlockMinus <- function(x){
  sum(x == "Poor, opposition to replay")
}

ReceptionOverpass <- function(x){
  sum(x == "Poor, no attack")
}

Reception1 <- function(x){
  sum(x == "Negative, limited attack")
}

Reception2 <- function(x){
  sum(x == "OK, no first tempo possible")
}

Reception3 <- function(x){
  sum(x == "Positive, attack")
}

ReceptionPerfect <- function(x){
  sum(x == "Perfect pass")
}

ReceptionGood <- function(x){
  Reception3(x) + ReceptionPerfect(x)
}

ReceptionGrade <- function(reception_evaluations, evaluation_grades){
  ReceptionScores <- c(
    Errors(reception_evaluations),
    ReceptionOverpass(reception_evaluations),
    Reception1(reception_evaluations),
    Reception2(reception_evaluations),
    Reception3(reception_evaluations),
    ReceptionPerfect(reception_evaluations)
  )
  return(sum(ReceptionScores*evaluation_grades))
}

ReceptionRotationScore <- function(reception_evaluations, rotation_vector, rotation){
  evaluations_to_code <- reception_evaluations[rotation_vector == rotation]
  return(ReceptionGood(evaluations_to_code)/length(evaluations_to_code))
}
