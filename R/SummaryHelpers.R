Kills <- function(x){
  sum(x == "Winning attack", na.rm = TRUE)
}

AttackStuffs <- function(x){
  sum(x == "Blocked", na.rm = TRUE)
}

Errors <- function(x){
  # This is always an error code
  sum(x == "Error", na.rm = TRUE)
}

P0 <- function(x){
  # x is character vector of attack evaluations
  sum(x == "Positive, good attack", na.rm = TRUE)
}

M0 <- function(x){
  sum(x == "Poor, easily dug", na.rm = TRUE)
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
  sum(x == "Winning block", na.rm = TRUE)
}

BlockPlus <- function(x){
  sum(x == "Positive, block touch", na.rm = TRUE)
}

BlockMinus <- function(x){
  sum(x == "Poor, opposition to replay", na.rm = TRUE)
}

ReceptionOverpass <- function(x){
  sum(x == "Poor, no attack", na.rm = TRUE)
}

Reception1 <- function(x){
  sum(x == "Negative, limited attack", na.rm = TRUE)
}

Reception2 <- function(x){
  sum(x == "OK, no first tempo possible", na.rm = TRUE)
}

Reception3 <- function(x){
  sum(x == "Positive, attack", na.rm = TRUE)
}

ReceptionPerfect <- function(x){
  sum(x == "Perfect pass", na.rm = TRUE)
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


Aces <- function(x){
  sum(x == "Ace", na.rm = TRUE)
}

ServeOverpass <- function(x){
  sum(x == "Positive, no attack", na.rm = TRUE)
}

ServePlus <- function(x){
  sum(x == "Positive, opponent some attack", na.rm = TRUE)
}

ServeOK <- function(x){
  sum(x == "OK, no first tempo possible", na.rm = TRUE)
}

ServeMinus <- function(x){
  sum(x == "Negative, opponent free attack", na.rm = TRUE)
}
