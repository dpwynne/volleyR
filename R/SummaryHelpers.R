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
