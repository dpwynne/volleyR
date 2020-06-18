Kills <- function(x){
  sum(x == "Winning attack")
}

Stuffs <- function(x){
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

AttackEff <- function(Kills, Stuffs, Errors, Attempts, P0 = 0, M0 = 0){
  # If P0 = 0 and M0 = 0 this is efficiency
  # If P0 and M0 are given this is efficacy
  (Kills - Stuffs - Errors + P0 - M0)/Attempts
}
