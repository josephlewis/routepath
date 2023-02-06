check_cred_mass <- function(cred_mass) { 
  
  if (!is.numeric(cred_mass) || length(cred_mass) != 2) {
    stop("cred_mass object must be a numeric vector with a length of two")
  }

  if (cred_mass[1] > cred_mass[2]) {
    stop("first numeric value of the cred_mass object must be less than the second numeric value")    
  }
  
  return(cred_mass)
}
