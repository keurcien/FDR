compute.fdr = function(list,ground.truth){
  if (length(list) == 0){
    x <- 0
  } else {
    x <- sum(!(list %in% ground.truth))/length(list)
  }
  return(x)
}

compute.power = function(list,ground.truth){
  if (length(ground.truth) == 0){
    warning("The list of true positives is empty.")
  } else {
    x <- sum(list %in% ground.truth)/length(ground.truth)
  }
  return(x)
}
