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

create.fdr.pow = function(list,ground.truth,lmax){
  fdr <- 0
  pow <- 0
  s <- seq(1,lmax,by = 1)
  for (k in s){
    l <- list[1:k]
    fdr <- c(fdr,compute.fdr(l,ground.truth))
    pow <- c(pow,compute.power(l,ground.truth))
    print(compute.fdr(l,ground.truth))
  }
  res <- NULL
  idx <- sort(fdr,decreasing = FALSE,index.return = TRUE)$ix
  res$fdr <- fdr[idx]
  res$pow <- pow[idx]
  return(res)
}
