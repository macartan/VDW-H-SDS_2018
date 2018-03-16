
#' Cluster robust standard errors
#'
#' Take a model and cluster variable and returns summary output
#' @param model A model eg from lm
#' @param cluster A variable
#' @keywords Cluster, Robust
#' @export
#' @examples
#'  cluster_robust(lm(time~temp, data = beaver1), beaver1$day)
#'
cluster_robust <- function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  not.miss<- !is.na(predict(model))
  if(length(not.miss)!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster[not.miss]))
  N <- length(cluster[not.miss])
  K <- model$rank
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum, na.rm=TRUE));
  vcovCL <- dfc * sandwich(model, meat = crossprod(uj)/N)
  out <- list()
  out[[1]] <- coeftest(model, vcovCL)
  out[[2]] <- N
  return(out)
}

#' Runs lm and returns cluster robust standard errors
#'
#' Take a model and cluster variable and returns summary output
#' @param formula An lm formula of the form Y~X
#' @param data A dataframe
#' @param weights weights used in lm
#' @keywords Cluster, Robust
#' @export
#' @examples
#'  lm_cluster_robust("time~temp", beaver1, "day")
#'

lm_cluster_robust <- function(formula, data, cluster_name,  weights = NULL){
 if(is.null(weights))   model <- lm(formula = as.formula(formula), data = data, na.action="na.exclude")
 if(!is.null(weights))  model <- lm(formula = as.formula(formula), weights = weights, data = data, na.action="na.exclude")
 cluster_robust(model, data[cluster_name][[1]])
}



#' Generates nice output after cluster robust
#'
#' Take a model and cluster variable and returns summary output
#' @param X a list output from lm_cluster_robust
#' @param coefrows rows of coefficients to  grab
#' @param stars add stars
#' @param alpha a vector of alpha levels for stars (descending order)
#' @param signs a symbol to be added for every alpha level satisfied
#' @keywords Cluster, Robust
#' @export
#' @examples
#'  output_function(lm_cluster_robust("time~temp", beaver1, "day"), coefrows = 2)
#'
output_function <- function(X, stars = TRUE,
                            alpha = c(0.1, 0.05, 0.01),
                            signs = "*",
                            coefrows = c(2:4),
                            round = 3
                            ){
  ncoefs <- length(coefrows)
  out <- names <- matrix(NA, 2*ncoefs+1)

  out[2*(1:ncoefs)-1] <-  round(X[[1]][coefrows,1],3)
  out[2*(1:ncoefs)] <-  paste("(", round(X[[1]][coefrows,2],3), ")", sep ="")
  out[2*ncoefs+1] <- X[[2]]

  if(stars) {
    for(j in 1: length(alpha)){
    out[2*(1:ncoefs)-1][X[[1]][coefrows,4] <= alpha[j]] <-
    paste(out[2*(1:ncoefs)-1][X[[1]][coefrows,4] <= alpha[j]], signs, sep ="")
    }}

  names[2*(1:ncoefs)-1] <- rownames(X[[1]])[coefrows]
  names[2*(1:ncoefs)] <- paste("sd_", rownames(X[[1]])[coefrows], sep = "")
  names[2*ncoefs+1] <- "N"
  rownames(out) <- names

  out
}

# END #