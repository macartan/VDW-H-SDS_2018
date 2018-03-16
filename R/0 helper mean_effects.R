#' Calculate Mean Effects Index
#'
#' Thanks to Tara Slough and egap for much of this coding
#' @param Z Binary Treatment
#' @param outcome_mat Matrix of outcome vars: should all  be signed in same direction
#' @param reorient Vector indicating rows to reorient
#' @keywords multiple comparisons
#' @export
#' @examples
#' Z <- rep(0:1,3)
#' Y <- data.frame(1:6, 2:7)
#' calculate_mean_effects_index(Z,Y)
#' calculate_mean_effects_index(Z,Y, reorient = c(FALSE, TRUE))
#' calculate_mean_effects_index(Z,Y, reorient = c(FALSE, TRUE), double_normalization=FALSE)
#' Y[1,1] <- NA
#' calculate_mean_effects_index(Z,Y, greedy = FALSE)
#' calculate_mean_effects_index(Z,Y, greedy = TRUE)
#' calculate_mean_effects_index(Z,Y, impute = TRUE)
#' calculate_mean_effects_index(Z,Y, impute = TRUE, greedy = TRUE)
calculate_mean_effects_index <- function(Z,                # Treatment
                                         outcome_mat, 
                                         reorient = rep(FALSE, ncol(outcome_mat)), 
                                         greedy = TRUE,
                                         impute = FALSE, 
                                         seed = NULL,
                                         double_normalization = TRUE
                                         ){
  
  if(!is.null(seed)) set.seed(seed)
  if(length(Z) != nrow(outcome_mat)) stop("Error: Treatment assignment, outcome matrix require same n!")

  missing <- is.na(outcome_mat)
  if(impute == TRUE){
    R <- 1 * missing
    means_for_imputation <- rbind(apply(outcome_mat[Z==0,], MAR = 2, FUN = mean, na.rm = TRUE),
                                  apply(outcome_mat[Z==1,], MAR = 2, FUN = mean, na.rm = TRUE))
    to_impute <- R * means_for_imputation[Z+1,]
    outcome_mat[is.na(outcome_mat)] <- 0
    outcome_mat <- outcome_mat + to_impute
  }

  outcome_mat[, reorient] <- -outcome_mat[, reorient] 

  c_mean <- apply(X = outcome_mat[Z==0,], MARGIN = 2, FUN = mean, na.rm = T)
  c_sd <- apply(X = outcome_mat[Z==0,], MARGIN = 2, FUN = sd, na.rm = T)
  z_score <- t(t(sweep(outcome_mat, 2, c_mean))/ c_sd)

  if(greedy)  index <-  apply(z_score, 1, mean, na.rm = TRUE)
  if(!greedy) index <-  apply(z_score, 1, mean)
  
  if(double_normalization) index <-  (index - mean(index[Z==0], na.rm =TRUE))/sd(index[Z==0], na.rm =TRUE)
  return(index)
}


# END #