
#' Turn matrix to latex
#'
#' Function takes a matrix and converts to latex string with & separators, \\ endings, and optional rownames. Apply tablr to output.
#' @param mat A matrix of table contents
#' @param rownames Optional row names for the table -- may have multiple columns
#' @param addslash  Add a set of slashes \\\\ to the end of each row
#' @keywords Latex tables
#' @export
#' @examples
#'  mat_to_tex(matrix(11:14,2), rownames = 1:2)
#'
mat_to_tex <- function(mat, rownames = NULL, add_slashes = TRUE) {
  if(!is.null(rownames)) mat <- cbind(rownames, mat)
  x <- t(sapply(1:nrow(mat), function(j)  paste(mat[j,], collapse  = "&")))
  if(add_slashes) x <- paste(x, rep("\\\\", nrow(mat)))
  as.matrix(x, nrow(mat),1)
}

#' Turn latex latex vector to text
#'
#' User defined function for outputting tables from R into LaTeX. Thanks Jasper Cooper for this.
#' @param x A vector with rows of Tex output.
#' @keywords Latex tables
#' @export
#' @examples
#'  tablr(mat_to_tex(matrix(11:14,2), rownames = 1:2))
#'


tablr <- function(x){
  x <- paste(x,"  \n",sep="")
  cat(x)
}

# END #