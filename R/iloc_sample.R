#' iloc_sample
#'
#' iloc_sample takes in a matrix or dataframe. It calculates the element wise proportion, and then returns the table based on a different total.
#'
#' @param x An iloc_demog table.
#' @param n the sample to adjust based on. tibble
#' @importFrom dplyr as_tibble
#' @export iloc_sample
#' @return A tibble

iloc_sample = function(x, n = NULL, p = NULL) {
  if (!any(c("data.frame", "matrix", "tibble") %in% class(x))) {
    stop("x must be one of: 'data.frame', 'matrix', or 'tibble'.",
         call. = F)
  }

  if (is.null(n) & is.null(p)) {
    stop("either n, or p must be supplied")
  }


  il_names = as.character(unlist(x[, 1]))
  m = x[, -c(1, length(x))]
  total = sum(m)
  if (is.null(n)) {
    n = round(total * p , 0)
  }
  p = m / total
  result = round(p * n, 0)

  result$total = rowSums(result)
  result$IL_name = il_names
  result = result[, c(ncol(result), 1:(ncol(result) - 1))]
  as_tibble(result)
}
