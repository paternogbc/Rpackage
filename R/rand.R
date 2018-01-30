#' Random numbers
#'
#' Generate random sequences of numbers using normal distribution
#'
#' @param n number of observations.
#' @param mean distribution mean
#' @param sd standard deviation of the mean
#' @param style the style output ("vector", "data.frame")
#' @param ncol the number of columns if style == "data.frame"
#' @details
#' @details This functions generate random numbers based on the nomral distribution
#' through the function \code{rnorm}. It uses the 'mean' and standard deviation of the
#' mean to generate 'n' random numbers. The user can choose to generate multiple
#' vectors by setting style == "data.frame".
#'
#' @return The function \code{rand} returns a a vector or a data.frame with
#' multiple random numbers.
#' components:
#' @return \code{vector}: A numeric vector with n observations
#' @return \code{data.frame}: A data.frame with n rows and ncol columns. Each
#' colulm corresponds to one vector with n observations.
#' @author Your Full Name
#' @seealso \code{\link[stats]{rnorm}}
#' @references Your refrence goes here.
#' @examples
#'
#' @export
#'

rand <- function(n, mean = 0, sd = 1, style = c("vector","data.frame"),
                 ncol = NULL) {
  # error checking:
  style <- match.arg(style)

  if(!inherits(n, "numeric")) stop("n must be class 'numeric'")
  if(!inherits(mean, "numeric")) stop("mean must be class 'numeric'")
  if(!inherits(sd, "numeric")) stop("sd must be class 'numeric'")

  ### START:
  if(style == "vector"){
    if(!is.null(ncol))
      warning("'ncol' can only be used with style = 'data.frame'. Argument not used.")
    m <- rnorm(n = n, mean = mean, sd = sd)
  }
  c <- 1
  if(style == "data.frame"){
    if(is.null(ncol)) stop("'ncol' argument was not defined.")
    m <- matrix(nrow = n, ncol = ncol, dimnames = list(1:n, 1:ncol))
    ### loop:
    for (i in 1:ncol){
      v <- rnorm(n = n, mean = mean, sd = sd)
      m[,i] <- v
    }
  }
  return(m)
}

