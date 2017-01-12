#' Fit online finite mixtures of logistic regression models
#'
#’ The ofmlr package allows one to fit, online (or in a data stream) multiple finite 
#' mixtures of logistic regression models in parallel. The main workhorse \code{online_log_mixture()} 
#' and its utility functions (such as \code{add_observation()} can be used to fit a model. 
#' The \code{multi_online_log_mixture} object allows one to fit multiple models in paralel
#' and compare their outcomes.
#'
#' @seealso Please see the more extensive documentation and examples under \code{\link{online_log_mixture()}}
#' 
#' @examples
#' M1 <- online_log_mixture(3,3, trace=1)
#' for(i in 1:10000){ 	
#' 	X <- runif(3,-2,2)
#' 	y <- rbinom(1, 1, inv_logit(c(0,-2,2)%*%X))
#' 	M1 <- add_observation(M2, y, X, 0)
#' }
#' plot(M1, params=TRUE)
#'
"_PACKAGE"
