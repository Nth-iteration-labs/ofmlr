#’ The package allows one to fit, online (or in a data stream) an multiple finite 
#' mixtures of logistic regression models. The main workhorse \code{online_log_mixture()} 
#' and its utility functions (such as \code{add_observation()} can be used to fit a model. 
#' The \code{multi_online_log_mixture} object allows one to fit multiple models in paralel
#' and compare their outcomes.
#’ @rdname ofmlr

#' Generate a mixture dataset
#'
#' This function creates a dataset with n rows, p predictors and k classes,
#' for simulation of a mixture or logisitc regression models.
#' 
#' @param n Number of observations
#' @param p Number of predictors for each individual logistic regression model
#' @param k Number of classes
#' @param beta A kXp matrix with the true regression coefficients for each mixture component
#' @param ak A vector of length k summing to 1 for the mixture probabilties
#' @keywords generate logistic mixture
#' @export
#' @examples
#' generate_mixture(100, 3, 2)
#' generate_mixture(100, 2, 1)
#' generate_mixture(10^4, 4,6)
#' @return A list containing slots \code{data} and \code{params} containing the 
#' simulated data and the true parameters respectively
generate_mixture <- function(
		n, 						# Number of observations
		p,				 		# Number of predictors per model
		k,						# Number of mixture components
		beta = matrix(runif(k*p,-2,2),nrow=k), 
		ak = generate_probability_vector(k)
	){
		
		if(!all(dim(beta)==c(k,p))){
			stop("Dimensions of Beta don't match up")
		}
		if(nrow(beta)!=length(ak)){
			stop("Component vector not of right length")
		}
		
		k <- sample(1:k, n, TRUE, ak)
		X <- matrix(c(rep(1,n),runif(n*(p-1),-5,5)), ncol=p)
		y <- rep(NA, n)
		for(i in 1:max(k)){
			y[k==i] <- rbinom(sum(k==i), 1, inv_logit(X[k==i,] %*% beta[i,]))
		}
	
		#data <- data.frame(y=y, k=k, x=X)
		data <- data.frame(y=y, x=X)
	
		obj <- list()
		obj$data <- data
		obj$params <- list()
		obj$params$beta <- beta
		obj$params$ak <- ak

		return(obj)
}

#' Genererate a probablity vector
#'
#' Utility function to generate a probalbity vector of specific lenght
#' @param k Number of components
#' @return Vector of lenght \code{k} with random entries that sum to 1
#' @export
generate_probability_vector <- function(k){
	p1 <- runif(k)
	return(p1 / sum(p1))
}

#' Compute inverse logit
#'
#' Utility function to compute an inverse logit
#' @param x Numeric vector (values between -Inf and Inf)
#' @return Numberic vector with lenght \code{lenght(x)} with entries between 0 and 1
inv_logit <- function(x){
	1/(1+exp(-x))
}




