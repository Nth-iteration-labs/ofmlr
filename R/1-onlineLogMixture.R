# Class definition itself; not documented
setClass("online_log_mixture",
	representation(
		params = "list", 
		descriptives = "list", 
		trace = "list"
		),
	prototype(
		params = list(),
		descriptives = list(),
		trace = list()
		)
	)
	
#' Create an S4 object to fit the online logistic mixture model
#'
#' This function allows you to initialize an online logistic regression
#' model. After initialization using the model description (in terms of 
#' the number of parameters of each logistic regression model and the number
#' of mixture components) one can use the \code{add.observation(object, y, X, ...)}
#' method to add observations and call the \code{summary()} and \code{plot()}
#' methods.
#'
#' @param p Number of predictors for each logistic regression model
#' @param k Number of mixture components
#' @param beta A kXp matrix with the true regression coefficients for each mixture component
#' @param ak A vector of length k summing to 1 for the mixture probabilties
#' @param ll.window The length of the (lagged) mean log-likelihood window
#' @param trace Whether or not the parameter estimates and the lagged mean log likelihood should be traced
#' can be set to an integer x > 0 and it will store a snapshot every x-th datapoint.
#' WARINING: if \code{trace=1} the object grows large very quickly
#' @export
#' @return An object of class "online_log_mixture"
#' The object contains the following slots:
#' \describe{
#'    \item{params}{A list containing all the parameters of the model. Contains the objects \code{beta}, \code{Sak}, \code{n}}
#'    \item{descriptives}{A list containing the (lagged) mean log-likelihood, the max log-likelihood, and the AIC and BIC approximations}
#'    \item{trace}{A list containing the trace of each of the parameters}
#'  }
#' @examples
#' online_log_mixture(3,2)
#' online_log_mixture(2,4, ak=c(.1,.2,.3,.4), trace=100)
online_log_mixture <- function(
		p, 
		k, 
		beta = matrix(runif(k*p,-2,2),nrow=k), 
		ak = generate_probability_vector(k), 
		ll.window = 500, 
		trace = FALSE
	){
		# Some checks
		if(!all(dim(beta)==c(k,p))){
			stop("Dimensions of Beta don't match up")
		}
		if(nrow(beta)!=length(ak)){
			stop("Component vector not of right length")
		}
	
		# Create the lists
		params <- descriptives <- tracelist <- list()
		params$beta <- beta
		params$Sak <- ak
		params$k <- k
		params$n <- 1
		
		descriptives$window <- ll.window	#	Size of the window
		descriptives$ll <- 0				#	Lagged log likelihood
		descriptives$maxll <- 0				# 	Lagged Max log likelihood
		descriptives$AIC <- 0				# 	AIC based on lag log likelihood
		descriptives$BIC <- 0				# 	BIC based on lag log likelihood
		descriptives$norm <- 0				# 	Current norm of parameters
		descriptives$dnorm <- 0				# 	Delta in total norm of parameters

		tracelist$trace <- FALSE
		if(trace){
			tracelist$trace <- TRUE
			tracelist$each <- trace
			tracelist$ak <- list(ak)
			tracelist$beta <- list(beta)
			tracelist$descriptives <- list(descriptives)
		}
		
		# Instantiate new object and return
		new("online_log_mixture", params = params, descriptives = descriptives, trace = tracelist)
}	


#' Add an observation
#'	
#' Generic for adding observations.
#' @name add_observation
#' @rdname add_observation-methods
#' @exportMethod add_observation
setGeneric(
	name = "add_observation",
	def = function(object, y, X, ...){standardGeneric("add_observation")})


#' Method to add an observation to an online logistic regression model
#'
#' The function takes as first argument an initialize model, and subsequently
#' update the parameters give an observation that is split into y (a 0 or 1 scalar)
#' and a vector X containing the features. Note that the lenght of X needs to match
#' the dimensions of the parameters of the current model.
#' @param object An object of type online_log_mixture
#' @param y A scalar with value 0 or 1; the dependent variable
#' @param X The feature vector of the current observation
#' @param lambda The current learn rate. If \code{<=0} than \code{lambda <- n+1000^(-.5)} where \code{n}
#' is the number of observations
#' @export
#' @examples
#' M1 <- online_log_mixture(2,4, trace=1)
#' M1 <- add_observation(M1, 1, c(2,-3))
#' M1
#'
#' M2 <- online_log_mixture(2,1, trace=1)
#' for(i in 1:500){
#' 		X <- runif(2,-2,2)
#' 		y <- rbinom(1, 1, inv_logit(c(2,-2)%*%X))
#' 		M2 <- add_observation(M2, y, X, .1)
#' }
#' M2
#' 
#' @return An updated object of type online_log_mixture
#' @rdname add_observation-methods
#' @aliases add_observation, ANY-method
setMethod(
	f="add_observation",
	signature = "online_log_mixture",
	definition = function(object, y, X, lambda=.1){
		
		#Check whether the length of the feature vector matches the current beta.:
		if(length(X) != ncol(object@params$beta)){ 
			stop("Error: Not the right dimension of the feature vector")
		}
		
		X <- as.numeric(X)
		
		######################################
		######################################
		# FIT THE ACTUAL MODEL
		######################################
		######################################
		# Create current probability vector
		ak <- object@params$Sak / object@params$n
		
		# E Step: Compute the weigths
		wk <- rep(NA, object@params$k)
		for(row in 1:object@params$k){
			wk[row] <- ak[row] * dbinom(y, 1, inv_logit(X%*%object@params$beta[row,]))
		}
		# Normalize:
		wk <- wk / sum(wk)
		
		# Learn rate from Stochastic Variational inference:
		if(lambda<=0){
			lambda <- (object@params$n+100)^-.5
		}
		
		# M Step:
		for(row in 1:object@params$k){		
			p <- inv_logit(X %*% object@params$beta[row,])
			object@params$beta[row,] <- object@params$beta[row,] + lambda*wk[row]*(y-p) %*% X
		}
		
		object@params$n <- object@params$n+1
		object@params$Sak <- object@params$Sak + wk
		
		######################################
		######################################
		# UPDATE DESCRIPTIVES
		######################################
		######################################
		ll <- ll_compute(y, X, object@params$beta, object@params$Sak/sum(object@params$Sak), wk)		
		object@descriptives$ll <- object@descriptives$ll + (ll$ll-object@descriptives$ll)/min(object@params$n, object@descriptives$window)
		object@descriptives$maxll <- object@descriptives$maxll + (ll$maxll-object@descriptives$maxll)/min(object@params$n, object@descriptives$window)
		object@descriptives$AIC <- 2*(object@params$k*ncol(object@params$beta)+object@params$k) - 2*object@descriptives$ll*min(object@params$n, object@descriptives$window)
		object@descriptives$BIC <- -2*object@descriptives$ll*min(object@params$n, object@descriptives$window)+(object@params$k*ncol(object@params$beta)+object@params$k)*log(min(object@params$n, object@descriptives$window))
		current_norm <- norm(object@params$beta, type="2") + norm(object@params$Sak/sum(object@params$Sak), type="2")
		object@descriptives$dnorm <- object@descriptives$dnorm + (abs(object@descriptives$norm-current_norm)-object@descriptives$dnorm)/min(object@params$n, object@descriptives$window)	 
		object@descriptives$norm <- current_norm	
	
		
		######################################
		######################################
		# UPDATE TRACE
		######################################
		######################################
		
		if(object@trace$trace){
			if(object@params$n%%object@trace$each==0){
				i <- length(object@trace$ak)+1
				object@trace$ak[[i]] <- object@params$Sak/sum(object@params$Sak)
				object@trace$beta[[i]] <- object@params$beta
				object@trace$descriptives[[i]] <- object@descriptives
			}
		}
		
		######################################
		# AND RETURN
		######################################
		return(object)
	})


#' (Internal) function to compute the log likelihood of an observation
#'
#' Function takes the current model parameters, and an observation to
#' compute the log likelihood of the observation
#'
#' @param y A scalar with value 0 or 1; the dependent variable
#' @param X The feature vector of the current observation
#' @param beta A kXp matrix with the true regression coefficients for each mixture component
#' @param ak A vector of length k summing to 1 for the mixture probabilties
#' @param wk The mixture weights (posterior probablity of the data point for each component)
#' 
#' @return A list containing the log likelihood of the datapoint and
#' the max log likelihood of the datapoint
ll_compute <- function(y, X, beta, ak=1, wk=1){
	p <- inv_logit(X %*% t(beta)) 
	lik.comp <- p^y * (1-p)^(1-y)
	return(list(ll=log(sum(ak * lik.comp)), maxll=log(lik.comp[which.max(wk)])))
}


#' Summary method for the online_log_mixture class
#'
#' Takes an object of type online_log_mixture and prints
#' the model parameters and a number of descriptives
#' @param object The fitted model
#' @return Nothing
#' @export
setMethod(
	f="summary",
	signature = "online_log_mixture",
	definition = function(object, ...){
		cat("Online fit of logistic mixure \n\n")
		cat("Number of mixture components: ", object@params$k, "\n")
		cat("Number of predictors (including intercept): ", ncol(object@params$beta), "\n\n")
		cat("Estimated cluster membership probabilities: \n")
		print(object@params$Sak / sum(object@params$Sak), digits=3)
		cat("\nEstimated coefficients:\n")
		print(object@params$beta, digits=3)
		cat("\nTotal number of observations: ", object@params$n-1, "\n")
		cat("The current (streaming mean) log likelihood is ", round(object@descriptives$ll,3), "(", round(object@descriptives$maxll,3),"), AIC=",round(object@descriptives$AIC,3)," and BIC=", round(object@descriptives$BIC,3),".\n")
		cat("NOTE: the norm of the parameters has changed by ", round(object@descriptives$dnorm,3), " in the last iteration.\n\n")
	})



#' Plot method for the online_log_mixture class
#'
#' Plot an object of type online_log_mixture.
#' This will only produce a plot when \code{trace!=FALSE}
#' The plots will be of the log-likelihood of the model over
#' the number of observations and the average change in L2
#' norm of the model parameters. Also, when \code{params=TRUE}
#' plots of the parameter estimates over time will also be produced.
#' @param x The online_log_mixture object
#' @param y NULL
#' @param params Boolean, if TRUE the trace of the parameter values will also be printed
#' @param omit Number of observations to omit from the log likelihood and l2 Norm traces
#' @param.y a vector with the min and max values of the plot of the beta parameters
#' @export
#' 
#' @examples
#' M2 <- online_log_mixture(3,3, trace=1)
#' for(i in 1:10000){ 	
#' 	X <- runif(3,-2,2)
#' 	y <- rbinom(1, 1, inv_logit(c(0,-2,2)%*%X))
#' 	M2 <- add_observation(M2, y, X, 0)
#' }
#' plot(M2, params=TRUE)
#'
setMethod(
	f = "plot",
	signature = c(x="online_log_mixture",y="missing"),
	definition = function(x,y, params=FALSE, omit=100, param.y=c(-5,5), ...){

		# Some Checks
		if(!x@trace$trace){
			stop("No trace enabled for the current model. Nothing to plot")
		} 

		if(x@params$n < omit+1){
			stop("Omitting more than the number of datapoints in the stream")
		}

		# Start plotting
		par(mfrow=c(2,1), mar=c(1, 1, 1, 1) + 0.1)
		if(params){
			if(x@params$k==1){
				par(mfrow=c(3,1), mar=c(1, 4, 1, 1) + 0.1)
			} else {
				par(mfrow=c(4,1), mar=c(1, 4, 1, 1) + 0.1)
			}
		}
		omit <- omit / x@trace$each
		ll <- sapply(x@trace$descriptives, function(x){x$ll})[-c(1:omit)]
		dnorm <- sapply(x@trace$descriptives, function(x){x$dnorm})[-c(1:omit)]
		plot(ll, ylab="Log-likelihood", type="l", xlab="")
		abline(v=x@trace$window)
		plot(dnorm, ylab="L2 Norm", type="l", xlab="")
		
		if(params){
			# plot Ak
			if(!x@params$k==1){
				plot(1, type="n", xlim=c(0, length(x@trace$ak)), ylim=c(0, 1), ylab="Components", xlab="")
				for(i in 1:x@params$k){
					y <- sapply(x@trace$ak, function(x, i){x[i]}, i=i)
					lines(y, col=i)
				}
			}
			
			# plot Bj
			plot(1, type="n", xlim=c(0, length(x@trace$ak)), ylim=param.y, ylab="Parameters", xlab="")
			for(i in 1:x@params$k){
				for(j in 1:ncol(x@params$beta)){
					y <- sapply(x@trace$beta, function(x, i,j){x[i,j]}, i=i,j=j)
					lines(y, col=i)
				}					
			}
		}
	})
	