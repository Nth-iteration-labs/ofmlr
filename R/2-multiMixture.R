# Create the class
setClass("multi_online_log_mixture",
	representation(models="list"),
	prototype(models=list())
	)

if(!isGeneric("add_model")){
	setGeneric(
		name = "add_model",
		def = function(object, model, model.list){standardGeneric("add_model")})
	}

#' Initialize model comparison 
#' 
#' Initialize an object for fitting multiple
#' mixtures of logistic regression models
#' in a stream of data. The class allows you to
#' add multiple models and summarize comparisons
#' between the models
#'
#' @param model The first model in a series of model comparisons
#' @return Object of type "multi_online_log_mixture"
#' The object contains the following slots:
#' \describe{
#'    \item{models}{A list containing each individual model (objects of class "online_log_mixture"). Can be indexed using \code{[[1]]}.}
#'  }
#' @export
#' @examples
#' M1 <- online_log_mixture(2,1,trace=10)
#' models <- multi_online_log_mixture(M1)
#' models <- add_model(models, online_log_mixture(2,2,trace=10))
#'
multi_online_log_mixture <- function(model){
	
	# Checking whether model is of right class
	models <- list()
	if(is(model, "online_log_mixture")){
		models[[1]] <- model
	} else {
		print("No model of class online_log_mixture provided")
	}
	new("multi_online_log_mixture", models=models)
}

#' Method to add an online_log_mixture model
#' 
#' Add multiple models to a model comparison object
#' using the add_model function. Note that for all models
#' the number of features (p) should be the same. 
#'
#' @param object A object of class multi_online_log_mixture
#' @param model A online_log_mixture model
#' @return An object of type multi_online_log_mixture
#' @export
#' @examples
#' M1 <- online_log_mixture(2,1,trace=10)
#' models <- multi_online_log_mixture(M1)
#' models <- add_model(models, online_log_mixture(2,2,trace=10))
#'
#' @rdname add_model-methods
#' @aliases add_model, ANY-method
setMethod(
	f="add_model",
	signature = "multi_online_log_mixture",
	definition = function(object, model){	
		if(is(model, "online_log_mixture")){
			object@models[[length(object@models)+1]] <- model
		} else {
			stop("No valid model specified")
		}
		return(object)
	})



#' Add an obseration to a model comparison.
#' 
#' The function takes as first argument an initialized model comparison, and subsequently
#' update the parameters of each model give an observation that is split into y (a 0 or 1 scalar)
#' and a vector X containing the features. Note that the lenght of X needs to match
#' the dimensions of the parameters of the current model.
#' @param object An object of type online_log_mixture
#' @param y A scalar with value 0 or 1; the dependent variable
#' @param X The feature vector of the current observation
#' @param lambda The current learn rate. If \code{<=0} than \code{lambda <- n+1000^(-.5)} where \code{n}
#' is the number of observations
#' @export
#' @examples
#' M1 <- online_log_mixture(2,1)
#' models <- multi_online_log_mixture(M1)
#' models <- add_model(models, online_log_mixture(2,2))
#' models <- add_observation(models, 1, c(2,-3))
#'
#' @return An updated object of type multi_online_log_mixture
#' @rdname add_observation-methods
#' @aliases add_observation, ANY-method
setMethod(
	f="add_observation",
	signature = "multi_online_log_mixture",
	definition = function(object, y, X, ...){

		for(i in 1:length(object@models)){
			object@models[[i]] <- add_observation(object@models[[i]], y, X, ...)
		}
		
		return(object)
	})


#' Summary method for the multi_online_log_mixture class
#'
#' Takes an object of type online_log_mixture and prints
#' the model a comparison of the different models based on
#' a number of different fit measures.
#' @param object The fitted model
#' @param each Default is FALSE; if TRUE it prints, in sequence, the 
#' summaries of each individual model in the comparison.
#' @return Nothing
#'
#' @examples
#' M1 <- online_log_mixture(2,1)
#' models <- multi_online_log_mixture(M1)
#' models <- add_model(models, online_log_mixture(2,2))
#' models <- add_observation(models, 1, c(2,-3))
#' summary(models)
#' @export
#' 
#' @rdname summary-methods
#' @aliases summary, ANY-method
setMethod(
	f="summary",
	signature = "multi_online_log_mixture",
	definition = function(object, each=FALSE, ...){
		
		if(!each){
		# Implement a table comparing the models
		cat("Printing a comparison of ",length(object@models), "finite mixtures of logistic regression models. \n\n")
		
		data <- data.frame()
		for(i in 1:length(object@models)){
			m <- object@models[[i]]
			result <- data.frame(
				"M"= paste("M",i,sep=""), 
				"k"= m@params$k, 
				"p"= ncol(m@params$beta),
				"ll"= m@descriptives$ll,
				"maxll"= m@descriptives$maxll,
				"AIC"= m@descriptives$AIC, 
				"BIC"= m@descriptives$BIC,
				"Norm"= m@descriptives$dnorm,
				"n"= m@params$n-1)
			data <- rbind(data, result)
		}
		print(data)	
		} else {
			for(i in 1:length(object@models)){
				summary(object@models[[i]])
				print("=============================")
			}	
		}	
	})



#' Plot method for the mult_online_log_mixture class
#'
#' Will create a plot of each of the models stored in 
#' the model comparison class that you can browse one by
#' one.
#' @param x An object of type multi_online_log_mixture
#' @param y NULL
#' @param params Boolean, if TRUE the trace of the parameter values will also be printed
#' @param omit Number of observations to omit from the log likelihood and l2 Norm traces
#' @param.y a vector with the min and max values of the plot of the beta parameters
#' @export
#' @examples
#' M1 <- online_log_mixture(2,1, trace=1)
#' models <- multi_online_log_mixture(M1)
#' models <- add_model(models, online_log_mixture(2,2, trace=1))
#' for(i in c(1:100)){
#'	models <- add_observation(models, rbinom(1,1,.5), rnorm(2,0,1))
#' }
#' plot(models, params=TRUE, omit=0)
#'
#' @rdname plot-methods
#' @aliases plot, ANY-method
setMethod(
	f = "plot",
	signature = c(x="multi_online_log_mixture",y="missing"),
	definition = function(x,y, params=FALSE, omit=100, param.y=c(-5,5), ...){
		
		# Plot each model and ask for keystroke
		for(i in 1:length(x@models)){	
			plot(x@models[[i]], params=params, omit=omit, param.y=param.y, ...)
			cat("Printed model ", i,". \n", sep="") 
			cat("Press [enter] to continue and print the next model.\n")
			line <- readline()
		}
	})

