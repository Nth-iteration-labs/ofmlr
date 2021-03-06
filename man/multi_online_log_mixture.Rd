% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2-multiMixture.R
\name{multi_online_log_mixture}
\alias{multi_online_log_mixture}
\title{Initialize model comparison}
\usage{
multi_online_log_mixture(model)
}
\arguments{
\item{model}{The first model in a series of model comparisons}
}
\value{
Object of type "multi_online_log_mixture"
The object contains the following slots:
\describe{
   \item{models}{A list containing each individual model (objects of class "online_log_mixture"). Can be indexed using \code{[[1]]}.}
 }
}
\description{
Initialize an object for fitting multiple
mixtures of logistic regression models
in a stream of data. The class allows you to
add multiple models and summarize comparisons
between the models
}
\examples{
M1 <- online_log_mixture(2,1,trace=10)
models <- multi_online_log_mixture(M1)
models <- add_model(models, online_log_mixture(2,2,trace=10))

}

