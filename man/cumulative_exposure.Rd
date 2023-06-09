% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cumexp_functions.R
\name{cumulative_exposure}
\alias{cumulative_exposure}
\title{Cumulative exposure function}
\usage{
cumulative_exposure(stress_function, t)
}
\arguments{
\item{stress_function}{A \code{\link[=stress_function]{stress_function()}}}

\item{t}{the endpoint for the integration}
}
\description{
\loadmathjax
}
\details{
The cumulative exposure function takes a \code{stress_function} given by \code{\link[=stress_function]{stress_function()}} and integrates it up to time \code{t}.
The integration
is performed with respect to the atomic masses given by \code{xi} in the \code{stress_function} argument, and the against
the inverse stresses given by the \code{stress} argument.

Models are of the form...
\mjdeqn{ 3 = x^3}{ 3 = x^3}

where their stochastic motivation is given in \insertCite{singpurwalla1995survival}{cumuldamage}.
}
\examples{

t <- seq(0.1 ,500, length.out=1e3)
t <- sort(c(t, 1, 3))
step_stress <- stress_function(step_domain = 250, 
                               stress = c(0.002813908,0.003620785), 
                               xi = c(0.192237127))

const_stress <- stress_function(stress = 0.002813908)

par(mfrow = c(1,3))
beta <- 2.290099966
plot(t, 
     step_stress$stress_fnx(t), 
     type='l') 

lines(t, 
      const_stress$stress_fnx(t), 
      type='l', col='red') 

rug(step_stress$breaks[-length(step_stress$breaks)], col = 'blue')
rug(step_stress$atom, col = 'red', ticksize = -.05)
plot(t, cumulative_exposure(step_stress, t), type='l')
lines(t, cumulative_exposure(const_stress, t), type='l', col='red')
plot(t, exp(lcdf_wei(cumulative_exposure(step_stress, t), scale = 1, shape = beta)), type='l')
lines(t,exp(lcdf_wei(cumulative_exposure(const_stress, t), scale = 1, shape = beta)), type='l', col='red')




}
\references{
\insertAllCited{}
}
