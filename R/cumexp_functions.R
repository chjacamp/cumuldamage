#' a small number
#' a small number for use in calculating approximate function values on open intervals
sm_epsilon <- function(power=-10) 10^power

#' Vectorized integrate function in `lower`, `upper`
#' @export
integrate <- Vectorize(integrate, c('lower', 'upper'))

#' Vector-size matching
#' @export
relength <- function(vector, by) {
  if(length(vector)>1 & length(vector)!=length(by)) 
    stop(paste0(vector, 'must be the same length as ', by, ' or length 1'))
  if(length(vector)<length(by)) rep(vector, length(by))
  else vector
}

#' Design Matrix by Size
#' 
#' Construct a design matrix by giving a vector of group_sizes (avoids `cut` for simple-function construction)
#' 
#' @export

matrix.constr <- function(group_sizes) {
  I <- diag(length(group_sizes))
  matrices <- lapply(seq_along(group_sizes), function(i) rep(1, group_sizes[i]) %x% I[i,,drop=FALSE])
  Reduce(rbind,matrices)
}

#' Lebesgue Integration
#' 
#' Adds atomic masses and other measures to `integrate`
#' 
#' @param fnx The function to be measured
#' @param base_measure A named list giving the type of measure to be used, with elements specifying the domain 
#' of integration. Measures include
#'   * Lebesgue: The standard measure; defaults to Riemann integration using the base R `integrate` approximation
#'   * Atomic: A point mass; the value of the function at the point mass is added 
#'   * Simple: A measure over a specified number of breaks, expecting a right-continuous step-function over those intervals
#'   in the breaks.
#'   * \<User\>: Any function defined on the domain of integration in `from` and `to`. For instance, dwei = c(0, 60) will
#'   integrate `fnx` against the Weibull probability density function using `integrate`.
#' Note: The domain for atomic masses must be length 1 or length
#' of `from` or `to`
#' @param from,to The set over which the function should be measured (wrt the base measure)
#' @export
lebesgue_integral <- function(fnx, base_measure=list(Lebesgue = c(0, Inf)), from, to) {
  area <- numeric(length(to))
  
  from <- relength(from, to)
  to <- relength(to, from)
  
  for(meas in names(base_measure)) {
    if(meas == 'lebesgue') {
      domain <- base_measure[[meas]]
      integrals <- integrate(fnx, 
                             lower = pmax(from, domain[1]), 
                             upper = pmin(to, domain[2]))
      area <- area +  as.numeric(integrals[1,seq_along(to)])
    }
    else if(meas == 'atom') {
      domain <- base_measure[[meas]]
      for(atom in domain) {
        domain_logic <- which(atom %between% list(from, to)) 
        area[to>=atom] <- area[to>=atom] + fnx(atom)
      }
    }
    else if(meas == 'simple') {
      domain <- base_measure[[meas]]
      domain_list <- lapply(from, function(x) {
        domain <- domain[domain>x]
        c(x, domain)
      })
      
      domain_list <- lapply(to, function(x) {
        domain <- domain[domain < x]
        c(domain,x)
      })
      
      area <- area + sapply(domain_list, function(x) {
        sum(diff(x)  *  fnx(x[-length(x)]+sm_epsilon()))
      })
      
    }
    else {
      prob_meas <- tryCatch(match.fun(meas), 
                            error = function(cond) {
                              message(paste0('The measure ', meas, ' is not a function in R.'))
                            })
      integrals <- integrate(function(x) x*prob_measure(x), 
                             lower = pmax(from, domain[1]), 
                             upper = pmin(to, domain[2]))
      area <- area +  as.numeric(integrals[1,seq_along(to)])
    }
    
  }
  return(area)
  
}

#' Stress Function
#' 
#' A function giving the idealized stress a unit operates under over time.
#' 
#' @return An object of class "stress_function" which includes the function (x$fnx)
#' and any specified atoms of shock stress (x$atom = shock_domain).
#' @param mode The type of stress function, constant stress gives models equivalent to 
#' their marginal distribution; step-stress gives models with variable stresses 
#' over time.
#' @param shock Logical indicating if the model should include a `xi` parameter 
#' governinng the probability of instant failure at the shock_domain
#' @param step_domain Numeric value giving the point at which the stress should 
#' increase.
#' @param shock_domain Numeric value giving the point at which the instantaneous 
#' shock `xi` should occur; defaults to `step_domain`
#' @param stress Numeric vector giving the value of the stresses, must have 1 more
#' unit than the step_domain
#' @param xi The value of the shock stress, should be same length 1 or same length
#' as `shock_domain`
#' @export

stress_function <- function(step_domain = numeric(),
                            shock_domain = step_domain,
                            stress = numeric(length(step_domain+1)), 
                            xi = numeric()) {
  res <- list()
  xi <- relength(xi, shock_domain)
  breaks <-  sort(unique(c(0, step_domain, 1e5)))
  
  res$stress_fnx <- function(x) {
    
    x <- sort(x)
    x_cut <- cut(x, breaks, include.lowest = TRUE, ordered_result = TRUE) 
    levels(x_cut) <- stress
    stress_val <- as.numeric(as.character(x_cut))
    
    if(length(xi)==0) return(stress_val)
    
    rel_xi <- xi[which(shock_domain %in% x)]
    xi_val <- numeric(length(x))
    xi_val[x%in%shock_domain] <- rel_xi
    stress_val * I(!x%in%shock_domain) + xi_val * I(x%in%shock_domain)
    
  }
  
  res$fnx <- function(x) {
    
    x <- sort(x)
    x_cut <- cut(x, breaks, include.lowest = TRUE, ordered_result = TRUE) 
    levels(x_cut) <- 1/stress
    stress_val <- as.numeric(as.character(x_cut))
    
    if(length(xi)==0) return(stress_val)
    
    rel_xi <- 1/xi[which(shock_domain %in% x)]
    xi_val <- numeric(length(x))
    xi_val[x%in%shock_domain] <- rel_xi
    stress_val * I(!x%in%shock_domain) + xi_val * I(x%in%shock_domain)
    
  }
  

  res$breaks <- breaks
  if(length(xi)>0) res$atom <- shock_domain
  
  class(res) <-  c(class(res), 'stress_function')
  
  res 
}

#' Cumulative exposure function
#'
#' The cumulative exposure function takes a `stress_function` given by [stress_function()] and integrates it up to time `t`. 
#' The integration
#' is performed with respect to the atomic masses given by `xi` in the `stress_function` argument, and the against
#' the inverse stresses given by the `stress` argument.
#' 
#' Models are of the form... 
#' \mjdeqn{ 3 = x^3}{ 3 = x^3}
#' 
#' where their stochastic motivation is given in \insertCite{singpurwalla1995survival}{cumuldamage}.
#' 
#' @param stress_function A [stress_function()]
#' @param t the endpoint for the integration
#' @example examples/cumexp.R
#' 
#' @importFrom Rdpack reprompt
#' @import mathjaxr
#' @description \loadmathjax
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
cumulative_exposure <- function(stress_function, t) {
  if(!inherits(stress_function, 'stress_function')) stop('must use stress_function to define stress')
  
  if(!is.null(stress_function$atom)) {
    seq <- 1:(length(stress_function$breaks)-1)
    base_measure <- list(simple = stress_function$breaks, 
                         atom = stress_function$atom)
    base_measure[['atom']] <- stress_function$atom
  } else {
    seq <- 1:(length(stress_function$breaks)-1)
    base_measure <- lapply(seq, function(i) c(stress_function$breaks[[i]], stress_function$breaks[[i+1]]))
    names(base_measure) <- rep('simple', length(seq))
  }
  
  lebesgue_integral(
    fnx = function(x) 1/stress_function$fnx(x),
    base_measure = base_measure,
    from = 0,
    to = t
  )
  
}
