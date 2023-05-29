#' @import data.table

.datatable.aware=TRUE # use data.table in R package development

data <- readRDS('all_claims.RDS')
setDT(data)
data <- data[is.na(usage_qty),status:=1]

alpha = c(50,60)
beta = 2
xi = 3

cumdam_likelihood <- function(alpha, xi, beta, data, step_column = '', shock_col = '', censoring_col='status', censoring_indicator = 1) {
  
  pairs <- data[!is.na(usage_qty)&part%in%c('14', '24'), .(usage_qty,id,part)]
  
  pairs <- dcast(pairs, id~part, fun.aggregate = min, value.var = 'usage_qty')
  
  pairs <- pairs[`14`!=Inf][`24`!=Inf]
  
  part_a <- pairs$`14`
  part_b <- pairs$`24`
  cumulative_exposure(stress_function(step_domain = part_b[1], 
                                      shock_domain = part_b[1], 
                                      alpha = pars[1:2], 
                                      xi = pars[3]), part_a[1])
  pairs[,id:=.I]
  
  lik <- function(pars=c(alpha, xi, beta)) {
    
    # pars = c(4e-5, 4e-5, 1e-4, 1.2)
    pairs[,lower_prob:=lcdf_wei(
      cumulative_exposure(
        stress_function(step_domain = `24`, 
                        shock_domain = `24`, 
                        stress = pars[1:2], 
                        xi = pars[3]), 
        `14`-sm_epsilon(-3) ), 
      scale = 1, 
      shape = pars[4]), id]
    pairs[,upper_prob:=lcdf_wei(
      cumulative_exposure(
        stress_function(step_domain = `24`, 
                        shock_domain = `24`, 
                        stress = pars[1:2], 
                        xi = pars[3]), 
        `14` +sm_epsilon(-3)), 
      scale = 1, 
      shape = pars[4]), id]
    
    
    pairs[,-sum(log_diff_exp(upper_prob, lower_prob))]
    
  }
  
  ok <- optim(c(1/250, 1/250, 1/400, 1), fn = lik, control=list(maxit = 5000, reltol = 1e-10, abstol = 1e-10))
  
  
  lik2 <- function(pars=c(alpha, xi, beta)) {
    
    # pars = c(4e-5, 4e-5, 1e-4, 1.2)
    pairs[,lower_prob:=lcdf_wei(`14`-sm_epsilon(-2), 
                                scale = 1/cumulative_exposure(
                                  stress_function(step_domain = numeric(), 
                                                  shock_domain = numeric(), 
                                                  alpha = pars[1]), 
                                  `14`), 
                                shape = pars[4]), id]
    pairs[,upper_prob:=lcdf_wei(`14`+sm_epsilon(-2), 
                                scale = 1/cumulative_exposure(
                                  stress_function(step_domain = numeric(), 
                                                  shock_domain = numeric(), 
                                                  alpha = pars[1]), 
                                  `14`), 
                                shape = pars[4]), id]
    
    
    pairs[,-sum(log_diff_exp(upper_prob, lower_prob))]
    
  }
  
  ok <- optim(c(1.3e-5, 9.9e-6, 1e-4, 1.3), fn = lik2, control=list(maxit = 5000, reltol = 1e-14, abstol = 1e-14))
}
