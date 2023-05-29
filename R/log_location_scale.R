mu_transform = function(scale) log(scale)
scale_transform = function(mu) exp(mu)
sigma_transform = function(shape) 1/shape
shape_transform = function(sigma) 1/sigma

phi_sev <- function(z) exp(z-exp(z))
Phi_sev <- function(z) 1-exp(-exp(z))
inv_phi_sev <- function(p) log(-log(1-p))

lpdf_sev <- function(x, mu, sigma) {
  z <- (x-mu)/sigma
  -log(sigma)+(z-exp(z))
}

lcdf_sev <- function(x, mu, sigma) {
  z <- (x-mu)/sigma
  log(1-exp(-exp(z)))
}

q_sev <- function(p, mu, sigma) {
  mu + inv_phi_sev(p)*sigma
}

lpdf_ls <- function(x, mu, sigma, family = c('sev', 'lev', 'logis', 'norm')) {
  family_dens <- paste0('phi_', match.arg(family))
  std_density <- do.call(family_dens, list((log(x)-mu)/sigma))
  -log(sigma)-log(x)+log(std_density)
}

lcdf_ls <- function(x, mu, sigma, family = c('sev', 'lev', 'logis', 'norm')) {
  family_cdf <- paste0('Phi_', match.arg(family))
  std_family <- do.call(family_dens, list((log(x)-mu)/sigma))
  log(std_family)
}

lccdf_ls <- function(x, mu, sigma, family = c('sev', 'lev', 'logis', 'norm')) {
  family_cdf <- paste0('Phi_', match.arg(family))
  std_family <- do.call(family_dens, list((log(x)-mu)/sigma))
  log(1-std_family)
}


lpdf_wei <- function(x, shape, scale) {
  log(shape) - log(scale) + (shape-1)*(log(x)-log(scale)) - (x/scale)^shape
}

plot(lpdf_ls(1:10, mu_transform(5), sigma_transform(2)), type='l')
plot(lpdf_wei(1:10, 2, 5), type='l') # same!


lcdf_wei <- function(x, shape, scale) {
  log(1-exp(-(x/scale)^shape))
}

lccdf_wei <- function(x, shape, scale) {
  -(x/scale)^shape
}

q_wei <- function(p, shape, scale) {
  scale*(-log(1-p))^(1/shape)
}

log_sum_exp <- function(x, y) {
  log(exp(x)+exp(y))
}

log_diff_exp <- function(x, y) {
  log(exp(x)-exp(y))
}

lpdf_wei_rl <- function(u, t_2, shape, scale) {
  lpdf_wei(u+t_2, shape, scale) - lccdf_wei(t_2, shape, scale)
}

r_wei <- function(n, shape, scale, lower=0, upper=Inf) {
  F_a <- exp(lcdf_wei(lower, shape, scale))
  F_b <- exp(lcdf_wei(upper, shape, scale))
  u <- runif(n, min=F_a, max=F_b)
  q_wei(u, shape, scale)
}

h_wei <- function(x, shape, scale) {
  exp(lpdf_wei(x, shape, scale)-lccdf_wei(x, shape, scale))
}


# examples