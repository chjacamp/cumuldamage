library(data.table)
library(GGally)

get_part_desc <- function(part_number) {
  claims[part%in%part_number, unique(description)]
}

claims <- readRDS('data/all_claims.RDS')
setDT(claims)

claims <- claims[usage_qty<=500]
claims[,description:=description[1], part]

claims_wide <- dcast(claims, id~part, value.var = 'usage_qty', fill=as.numeric(NA), fun.aggregate = min)

most_claimed_parts <- claims[,.N,part][order(-N)]

top_claims_wide <- dcast(claims[most_claimed_parts[1:5], on = 'part'], id~part, value.var = 'usage_qty', fill=as.numeric(NA), fun.aggregate = min)

ggpairs(top_claims_wide[,-c('id')], aes(alpha=.5))

rug_dependence_plot <- function(data, part, rug_part, region, neighborhood, shock=FALSE,
                                sample=100, sub=TRUE) {
  subset_data <- data[abs(get(rug_part)-region)<neighborhood,mget(c(part,rug_part))]
  if(shock==FALSE) subset_data <- subset_data[get(rug_part)!=get(part)]
  if(sub==TRUE) subtitle <- paste0(get_part_desc(part), ' by ', get_part_desc(rug_part))
  else subtitle <- NULL
  setorderv(subset_data, part)
  part_cdf <- ecdf(subset_data[,get(part)])
  plot(part_cdf, sub = subtitle, main = NULL)
  rug(sample(subset_data[,get(rug_part)], size = 100, replace = TRUE))
  for(i in subset_data[!is.na(get(rug_part))&get(rug_part) == get(part), which=TRUE]) 
    lines(x=c(subset_data[i,..rug_part], subset_data[i,..part]), 
          y=c(0, part_cdf(subset_data[i,..part])), col='red')
}

par(mfrow=c(3,3))
sapply(seq(50, 500, length.out = 9), function(x) {
  rug_dependence_plot(top_claims_wide, '14', '24', x, 25, sub=FALSE, shock=TRUE)
})

get_part_desc(names(top_claims_wide)[-1])

plot(ecdf(top_claims_wide$`14`))
rug(sample(top_claims_wide$`24`, 1000))

plot(ecdf(top_claims_wide$`24`))
rug(sample(top_claims_wide$`14`, 1000))

library(muhaz)

ok <- muhaz(na.omit(top_claims_wide$`20`))
plot(ok)

scale_pwei <- scales::trans_new(
  'scale_pwei', 
  transform = function(p) log(-log(1-p)),
  inverse = function(t_p) 1-1/exp(exp(t_p)), 
  domain = c(0.001,.999)
)

library(ggplot2)

rand_w <- sort(rweibull(1e4, 2, 50))

dt <- data.table(
  t = rand_w,
  F_p = cumsum(rand_w/sum(rand_w))
)
eps <- 1e-05
dt[F_p==1, F_p:=1-eps]

options(scipen=99, digits=2)
ggplot(dt, aes(x=t, y = F_p)) + geom_line() + 
  scale_y_continuous(trans=scale_pwei, breaks = c(.0001, .001, .01, .02, .05, 1:10/10)) + 
  scale_x_continuous(trans='log', breaks = c(1, 2, 5, 20, 50, 200, 500))



part_24 <- sort(na.omit(claims_wide$`14`))
dt <- data.table(
  t = part_24,
  F_p = cumsum(part_24/sum(part_24))
)
eps <- .001

dt <- dt[F_p %between% c(.001, .999)]

options(scipen=99, digits=2)
x_val <- 't'
ggplot(dt, aes(x=get(x_val), y = F_p)) + geom_line() + 
  scale_y_continuous(trans=scale_pwei, breaks = c(.0001, .001, .01, .02, .05, 1:10/10)) + 
  scale_x_continuous(trans='log', breaks = c(1, 2, 5, 20, 50, 200, 500))


weibull_dependence_plot <- function(data, part, rug_part, region, neighborhood, shock=FALSE,
                                sample=100, sub=TRUE) {
  subset_data <- data[abs(get(rug_part)-region)<neighborhood,mget(c(part,rug_part))]
  if(shock==FALSE) subset_data <- subset_data[get(rug_part)!=get(part)]
  if(sub==TRUE) subtitle <- paste0(get_part_desc(part), ' by ', get_part_desc(rug_part))
  else subtitle <- NULL
  setorderv(subset_data, part)
  subset_data <- subset_data[order(get(part))]
  part_cdf <- ecdf(subset_data[,get(part)])
  subset_data[,cdf:=part_cdf(get(part))]
  ggplot(subset_data, aes(x=get(part), y=cdf)) + 
    geom_point() + 
    geom_rug(aes(x=get(rug_part), y =NULL)) + 
    scale_y_continuous(trans=scale_pwei, breaks = c(.0001, .001, .01, .02, .05, 1:10/10)) + 
    scale_x_continuous(trans='log', breaks = c(1, 2, 5, 20, 50, 200, 500))
  
}


weibull_dependence_plot(claims_wide, '14', '24', 400, 20, shock=TRUE)
weibull_dependence_plot(claims_wide, '14', '24', 300, 20, shock=TRUE)
weibull_dependence_plot(claims_wide, '14', '24', 200, 20, shock=TRUE)

weibull_dependence_plot(claims_wide, '24', '14', 200, 1000, shock=TRUE)
