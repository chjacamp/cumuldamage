library(cumuldamage)

t <- seq(0.1 ,500, length.out=1e3)
t <- sort(c(t, 1, 3))
step_stress <- stress_function(step_domain = 150, 
                               stress = c(1/350,1/276), 
                               xi = c(1/5))

const_stress <- stress_function(stress = 1/350)

par(mfrow = c(1,3))
beta <- 2.29
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
plot(t, exp(cumuldamage:::lcdf_wei(cumulative_exposure(step_stress, t), scale = 1, shape = beta)), type='l')
lines(t,exp(cumuldamage:::lcdf_wei(cumulative_exposure(const_stress, t), scale = 1, shape = beta)), type='l', col='red')




