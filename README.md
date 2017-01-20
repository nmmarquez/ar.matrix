# ar.matrix
Using sparse precision matricies and Choleski factorization simulates data that
is auto-regressive. Currently offers support for creation of precision and
variance-covariance matricies as well as simulating data from a Gaussian
Markov random field following an evenly spaced autoregressive one structure
(AR1), proper conitional autoregressive structure (pCAR), and Leroux conditional
autoregressive structure (lCAR). Addionally, the package offers a wrapper for
simulating data using a sparse precision matrix which takes advantage of
Choleski factor decoposition. This allows for faster simulations of large 
sparse precision matrix when in comparison to using something such as 
`mvtnorm::rmvnorm` to simulate data from the variance-covariance matrix.

```
library(ar.matrix)
Q <- Q.AR1(800, 1, .99) # precision matrix
Sigma <- solve(Q) # inverse of precision matrix, dense vcov matrix

# using mvtnorm::rmvnorm
system.time(mvtnorm::rmvnorm(10, sigma=Sigma))
#   user  system elapsed 
#  1.604   0.008   1.611 

# using the ar.matrix Choleski method
system.time(sim.AR(10, Q))
#   user  system elapsed 
#  0.896   0.012   0.907 
```

## Examples of simulations

In order to use the examples in the documentation it is neccessary to install
ggplot2, sp, and leaflet.

AR1 simulations are 1-dimensional evenely spaced process where each point is
correlated with the point before it and after it with some value rho. In order
to better to observe this phenomenon we can run the following code to simuate
100 realizations of an AR1 process with high point toint correlation and
subtract off the first value such that all relaizations start to diverge from
zero. See the example in help file for r.AR1.

![AR1](http://i.imgur.com/oAnDmwW.png "1D AR1 process")

The precision matricies can also be used directly to create more complicated
multi dimensional process by taking the kronecker product of two matricies.
the following code does this and produces realizations that are correlated in 
two dimensions, in this case time and age.

```
# simulate 2D ar1 process
# pairwise correlation
rho <- .95
# pairwise variance
sigma <- .5

# 2 dimensions of simulations
years <- 20
ages <- 10

# kronnecker product to get joint covariance
Q2D <- kronecker(Q.AR1(M=years, sigma, rho), Q.AR1(M=ages, sigma, rho))

# simulate the data and place it in a data frame
Q2D.df <- data.frame(obs=c(sim.AR(1, Q2D)), age=rep(1:ages, years),
                     year=rep(1:years, each=ages))

# graph results
ggplot(data=Q2D.df, aes(year, obs, group=age, color=age)) + geom_line()
```

![2D AR1](http://i.imgur.com/kz48GnJ.png "2D AR1 process")

We can also use a conditional autoregressive process to simulate correlated
data based on some critera. in this case we simulate data that is correlated in
geographic space if two spaces are touching one another.

![CAR](http://i.imgur.com/wGqeVWp.png "CAR process")
