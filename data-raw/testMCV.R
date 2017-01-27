Q.matern <- function(N, range=1, smoothness=.5, phi=1, distance=NULL){
    if(is.null(distance)){
        distance <- rep(1, N-1)
    }
    if(is.null(dim(distance))){
        M <- diag(N) * 0
        for(i in 1:(N-2)){
            remove <- -1 * 1:i
            distance_ <- sapply(1:(N-i), function(x) sum(distance[x:(x+i-1)]))
            diag(M[remove,]) <- distance_
        }
        M[N, 1] <- sum(distance)
        M[upper.tri(M)] <- t(M)[upper.tri(t(M))]
        distance <- M
    }
    solve(fields::Matern(M, range=range, phi=phi, nu=smoothness))
}

d<- seq( 0,200,,200)
y<- Matern( d, range=1.5, smoothness=10, phi=1)
plot( d,y, type="l")

N <- 200
distance <- rep()
#distance <- c(1, .9, .8, .7)
sigma <- 1
rho <- 1.5
nu <- 10
M <- 10

Q <- Q.matern(N, phi=sigma, range=rho, smoothness=nu)
plot(d, solve(Q)[1,], type="l")
#Q <- Q.AR1(N, sigma, rho)
#Q <- diag(N)
obs <- sim.AR(M, Q)

# subtract off the first time point to more easily observe correlation
obs_adj <- obs - obs[,1]
# move objects to a data frame
ar1_df <- data.frame(obs=c(t(obs_adj)), realization=rep(1:M, each=N),
                     time=rep(1:N, M))
# plot each realization
ggplot(data=ar1_df, aes(time, obs, group=realization, color=realization)) +
    geom_line()

