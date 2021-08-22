fd2 = function(x1,x2){
  return(sin(3*pi*x1)*x2*(1-x2)**3)
}
x = matrix((1:100)/100, ncol = 100, nrow = 100)
y = matrix((1:100)/100, ncol = 100, nrow = 100, byrow = TRUE)

u0 = fd2(x,y)
#View(u0)

dx1 = 0.01
dx2 = 0.01
T = 1000
t = 0
dt = 0.1
D = 1


Iter = function(x){
  i <- size(x,1)
  x[2:(i-1),2:(i-1)] <- (x[1:(i-2),1:(i-2)] + x[3:i,1:(i-2)] + x[3:i,3:i] + x[1:(i-2), 3:i] - 4*x[2:(i-1),2:(i-1)])
  return(x)
}

u <- u0
X <- array(0, dim = c(size(u0), T/dt + 1))
X[,,1] <- u
j<- 1

#u = array(0, dim=c(k, k, T/dt))
#u[,,1] = u0
#fig <- plot_ly(z =~u0, type = "heatmap")
#fig <- fig %>% layout(
#  title = 'Warunek początkowy'
#)
#fig
while(t < T)
{
  u <- u + dt*D*Iter(u)
  u[1,]= -1
  u[k,] = 1
  u[,1] = u[,2]+dx
  u[,k] = u[,k-1]+dx
  t <- t + dt
  
  if(mod(t*T/dt,T) < T)
  {
    j<- j+1
    X[,,j] <- u
  }
}

fig = plot_ly(z=X[,,1000], type = "surface")
fig <- fig %>% layout(
  title = 'Wykres 7.2, t = 100, destabilizacja',
  scene = list(xaxis=list(title ="x_1"), yaxis=list(title="x_2"), zaxis = list(title="u"))
)
fig
