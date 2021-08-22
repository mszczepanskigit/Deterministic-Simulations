T = 100
t = 0
dt = 0.1
Du = 0.001
Dv = 1
A = 1
B = 3
k=200

u = matrix(A, ncol = 200, nrow = 200)
v = matrix(B/A, ncol = 200, nrow = 200)

u[90:110,90:110] = A+0.01

Iter = function(x){
  i <- size(x,1)
  x[2:(i-1),2:(i-1)] <- (x[1:(i-2),1:(i-2)] + x[3:i,1:(i-2)] + x[3:i,3:i] + x[1:(i-2), 3:i] - 4*x[2:(i-1),2:(i-1)])
  return(x)
}


U <- array(0, dim = c(size(u), T/dt + 1))
U[,,1] <- u
V <- array(0, dim = c(size(v), T/dt + 1))
V[,,1] <- v
j<- 1

while(t < T)
{
  u <- u + dt*(Du*Iter(u) +(A - (B+1)*u + (u**2) *v))
  v <- v + dt*(Dv*Iter(u) +(B*u - (u**2) *v))
  
  u[1,]= u[2,]
  u[k,] = u[k-1,]
  u[,1] = u[,2]
  u[,k] = u[,k-1]
  
  v[1,]= v[2,]
  v[k,] = v[k-1,]
  v[,1] = v[,2]
  v[,k] = v[,k-1]
  t <- t + dt
  
  if(mod(t*T/dt,T) < T+1)
  {
    j<- j+1
    U[,,j] <- u
    V[,,j] <- v
  }
}

fig = plot_ly(z=U[,,1000], type = "heatmap")
fig <- fig %>% layout(
  title = "u(x,t), A = 1, B = 3, t = 100",
  scene = list(
    xaxis = list(title = "$x_1$"),
    yaxis = list(title = "$x_2$"),
    zaxis = list(title = "u(x,t)")
  ))

fig2 = plot_ly(z=V[,,1000], type = "heatmap")
fig2 <- fig2 %>% layout(
  title = "v(x,t), A = 1, B = 3, t = 100",
  scene = list(
    xaxis = list(title = "$x_1$"),
    yaxis = list(title = "$x_2$"),
    zaxis = list(title = "v(x,t)")
  ))

fig
fig2


