a = linspace(0,1,100)

dt = 0.1
T = 20
t = 0
Du = 0.001
Dv = 1
i = 2
dx = 1/(length(a))
A = 1
B = 3

u0 <- rep(A, length(a))
v0 <- rep(B/A, length(a))

u0[40:60] <- A + 0.01

u = matrix(0, ncol = length(u0), nrow = T/dt)
v = matrix(0, ncol = length(u0), nrow = T/dt)

u[1,] = u0
v[1,] = v0

DPu = function(u){
  k = length(u)
  w = u[1:(k-2)] + u[3:k] - 2*u[2:(k-1)]
  u[2:(k-1)] = w
  return(u)
}

DPv = function(u){
  k = length(u)
  w = u[1:(k-2)] + u[3:k] - 2*u[2:(k-1)]
  u[2:(k-1)] = w
  return(u)
}

while(t<T-dt)
{
  k=length(u[1,])
  u[i,] <- u[(i-1),] + dt*(Du*DPu(u[(i-1),]) + (A - (B+1)*u[i-1,] + (u[i-1,]**2) *v[i-1,]))
  v[i,] <- v[(i-1),] + dt*(Dv*DPv(v[(i-1),]) + (B*u[i-1,] - (u[i-1,]**2) *v[i-1,]))
  
  u[i,1] = u[i,2]
  v[i,1] = v[i,2]
  
  u[i,k] = u[i,k-1]
  v[i,k] = v[i,k-1]
  
  i = i + 1
  t = t + dt
}
fig = plot_ly(z=~u, type = 'surface')
fig <- fig %>% layout(
  title = "u(x,t), A = 1, B = 3",
  scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "t"),
    zaxis = list(title = "u(x,t)")
  ))

fig2 = plot_ly(z=~v, type = 'surface')
fig2 <- fig2 %>% layout(
  title = "v(x,t), A = 1, B =3",
  scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "t"),
    zaxis = list(title = "v(x,t)")))

fig

fig2
