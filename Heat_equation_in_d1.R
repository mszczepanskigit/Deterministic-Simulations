a = linspace(0,1,100)

fd1 = function(x){
  return(exp(2*x)-1)
}

u0 <- fd1(a)
#u0 = rep(1,length.out=30)

dt = 0.1
T = 50
t = 0
D = 5.1
i = 2
dx = 1/100

u = matrix(0, ncol = length(u0), nrow = T/dt)

u[1,] = u0
#View(u0)
v = u0
DP = function(v){
  k = length(v)
  w = v[1:(k-2)] + v[3:k] - 2*v[2:(k-1)]
  v[2:(k-1)] = w
  return(v)
}

while(t<T)
{
  u[i,] = u[(i-1),] + dt*D*DP(u[(i-1),])
  u[i,1]=1
  u[i,length(u[1,])] = u[i,length(u[1,])-1] + dx
  i = i + 1
  t = t + dt
}

fig = plot_ly(z=~u, type = 'surface')

xlist = list(title = "x", nticks = 5)
ylist = list(title = "t", nticks = 6)
zlist = list(title = "u", nticks = 5)

fig <- fig %>% layout(
  title = 'WYKRES 4, D = 5.1, T =50',
  scene = list(xaxis = xlist, yaxis = ylist, zaxis = zlist),
  showlegend = FALSE
  )

fig
#View(u)
#length(u0)

##Zaczyna wybuchać przy wartości D ~ 5.4-5.5
