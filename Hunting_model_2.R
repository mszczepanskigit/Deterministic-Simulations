u0 = c(0.1, 0.5, 2.0, 3.0, 1.2,1.5,0.75,0.15) # chaos jak nie wchodzimy po n krokach na stacjonarne, dla ujemnych wybucha
dt = 0.1
T = 4
x = linspace(0,T,(T/dt+1))
t = 0

r = 2
L = 2
M = 0

u = matrix(u0,1,length(u0))


f = function(u){
  return(r*u[nrow(u),]*(1-u[nrow(u),]/L) - M)
}

while(t<T){
  un = u[nrow(u),] + dt * f(u)
  u = rbind(u,un)
  t = t + dt
}

x1 = (-r - sqrt(r^2 - 4*M*r/L))/(-2*r/L)

matplot(x, u, "l","solid",lwd = 1.5,ylim=c(-0.5,max(u0)),xlab="Czas",ylab="Liczebnoœæ populacji")
mtext(side=3, line=0.5, "rL=4M", font=3, cex=2)
#lines(x,rep(1,length(x)),lwd=2)
#lines(x,rep(x1,length(x)),lwd=2)
text(x=2.5,y=2.75,"r = 2")
text(x=3.5,y=2.75,"L = 2")
text(x=1.5,y=2.75,"M = 0")
grid()