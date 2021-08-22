u0 = c(0.1, 0.5, 2.0, 3.0, 1.0) # chaos jak nie wchodzimy po n krokach na stacjonarne, dla ujemnych wybucha
dt = 0.1
T = 4
x = linspace(0,T,(T/dt+1))
t = 0

r = 1
L = 1
k = 2

u = matrix(u0,1,length(u0))


f = function(u){
  return(r*u[nrow(u),]*(1-u[nrow(u),]/L) - k*u[nrow(u),])
}

while(t<T){
  un = u[nrow(u),] + dt * f(u)
  u = rbind(u,un)
  t = t + dt
}
matplot(x, u, "l","solid",lwd = 1.5,ylim=c(0,max(u0)),xlab="Czas",ylab="Liczebnoœæ populacji")
mtext(side=3, line=0.5, "r < k", font=3, cex=2)
lines(x,rep(0,length(x)),lwd=2)
lines(x,rep(L*(1-k/r),length(x)),lwd=2)
text(x=2.5,y=2.75,"r = 1")
text(x=3.5,y=2.75,"L = 1")
text(x=1.5,y=2.75,"k = 2")
grid()