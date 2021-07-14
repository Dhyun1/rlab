## 2.7,2.16,3.6,3.17,4.7,5.2,5.6
y<-c(19,31,1.3,1.5,43,14,14)
wilcox.test(y,conf.int=T,conf.level = 0.90)

##--------------------------------------------------

a<-c(14.6,15.8,16.4,14.6,14.9,14.3,14.7,17.2,16.8,16.1)
b<-c(15.5,17.9,15.5,16.7,17.6,16.8,16.7,16.8,17.2,18.0)
wilcox.test(a,b,alternative="greater")

##--------------------------------------------------

a1<-c(9,10,4,19,13,12,8,0,13,6,12,5,7)
b1<-c(6,7,3,19,4,12,2,0,6,7,5,0,7)
ansari.test(a1-median(a1), b1-median(b1), alternative = "two.sided")

##--------------------------------------------------

a2<-c(82,74,87,86,75)
b2<-c(88,77,91,88,94,93,83,94)
ks.test(a2,b2)

##--------------------------------------------------

##exp alpha control
set.seed(0404)
N = 5000 ; n = 100 ; c.w = c.s = 0 ; alpha= 0.05
for(i in 1:N){
  x = rexp(n, rate = 1)
  y = rexp(n, rate = 1)
  if(ansari.test(x, y, alternative = "two.sided")$p.value  < alpha)
    c.w = c.w + 1
  if(var.test(x, y, ratio = 1, alternative = "two.sided")$p.value < alpha)
    c.s = c.s + 1
}
c.s/N ; c.w/N

##t alpha control
set.seed(0404)
N = 5000 ; n = 100 ; c.w = c.s = 0 ; alpha= 0.05
for(i in 1:N){
  x = rt(n,5)
  y = rt(n,5)
  if(ansari.test(x, y, alternative = "two.sided")$p.value  < alpha)
    c.w = c.w + 1
  if(var.test(x, y, ratio = 1, alternative = "two.sided")$p.value < alpha)
    c.s = c.s + 1
}
c.s/N ; c.w/N

##normal alpha control
set.seed(0404)
N = 5000 ; n = 100 ; c.w = c.s = 0 ; alpha= 0.05
for(i in 1:N){
  x = rnorm(n,0,1)
  y = rnorm(n,0,1)
  if(ansari.test(x, y, alternative = "two.sided")$p.value  < alpha)
    c.w = c.w + 1
  if(var.test(x, y, ratio = 1, alternative = "two.sided")$p.value < alpha)
    c.s = c.s + 1
}
c.s/N ; c.w/N

##normal power comparison
set.seed(0404)
N = 5000 ; n = 100 ; c.w = c.s = 0 ; alpha= 0.05
for(i in 1:N){
  x = rnorm(n,0,1)
  y = rnorm(n,0,1.1)
  if(ansari.test(x, y, alternative = "two.sided")$p.value  < alpha)
    c.w = c.w + 1
  if(var.test(x, y, ratio = 1, alternative = "two.sided")$p.value < alpha)
    c.s = c.s + 1
}
c.s/N ; c.w/N

##--------------------------------------------------

x=c(6.67,6.71, 6.69, 6.74, 6.65, 6.72)
y=c(6.54,6.77, 6.43, 6.82, 6.74, 6.79)
ansari.test(x, y, alternative = "two.sided")
nx=length(x)
ny=length(y)
(S2x=sum((x-mean(x))^2)/(nx-1))
(S2y=sum((y-mean(y))^2)/(ny-1))
(F0=S2x/S2y)
pf(F0,nx-1,ny-1)
