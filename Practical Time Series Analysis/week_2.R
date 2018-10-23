require(astsa)
help("astsa")
help(jj)

plot(jj, type ='o', main='Johnson&Johnson quaterly earnings per share', ylab='Earnings', xlab='Years')

plot(flu, type ='o', main='Johnson&Johnson quaterly earnings per share', ylab='Earnings', xlab='Years')

plot(globtemp, type ='o', main='Johnson&Johnson quaterly earnings per share', ylab='Earnings', xlab='Years')

plot(star, type ='o', main='Johnson&Johnson quaterly earnings per share', ylab='Earnings', xlab='Years')

#create a seed
randomProcess = ts(rnorm(100))
print(randomProcess)

#compute autocovariance
(acf(randomProcess, type='covariance'))


acf(randomProcess, main='a Title')
(acf(randomProcess, main='a Title'))

data = rnorm(100,0,1)
acf(2+3*x + data, main="ACF: Noise or Signal?")

x=NULL
x[1]=0
for(i in 2:1000){
  x[i] = x[i-1]+rnorm(1)
}
plot(x)

randomWalk=ts(x)
plot(randomWalk, main='Random walk')
acf(randomWalk)
plot(diff(randomWalk))
acf(diff(randomWalk))
