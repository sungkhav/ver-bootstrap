#clear workspace
rm(list=ls(all=TRUE))
set.seed(5011)

#note that this will vary depending on where you put the data file
verizon_dat<-read.csv('Verizon.csv',header=T)

#verizons own versus others customers
own_customers<-subset(verizon_dat,Group=="ILEC")
other_customers<-subset(verizon_dat,Group=="CLEC")

#let's overlay two smoothed histograms to see what the sample distributions look like.
d1<-density(own_customers$Time,from=-20, to=200)
d2<-density(other_customers$Time,from=-20, to=200)
plot(d1,col='red',lwd=3,main='Service Wait Times')
#add second density curve
lines(d2,col='blue',lwd=3)
legend('topright',c('Verizon','Victims'),inset=.05,col=c('red','blue'),lwd=3)


readline(prompt="Press [enter] to continue")

qqnorm(own_customers$Time)

print('Lets look at using sample function()')
readline(prompt="Press [enter] to continue")

sample_size<-30
replications<-5000
my_samples<-numeric()
for (i in 1:replications){
  #sampling with replacement
  my_sample<-sample(own_customers$Time,size=sample_size,replace=T)
  my_samples<-c(my_samples,mean(my_sample))
    
}

hist(my_samples,breaks=100,freq=F)


print('Lets get a confidence interval for the difference in mean wait time')
readline(prompt="Press [enter] to continue")

replications<-1000
my_diff_samples<-numeric()
for (i in 1:replications){
  #sampling with replacement
  sample_own<-sample(own_customers$Time,replace=T)
  sample_other<-sample(other_customers$Time,replace=T)
  #take difference and store
  my_diff_samples<-c(my_diff_samples,mean(sample_own)-mean(sample_other))
}

hist(my_diff_samples,breaks=100,freq=F)


print('Lets get p-value under the null')
readline(prompt="Press [enter] to continue")

replications<-5000
my_perm_samples<-numeric()
for (i in 1:replications){
  #sampling without replacement for permuation...we shuffle labels
  #we'll get random pointers to construct a large group
  indices_group1<-sample(1:length(verizon_dat$Time),
                                size=length(own_customers$Time),replace=F)
  group1<-verizon_dat$Time[indices_group1]
  #ok so all times other than those contained in group1
  group2<-verizon_dat$Time[-indices_group1]
  #take difference and store
  my_perm_samples<-c(my_perm_samples,mean(group1)-mean(group2))
}

hist(my_perm_samples,breaks=100,freq=F)
observed<-mean(own_customers$Time)-mean(other_customers$Time)
abline(v=-abs(observed),col='blue',lwd=3)
#abline(v=abs(observed),col='blue',lwd=3)
#so the one tailed p-value
p_value<-sum(my_perm_samples<=observed)/replications
print('The permutation p-value')
print(p_value)