#function for any size confidence interval
conf.int=function(p=point.estimate,c=confidence.level,s=standard.error)
  {
  CI.l=p-qnorm(1-(1-c)/2)*s
  CI.u=p+qnorm(1-(1-c)/2)*s
  list(lower=CI.l,upper=CI.u)
}
#example point estimate=6, confidence level =.95, standard error= 1.5
conf.int(6,.95,1.5)

#function for t confidence interval
conf.int.t=function(p=point.estimate,c=confidence.level,s=standard.error, df=degrees.of.freedom)
{
  CI.l=p-qt(1-(1-c)/2,df)*s
  CI.u=p+qt(1-(1-c)/2, df)*s
  list(lower=CI.l,upper=CI.u)
}
#example L14.2
conf.int.t(3,.90,1.721918,9999)