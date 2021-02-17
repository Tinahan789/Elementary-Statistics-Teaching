# function that computes the chi-squared statistic parts
chi.s=function(o=observed,e=expected){
  cst=(o-e)^2/e
  return(cst)
}

#Independent experiments
oo1=10;og1=11;or1=9 #data set 1
oo2=5;og2=15;or2=10 #data set 2
oo3=2;og3=14;or3=14 #data set 3
e=10 #expected frequency
d=6 #pooled degrees of freedom
c1=sum(chi.s(oo1,e),chi.s(og1,e),chi.s(or1,e));c1 #first chi-squared statistic
c2=sum(chi.s(oo2,e),chi.s(og2,e),chi.s(or2,e));c2 #second chi-squared statistic
c3=sum(chi.s(oo3,e),chi.s(og3,e),chi.s(or3,e));c3 #third chi-squared statistic
c=c1+c2+c3;c #pooled chi-squared statistic
p=pchisq(c,d,lower.tail = FALSE);p #p-value

#one test with bigger sample
e=30 #expected frequency
d=2 #degrees of freedom
c=sum(chi.s(oo1+oo2+oo3,e),chi.s(og1+og2+og3,e),chi.s(or1+or2+or3,e));c #chi-squared statistic
p=pchisq(c,d,lower.tail = FALSE);p #p-value
