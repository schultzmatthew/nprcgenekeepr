#Calculate PSI
cal_psi <- function(data, bench, target, bin)
{
  ben<-sort(data[,bench]);
  tar<-sort(data[,target]);
  # get and sort benchmark and target variable
  ttl_bench<-length(tar);
  ttl_target<-length(ben);
  # get total num obs for benchmark and target
  n<-ttl_bench%/%bin; #Num of obs per bin
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin

  for (i in 1:bin) # calculate PSI for ith bin
  {

    lower_cut<-ben[(i-1)*n+1];
    if(i!=bin){upper_cut<-ben[(i-1)*n+n]; pct_ben<-n/ttl_bench;} else
    {upper_cut<-ben[ttl_bench];
    pct_ben<(ttl_bench-n*(bin-1))/ttl_bench;}
    #last bin should have all remaining obs

    pct_tar<-length(tar[tar>lower_cut&tar<=upper_cut])/ttl_target;
    psi_bin[i]<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  }
  psi<-sum(psi_bin) * (20 / bin);
  return(psi);
}
set.seed(1)
#let's generate two uniform distributions between 0 to 1
#with obs=1000 to represent model score.
score1<-runif(1000,0,1);
score2<-runif(1000,0,1);
dataset<-data.frame(score1,score2);
cal_psi(data=dataset, bench="score1",target="score2",bin=100)
