mhsampler<-function(X){
  N = 5000000;
  lambda = vector(length = N);
  M = vector(length = N);
  lambda[1]=240*X;
  M[1]=8;
  u = runif(N,-10,10);
  w= rbinom(N,1,0.5);
  for (i in 2:N)
  { if ((u[i-1]+lambda[i-1])>0) 
  {        lambda[i]=u[i-1]+lambda[i-1];
  }    
    else   
    {        lambda[i]=0
    }
    
    if (M[i-1]==9) 
    {   if (w[i-1]==0) 
    { M[i]=8}
      else   
      {M[i]=10}
    }
    
    else   
    { M[i]=9}
  }
  return(data.frame(M[1:1000],lambda[1:1000]))
  
}
samper1=mhsampler(2)
samper2=mhsampler(4)
samper3=mhsampler(6)
samper4=mhsampler(8)
