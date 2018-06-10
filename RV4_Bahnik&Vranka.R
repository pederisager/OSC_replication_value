
rv <- function(citations, ps, Ns, Ts, unique = FALSE, r = 0.8, k = 10, studies = NA) {
  s <- 1
  n <- length(ps)
  for(i in 1:n) {
    s <- s / ((40*ps[i]) ** (sqrt(Ns[i]*Ts[i]) / k))
  }
  if(!unique) {
    if(!is.na(studies)) n <- studies
    citations <- citations * (1-r**n)/((1-r)*n)
  }
  return(citations / max(s, 1/s))
}


#The code below can be adapted to use the formula above.
#ps, Ns, and Ts need to be a vector of equal length
ps<-c(0.01, 0.03, 0.003, 0.045) #p-values for each study
Ns<-c(40, 65, 210, 24) #total number of participants per study
Ts<- c(1, 1, 1, 1) #number of trials
citations<-324 #number of citations
rv(ps=ps, Ns=Ns, Ts=Ts, citations=citations)#run formula
