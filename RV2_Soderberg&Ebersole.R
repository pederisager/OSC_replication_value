#Inputs
# Cite is the current number of citations for an article here.
# IF is the impact factor for the journal where the article was published here. If you can find the impact factor from when the article was first published, that is ideal. If not, use the current impact factor.
# Years is the number of years used to calculate the impact factor you used. Most often, impact factors are calculated over a period of two years.
# MonthsTotal is the number of months since the article was originally published.
# Pval is a list [c()] of p-value(s) for independent, key finding(s) of each study in the article


RVfunction<-function(Cite,IF,MonthsTotal,Pval) {
  
  # function to load/install packages (credit: CousinCocaine on Stack Exchange)
  packages <- function(x) {
    
    x <- as.character(match.call()[[2]])
    
    if ( ! require(x, character.only = TRUE)) {
      
      install.packages(pkgs = x, repos = "http://cran.r-project.org")
      require(x, character.only = TRUE)    
    }
  }
  
  packages(metap)
  
  Pval <- 20* Pval  ##turning p-values into pp-values 
  
  ##compute YearsIF variable and then calculate replication value
  
  if(length(Pval)==1) {
    RV<-(Cite - (IF * MonthsTotal)/12) * Pval
    
  }else {
    RV<-(Cite - (IF * MonthsTotal)/12) * (sumz(Pval)$p)
    
  }
  return(RV)
}

#Fill in inputs below
Pval<-c(0.005, 0.04, 0.03)
Cite<-(1)
IF<-(3)
MonthsTotal<-(100)

RVfunction(Cite,IF,MonthsTotal,Pval)
