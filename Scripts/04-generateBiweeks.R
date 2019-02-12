#!/usr/bin/env Rscript --vanilla

    # setup environment and functions

bw <- new.env()
bw$makeStart <- function(year){
    as.Date(paste(c(year,1,1), collapse='-'))
}


### USE THIS FUNCTION TO GET THE BIWEEK NUMBER GIVEN A DATE VECTOR
bw$get <- function(d){
    d <- as.Date(d)
    d <- as.integer(cut(d, breaks=bw$seq))
    d <- (d %% 26)
    ifelse(d==0,26,d)
}

bw$getYear <- function(d){
    d <- as.Date(d)
    d <- as.integer(cut(d, breaks=bw$seq))
    bw$seqYear[d]
}





    # setup parameters

# 1st year of Dengue Brazil data
#bw$start <- bw$makeStart(1999)
bw$start <- as.Date('1999-01-20')+14

# sequence of biweek cuts
bw$seq <- seq(bw$start, Sys.Date()+1, by="2 weeks")

# year of the biweeks
bw$seqYear <- as.integer(format(bw$seq[1],'%Y')) + cumsum(bw$get(bw$seq)==1) - 1




#plot(
#    x = as.Date(paste0("2000-",format(bw$seq, "%m-%d")))
#    , y = bw$get(bw$seq)
#)
