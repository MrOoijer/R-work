days.since <- function(date){
        # date is char string yyyy-mm-dd
        yr=substr(date,1,4)
        d2=sprintf("%s-01-01", yr)
        d3=sprintf("%s-12-31", yr)
        a1<-as.numeric(as.Date(date)-as.Date(d2))
        a2<-as.numeric(as.Date(d3)-as.Date(d2))+1
        as.numeric(yr) + (a1/a2)
        
}