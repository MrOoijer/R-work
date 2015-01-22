year.with.month <- function (df){
        # first column of df is year, 
        # return same columns for every month
        df0<-df
        for (i in 1:11){
                df1<-df
                df1[,1]<-df1[,1]+i/12
                df0<-rbind(df0, df1)
        }
        return(df0[order(df0[,1]),]) # should be ordered
}