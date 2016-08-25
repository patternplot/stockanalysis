library(quantmod)

long_short_pair_analysis <- function(lstock,sstock, frdate, todate)
    
{
    my_symbols <- c(lstock, sstock)
    
    stockData <- new.env() 
    
    getSymbols(my_symbols, env= stockData, src='yahoo',from = frdate, to = todate)
    
    basket <<- data.frame(as.xts(merge(stockData[[lstock]], stockData[[sstock]])))
    
    basket <<- basket[,names(basket)[grepl(x=names(basket),pattern='Close')]]
    
    # basket[,paste0(lstock,"_CLCL")] <<- ClCl(stockData[[lstock]])[,1]
    # basket[,paste0(sstock,"_CLCL")] <<- ClCl(stockData[[sstock]])[,1]
    
    
    basket <<- merge(basket, data.frame(ClCl(stockData[[lstock]])), by.x = 0, by.y = 0, all = T)
    colnames(basket)[ncol(basket)] <<- "ClCl.Long.Stock"
    
    basket <<- merge(basket, data.frame(ClCl(stockData[[sstock]])), by.x = "Row.names", by.y = 0, all = T)
    colnames(basket)[ncol(basket)] <<- "ClCl.Short.Stock"
    
    
    basket[,'PERCENT_MOVE'] <<- basket[,"ClCl.Long.Stock"] - basket[,"ClCl.Short.Stock"]
    
    
    
    ANALYSIS.RESULT <<- ifelse(basket[, "PERCENT_MOVE"] > 0 , "PROFIT DAYS", "LOSS DAYS")
    
    TOTAL.PERCENT.MOVE <<- sum(basket[, "PERCENT_MOVE"], na.rm = T ) * 100
    
    TOTAL.PERCENT.MOVE <<- round(TOTAL.PERCENT.MOVE, digits=2)
    
    
    fileConn<-file("output.txt","a")
    writeLines(paste0("LONG STOCK:",lstock), con = fileConn, sep = "\n")
    writeLines(paste0("SHORT STOCK:",sstock), con = fileConn, sep = "\n")  
    writeLines(paste0("PROFIT DAYS:",sum(ANALYSIS.RESULT == "PROFIT DAYS",na.rm=T)), con = fileConn, sep = "\n")
    writeLines(paste0("LOSS DAYS:",sum(ANALYSIS.RESULT == "LOSS DAYS",na.rm=T)), con = fileConn, sep = "\n")
    writeLines(paste0("TOTAL PROFIT/LOSS DURING PERIOD:",TOTAL.PERCENT.MOVE,"%"), con = fileConn, sep = "\n\n")
    close(fileConn)
    
    
    # cat("-----------------------------\n")
    # cat("LONG STOCK:",lstock,"\n")
    # cat("SHORT STOCK:",sstock,"\n")       
    # print.table(table(ANALYSIS.RESULT),justify = "left")
    # cat("TOTAL PROFIT/LOSS DURING PERIOD:",TOTAL.PERCENT.MOVE * 100,"%","\n")
    # cat("\n")
    # 
    # plot(as.Date(as.character(basket$Row.names)), basket$PERCENT_MOVE, col="blue",
    #      type='s', ylab="", xlab="")
    
}

long_short_basket <- function(tickers, from.date = "2008-01-01", to.date = Sys.Date() )
{
    tknum <- length(tickers)
    
    if (tknum < 2)
    {
        stop ('Please pass at least 2 stock tickers for analysis.')
        
    }
    
    for( t in 1:(tknum-1))
    {
        
        long.stock <- tickers[t]
        
        short.candidates <- tickers[(t+1):tknum]
        
        for(s in short.candidates)
        {
            short.stock <- s
            long_short_pair_analysis(long.stock, short.stock,from.date, to.date )
        }
        
    }
}
