library(quantmod)

open_price_day_trade_analysis <- function(tkrsym, frdate, todate,long.threshold, short.threshold,loss.limit,profit.preserve )
    
{
    
    # Get extra 1 year prior data to help in calculating moving averages etc.  
    data.frdate <<- as.Date(frdate) - 200
    
    stockData <- new.env()
    
    getSymbols(tkrsym, env= stockData, src='yahoo',from = data.frdate, to = todate)
    
    stkHist <<- data.frame(as.xts(stockData[[tkrsym]]))
    
    stkHist <<- merge(stkHist, data.frame(ClOp(stockData[[tkrsym]])), by.x = 0, by.y = 0, all = T)
    
    
    
    # Make Column Names generic for later calls
    colnames(stkHist) <<- sub(paste0(tkrsym,"."), "Stock.", colnames(stkHist))
    colnames(stkHist)[ncol(stkHist)] <<- "Stock.ClOp"
    
    
    stkHist$Stock.ShortTerm.MA <<- EMA(stkHist$Stock.Close, n=5)
    stkHist$Stock.LongTerm.MA  <<- EMA(stkHist$Stock.Close, n=10)
    stkHist$Stock.SD  <<- runSD(log(stkHist$Stock.Close), n=3)
    
    stkHist <<- stkHist[-which(as.character.Date(stkHist$Row.names) < frdate),]
    
    stkHist$TRADE[1] <<- "NO TRADE"
    stkHist$trend.factor[1] <<- 0
    stkHist$PERCENT.RETURN[1] <<- 0
    stkHist$PERCENT.RETURN[1] <<- 0
    
    
    # stkHist$TRADE[2] <<- "NO TRADE"
    # stkHist$trend.factor[2] <<- 0
    # stkHist$PERCENT.RETURN[2] <<- 0
    
    
    for(i in 2:nrow(stkHist))
        
    {
        
        # stkHist$trend.factor[i] <<- (stkHist$Stock.Close[i] - stkHist$Stock.ShortTerm.MA[i])*10/stkHist$Stock.Close[i]
        #  stkHist$trend.factor[i] <<- (stkHist$Stock.Close[i-1] - stkHist$Stock.ShortTerm.MA[i-1])*10/stkHist$Stock.Close[i-1]
        # if((stkHist$Stock.Open[i] >= 1.03*stkHist$Stock.ShortTerm.MA[i-1]))
        # {
        #   stkHist$trend.factor[i] <<- 0.3
        # }
        # else if ((stkHist$Stock.Open[i] < 0.97*stkHist$Stock.ShortTerm.MA[i-1]))
        # {
        #   stkHist$trend.factor[i] <<- -0.3
        # }
        # 
        # else
        # {
        #   stkHist$trend.factor[i] <<- 0
        #   
        # }
        
        # if(stkHist$Stock.SD[i-1] > .04) {stkHist$tooVolatile[i] <<- TRUE}
        
        
        stkHist$trend.factor[i] <<- ((stkHist$Stock.Close[i-1]/stkHist$Stock.ShortTerm.MA[i-1]) - 1) * 10
        
        
        
        # Stock Opens GREEN -> Take Long position on the Stock and (Optional) Set floor and/or Ceiling 
        # if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.ClOp[i]*100 >= -stkHist$trend.factor[i]))
        #  if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.Open[i] >= stkHist$Stock.ShortTerm.MA[i-1]))
        
        #     if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.Open[i] >= 1.03*stkHist$Stock.ShortTerm.MA[i-1]))
        if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.SD[i-1] <= .04) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.ShortTerm.MA[i-1]  >= 1.005*stkHist$Stock.LongTerm.MA[i-1]))
            #       if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.ShortTerm.MA[i-1]  >= 1.01*stkHist$Stock.LongTerm.MA[i-1])  && (stkHist$Stock.ShortTerm.MA[i-2]  >= 1.01*stkHist$Stock.LongTerm.MA[i-2]))
            #    if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 >= -long.threshold) && (stkHist$Stock.ShortTerm.MA[i-1]  >= 1.01*stkHist$Stock.LongTerm.MA[i-1]))
        {
            
            stkHist$TRADE[i] <<- "LONG" 
            
            # STOP LOSS (FLOOR)
            stkHist$PERCENT.RETURN[i] <<- ifelse(stkHist$Stock.Low[i] < (1-(loss.limit/100)) * stkHist$Stock.Open[i], -loss.limit,(stkHist$Stock.Close[i] - stkHist$Stock.Open[i])*100/stkHist$Stock.Open[i] )
            
            # PRESERVE PROFIT (CEILING)
            stkHist$PERCENT.RETURN[i] <<- ifelse((stkHist$Stock.High[i] > (1+(profit.preserve/100)) * stkHist$Stock.Open[i]) & (stkHist$PERCENT.RETURN[i] != -loss.limit), profit.preserve,stkHist$PERCENT.RETURN[i] )
            
            
        }
        # Stock Opens RED -> Short the Stock and (Optional) Set floor and/or Ceiling 
        # else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.ClOp[i]*100 < -stkHist$trend.factor[i]))
        #  else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.Open[i] < stkHist$Stock.ShortTerm.MA[i-1]))
        #  else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.Open[i] < 0.97*stkHist$Stock.ShortTerm.MA[i-1]))
        
        else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.SD[i-1] <= .04) && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.ShortTerm.MA[i-1] < 0.995*stkHist$Stock.LongTerm.MA[i-1]))
            
            #     else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.ShortTerm.MA[i-1] < 0.99*stkHist$Stock.LongTerm.MA[i-1]) && (stkHist$Stock.ShortTerm.MA[i-2] < 0.99*stkHist$Stock.LongTerm.MA[i-2]))
            #    else if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= short.threshold) && (stkHist$Stock.ShortTerm.MA[i-1] < 0.99*stkHist$Stock.LongTerm.MA[i-1]))
        {
            
            stkHist$TRADE[i] <<- "SHORT" 
            
            # STOP LOSS
            stkHist$PERCENT.RETURN[i] <<- ifelse(stkHist$Stock.High[i] > (1+(loss.limit/100)) * stkHist$Stock.Open[i], -loss.limit,(- stkHist$Stock.Close[i] + stkHist$Stock.Open[i])*100/stkHist$Stock.Close[i] )
            
            # PRESERVE PROFT 
            stkHist$PERCENT.RETURN[i] <<- ifelse((stkHist$Stock.Low[i] < (1-(profit.preserve/100)) * stkHist$Stock.Open[i]) & (stkHist$PERCENT.RETURN[i] != -loss.limit), profit.preserve,stkHist$PERCENT.RETURN[i] )
            
            
            
        }
        else
        {
            stkHist$TRADE[i] <<- "NO TRADE"
            stkHist$PERCENT.RETURN[i] <<- 0
            
        }
        
        
        
    }
    
    STOCK.MOVE.DURING.PERIOD <<- (stkHist[nrow(stkHist),"Stock.Close"] - stkHist[1,"Stock.Close"])*100/stkHist[1,"Stock.Close"]
    STOCK.MOVE.DURING.PERIOD <<- round(STOCK.MOVE.DURING.PERIOD, digits=2)
    
    NOTRADES <<- which(stkHist$TRADE == "NO TRADE")
    # stkHist <<- stkHist[-NOTRADES,]
    
    WINNING.LONG.POSITIONS <<- which(stkHist$TRADE == "LONG" & stkHist$PERCENT.RETURN > 0 )
    LOSING.LONG.POSITIONS <<- which(stkHist$TRADE == "LONG" & stkHist$PERCENT.RETURN <= 0 )
    
    WINNING.SHORT.POSITIONS <<- which(stkHist$TRADE == "SHORT" & stkHist$PERCENT.RETURN > 0 )
    LOSING.SHORT.POSITIONS <<- which(stkHist$TRADE == "SHORT" & stkHist$PERCENT.RETURN <= 0 )
    
    DAY.ANALYSIS <<- ifelse(stkHist[-NOTRADES, "PERCENT.RETURN"] > 0 , "POS", "NEG")
    
    TOTAL.PERCENT.MOVE <<- sum(stkHist[-NOTRADES, "PERCENT.RETURN"], na.rm = T ) 
    
    TOTAL.PERCENT.MOVE <<- round(TOTAL.PERCENT.MOVE, digits=2)
    
    
    fileConn<-file("output.txt","a")
    writeLines(paste0("STOCK:",tkrsym), con = fileConn, sep = "\n")
    writeLines(paste("BETWEEN TIME RANGE:",frdate,"AND",todate), con = fileConn, sep = "\n")
    writeLines(paste0("LONG THRESHOLD:",long.threshold,"%"), con = fileConn, sep = "\n")
    writeLines(paste0("SHORT THRESHOLD:",short.threshold,"%"), con = fileConn, sep = "\n")
    writeLines(paste0("STOP LOSS:",loss.limit,"%"), con = fileConn, sep = "\n")
    writeLines(paste0("PROFIT PRESERVE:",profit.preserve,"%"), con = fileConn, sep = "\n")
    writeLines(paste0("POSITIVE RETURN DAYS:",sum(DAY.ANALYSIS == "POS",na.rm=T)), con = fileConn, sep = "\n")
    writeLines(paste0("NEGATIVE RETURN DAYS:",sum(DAY.ANALYSIS == "NEG",na.rm=T)), con = fileConn, sep = "\n")
    writeLines(paste0("NO TRADE DAYS:",length(NOTRADES)), con = fileConn, sep = "\n")
    writeLines(paste0("LONG POSITIONS (WIN /LOSS):",length(WINNING.LONG.POSITIONS),"/",length(LOSING.LONG.POSITIONS)), con = fileConn, sep = "\n")
    writeLines(paste0("SHORT POSITIONS (WIN /LOSS):",length(WINNING.SHORT.POSITIONS),"/",length(LOSING.SHORT.POSITIONS)), con = fileConn, sep = "\n")
    writeLines(paste0("TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:",TOTAL.PERCENT.MOVE,"%"), con = fileConn, sep = "\n")
    writeLines(paste0("STOCK MOVE DURING PERIOD:",STOCK.MOVE.DURING.PERIOD,"%"), con = fileConn, sep = "\n\n")
    close(fileConn)
    
    
}

open_price_trade_basket <- function(tickers, from.date = "2008-01-01", to.date = Sys.Date(), lt=1, st =1, ll =3,pp =1 )
{
    tknum <- length(tickers)
    
    if (tknum < 1)
    {
        stop ('Please pass at least 1 stock ticker for analysis.')
        
    }
    
    for( t in 1:tknum)
    {
        
        tkr <- tickers[t]
        
        
        open_price_day_trade_analysis(tkr,from.date, to.date,lt,st,ll,pp )
        
        
    }
}

ClOp <-
    function(x)
    {
        xx <- Delt(Lag(Cl(x)),Op(x))
        colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
        xx
    }


