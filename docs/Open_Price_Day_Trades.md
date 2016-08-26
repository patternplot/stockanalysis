### Open Price Day Trades

Summary
=======

Suppose that every day before the stock market open, we checked if the
stock's short-term moving average (STMA) - for example, an average taken
from the last 5 trading days - was more or less than the stock's
long-term moving average (LTMA) - for example, an average taken from the
last 10 trading days. Then if the STMA of the stock was found to be more
than the LTMA by a certain percentage threshold, we placed a BUY order
on the stock at the open price. If, on the other hand, the STMA was less
than the LTMA by a certain threshold, we took a short position on the
stock.In either case, we closed our position just before the close of
the market on the same day.

Optionally, we can also set a loss limit % and a proft preserve %
thresholds. In the case when loss limit % is set, we determine if the
loss threshold was reached based on the high/low stock price of the day.
If it was, we will assume the position was closed at that given stock
price (and not wait for the market close). We do the same when the
profit preserve % is set. When both the thresholds are set and the day's
stock high/low price suggests that both thresholds could have been met,
we will assume that the loss threshold was reached first and take our
losses.

This analysis does back testing on historical stock prices to find the
likely profit/loss from the above strategy for a given time period.For
the purpose of this analysis, we will not consider broker commission
fees when calculating profit/loss.

Code
====

Below is a wrapper function (*open\_price\_trade\_basket*) that takes in
the following parameters:

`tickers` --\> a vector containing one or more stock ticker symbols

`from.date` --\> FROM DATE of the DATE RANGE of our analysis

`to.date` --\> TO DATE of the DATE RANGE of our analysis

`lt` --\> LONG POSITION THRESHOLD %: We can set this threshold to
indicate that we want to take a LONG POSITION only if the stock's OPEN
PRICE is NOT MORE than this % above last trading day's ClOSE PRICE. The
premise is that if the stock opens too high, it is not likely to go much
higher during the day.

`st` --\> SHORT POSITION THRESHOLD %: We can set this threshold to
indicate that we want to take a SHORT POSITION only if the stock's OPEN
PRICE is NOT MORE than this % below last trading day's ClOSE PRICE. The
premise is that if the stock opens too low, it is not likely to go much
lower during the day.

`ll` --\> STOP LOSS THRESHOLD % : Setting this position will close the
position if the stock hits this loss threshold during the day. For
simplicity, we determine if this threshold was reached by looking the
Stock's intraday High/Low price.

`pp` --\> PROFIT PRESERVE THRESHOLD % : Setting this position will close
the position if the stock hits this proft threshold during the day. For
simplicity, we determine if this threshold was reached by looking the
Stock's intraday High/Low price.

If by looking at Stock's High/Low price, we determine that *both* STOP
LOSS THRESHOLD % and PROFIT PRESERVE THRESHOLD % could have been
reached, we assume that LOSS THRESHOLD was reached first and take our
loss for the day.

    require(quantmod)

    # Wrapper Function
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

    #Workhorse function
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
        
        # Doing EXPONENTIAL MOVING AVERAGES. We can try other types of moving averages as well.
        stkHist$Stock.ShortTerm.MA <<- EMA(stkHist$Stock.Close, n=5)
        stkHist$Stock.LongTerm.MA  <<- EMA(stkHist$Stock.Close, n=10)
        
        # Can use 'Standard Deviation' to determine volatility if needed (not doing it in this code)
        # stkHist$Stock.SD  <<- runSD(log(stkHist$Stock.Close), n=3)
        
        # Taking out all the rows we got only to calculate moving averages
        stkHist <<- stkHist[-which(as.character.Date(stkHist$Row.names) < frdate),]
        
        
        # We will not use the first row - as there is no previous Close to Open for this now.
        stkHist$TRADE[1] <<- "NO TRADE"
        stkHist$trend.factor[1] <<- 0
        stkHist$PERCENT.RETURN[1] <<- 0
        stkHist$PERCENT.RETURN[1] <<- 0
        
      
        for(i in 2:nrow(stkHist))
            
        {
            

            if(!is.na(stkHist$Stock.ClOp[i]) && (stkHist$Stock.ClOp[i]*100 <= long.threshold) && (stkHist$Stock.ShortTerm.MA[i-1]  >= 1.005*stkHist$Stock.LongTerm.MA[i-1]))
            {
                
                stkHist$TRADE[i] <<- "LONG" 
                
                # STOP LOSS 
                stkHist$PERCENT.RETURN[i] <<- ifelse(stkHist$Stock.Low[i] < (1-(loss.limit/100)) * stkHist$Stock.Open[i], -loss.limit,(stkHist$Stock.Close[i] - stkHist$Stock.Open[i])*100/stkHist$Stock.Open[i] )
                
                # PRESERVE PROFIT (IF STOP LOSS WAS NOT HIT)
                stkHist$PERCENT.RETURN[i] <<- ifelse((stkHist$Stock.High[i] > (1+(profit.preserve/100)) * stkHist$Stock.Open[i]) & (stkHist$PERCENT.RETURN[i] != -loss.limit), profit.preserve,stkHist$PERCENT.RETURN[i] )
                
                
            }

            else if(!is.na(stkHist$Stock.ClOp[i])  && (stkHist$Stock.ClOp[i]*100 >= -short.threshold) && (stkHist$Stock.ShortTerm.MA[i-1] < 0.995*stkHist$Stock.LongTerm.MA[i-1]))

            {
                
                stkHist$TRADE[i] <<- "SHORT" 
                
                # STOP LOSS
                stkHist$PERCENT.RETURN[i] <<- ifelse(stkHist$Stock.High[i] > (1+(loss.limit/100)) * stkHist$Stock.Open[i], -loss.limit,(- stkHist$Stock.Close[i] + stkHist$Stock.Open[i])*100/stkHist$Stock.Close[i] )
                
                # PRESERVE PROFIT (IF STOP LOSS WAS NOT HIT ALREADY)
                stkHist$PERCENT.RETURN[i] <<- ifelse((stkHist$Stock.Low[i] < (1-(profit.preserve/100)) * stkHist$Stock.Open[i]) & (stkHist$PERCENT.RETURN[i] != -loss.limit), profit.preserve,stkHist$PERCENT.RETURN[i] )
                
               
            }
            else
            {
                stkHist$TRADE[i] <<- "NO TRADE"
                stkHist$PERCENT.RETURN[i] <<- 0
                
            }
            
            
            
        }
        
        # How much did the underlying stock move during the time period
        STOCK.MOVE.DURING.PERIOD <<- (stkHist[nrow(stkHist),"Stock.Close"] - stkHist[1,"Stock.Close"])*100/stkHist[1,"Stock.Close"]
        
        # Rounding off
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
        
        cat(paste0("STOCK:",tkrsym),"\n")
        cat(paste("BETWEEN TIME RANGE:",frdate,"AND",todate),"\n")
        cat(paste0("LONG THRESHOLD:",long.threshold,"%"),"\n")
        cat(paste0("SHORT THRESHOLD:",short.threshold,"%"),"\n")
        cat(paste0("STOP LOSS:",loss.limit,"%"), "\n")
        cat(paste0("PROFIT PRESERVE:",profit.preserve,"%"),"\n")
        cat(paste0("POSITIVE RETURN DAYS:",sum(DAY.ANALYSIS == "POS",na.rm=T)),"\n")
        cat(paste0("NEGATIVE RETURN DAYS:",sum(DAY.ANALYSIS == "NEG",na.rm=T)),"\n")
        cat(paste0("NO TRADE DAYS:",length(NOTRADES)),"\n")
        cat(paste0("LONG POSITIONS (WIN /LOSS):",length(WINNING.LONG.POSITIONS),"/",length(LOSING.LONG.POSITIONS)),"\n")
        cat(paste0("SHORT POSITIONS (WIN /LOSS):",length(WINNING.SHORT.POSITIONS),"/",length(LOSING.SHORT.POSITIONS)),"\n")
        cat(paste0("STOCK MOVE DURING PERIOD:",STOCK.MOVE.DURING.PERIOD,"%"),"\n")
        cat(paste0("TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:",TOTAL.PERCENT.MOVE,"%"),"\n\n")
        
    }


    ClOp <-
        function(x)
        {
            xx <- Delt(Lag(Cl(x)),Op(x))
            colnames(xx) <- paste("ClOp", deparse(substitute(x)), sep='.')
            xx
        }

ANALYSIS
========

Let us test this strategy with various stocks

    open_price_trade_basket(c("BAC", "USO","INTC","FB","TXN"), from.date="2015-01-01", to.date="2016-08-25",lt=10, st=10,ll=3,pp=15)

    ## STOCK:BAC 
    ## BETWEEN TIME RANGE: 2015-01-01 AND 2016-08-25 
    ## LONG THRESHOLD:10% 
    ## SHORT THRESHOLD:10% 
    ## STOP LOSS:3% 
    ## PROFIT PRESERVE:15% 
    ## POSITIVE RETURN DAYS:140 
    ## NEGATIVE RETURN DAYS:128 
    ## NO TRADE DAYS:148 
    ## LONG POSITIONS (WIN /LOSS):72/63 
    ## SHORT POSITIONS (WIN /LOSS):68/65 
    ## STOCK MOVE DURING PERIOD:-13.24% 
    ## TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:49.55% 
    ## 
    ## STOCK:USO 
    ## BETWEEN TIME RANGE: 2015-01-01 AND 2016-08-25 
    ## LONG THRESHOLD:10% 
    ## SHORT THRESHOLD:10% 
    ## STOP LOSS:3% 
    ## PROFIT PRESERVE:15% 
    ## POSITIVE RETURN DAYS:174 
    ## NEGATIVE RETURN DAYS:150 
    ## NO TRADE DAYS:92 
    ## LONG POSITIONS (WIN /LOSS):73/61 
    ## SHORT POSITIONS (WIN /LOSS):101/89 
    ## STOCK MOVE DURING PERIOD:-44.95% 
    ## TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:70.38% 
    ## 
    ## STOCK:INTC 
    ## BETWEEN TIME RANGE: 2015-01-01 AND 2016-08-25 
    ## LONG THRESHOLD:10% 
    ## SHORT THRESHOLD:10% 
    ## STOP LOSS:3% 
    ## PROFIT PRESERVE:15% 
    ## POSITIVE RETURN DAYS:128 
    ## NEGATIVE RETURN DAYS:100 
    ## NO TRADE DAYS:188 
    ## LONG POSITIONS (WIN /LOSS):75/41 
    ## SHORT POSITIONS (WIN /LOSS):53/59 
    ## STOCK MOVE DURING PERIOD:-3.49% 
    ## TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:19.87% 
    ## 
    ## STOCK:FB 
    ## BETWEEN TIME RANGE: 2015-01-01 AND 2016-08-25 
    ## LONG THRESHOLD:10% 
    ## SHORT THRESHOLD:10% 
    ## STOP LOSS:3% 
    ## PROFIT PRESERVE:15% 
    ## POSITIVE RETURN DAYS:107 
    ## NEGATIVE RETURN DAYS:114 
    ## NO TRADE DAYS:195 
    ## LONG POSITIONS (WIN /LOSS):74/77 
    ## SHORT POSITIONS (WIN /LOSS):33/37 
    ## STOCK MOVE DURING PERIOD:57.92% 
    ## TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:-0.88% 
    ## 
    ## STOCK:TXN 
    ## BETWEEN TIME RANGE: 2015-01-01 AND 2016-08-25 
    ## LONG THRESHOLD:10% 
    ## SHORT THRESHOLD:10% 
    ## STOP LOSS:3% 
    ## PROFIT PRESERVE:15% 
    ## POSITIVE RETURN DAYS:117 
    ## NEGATIVE RETURN DAYS:104 
    ## NO TRADE DAYS:195 
    ## LONG POSITIONS (WIN /LOSS):82/56 
    ## SHORT POSITIONS (WIN /LOSS):35/48 
    ## STOCK MOVE DURING PERIOD:30.55% 
    ## TOTAL PROFIT/LOSS DURING PERIOD WITH THIS STRATEGY:12.52%

CONCLUSION
==========

This strategy works nicely if the market is following a NET UP TREND or
a NET DOWN TREND . It does not do so well during the times when the
market was on a roller-coaster ride (as was the case for Facebook a few
times during the time period in the above analysis). This is because by
the time the moving averages determine a trend, the market has already
changed direction.

Also, this strategy is guaranteed to make your broker rich - with all
the commission fees !!
