# Time Series Plotting

install.packages("ggplot2")
install.packages("xts")
install.packages("dygraphs")

library(ggplot2)
library(xts)
library(dygraphs)

# Get IBM and Linkedin stock data from Yahoo Finance
ibm_url <- "http://real-chart.finance.yahoo.com/table.csv?s=IBM&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
lnkd_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"
tsla_url <- "http://real-chart.finance.yahoo.com/table.csv?s=TSLA&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"

yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

ibm  <- yahoo.read(ibm_url)
lnkd2 <- yahoo.read(lnkd_url)
tsla <- yahoo.read(tsla_url)

ggplot(ibm,aes(Date,Close)) + 
  geom_line(aes(color="ibm")) +
  geom_line(data=lnkd2,aes(color="lnkd")) +
  geom_line(data=tsla, aes(color="tsla"))
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("ibm", "lnkd", "tsla"),
                      values = c("blue", "brown", "green")) +
  ggtitle("Closing Stock Prices: IBM & Linkedin & Tesla") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))

# Plot with the htmlwidget dygraphs
# dygraph() needs xts time series objects
ibm_xts <- xts(ibm$Close,order.by=ibm$Date,frequency=365)
lnkd_xts <- xts(lnkd2$Close,order.by=lnkd2$Date,frequency=365)
tsla_xts <- xts(tsla$Close,order.by = tsla$Date,frequency=365)

stocks <- cbind(ibm_xts,lnkd_xts, tsla_xts)

dygraph(stocks,ylab="Close", 
        main="IBM and Linkedin and Tesla Closing Stock Prices") %>%
  dySeries("..1",label="IBM") %>%
  dySeries("..2",label="LNKD") %>%
  dySeries("..3",label="TSLA") %>%
  dyOptions(colors = c("blue","brown", "green")) %>%
  dyRangeSelector()
