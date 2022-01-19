############ Plotting Drought Metrics for RAPID #############

require(tidyverse)
require(lubridate)

spi_m <- read.csv("SB_SPI_1885-2022_monthly.csv")
spi <- spi_m %>% select(DATE, AVG=X0, D0,D1,D2,D3,D4,W0,W1,W2,W3,W4)
spi$YYYYMMDD <- str_extract(spi$DATE,"\\d+")
spi$YEAR <- str_extract(spi$DATE, "\\d{4}")
spi$MONTH <- str_replace(str_replace(as.character(spi$YYYMMDD),"\\d{4}","" ), "\\d{2}$","")
spi$date <- as_date(spi$YYYYMMDD)
spi <- spi %>% mutate(D01 = D0+D1, D012 = D01+D2,D0123=D012+D3, D01234=D0123+D4)

plot(D012~date, spi[which(spi$date>2000),], type="l")


plot(D0~date, spi[which(year(spi$date)>2008),], type="l", col="yellow")
lines(D2~date, spi[which(year(spi$date)>2008),], type="l", col="orange")
lines(D3~date, spi[which(year(spi$date)>2000),], type="l", col="red")
lines(D4~date, spi[which(year(spi$date)>2000),], type="l", col="darkred")


spi_w <- read.csv("SB_SPI_2000-2022_weekly.csv")
spi_w$date <- as.Date(as.character(spi_w$ValidStart), format="%m/%e/%y")


plot(D0~date, spi_w[which(year(spi_w$date)>2008 & year(spi_w$date)<2022),], type="l", col="yellow")
lines(D2~date, spi_w[which(year(spi_w$date)>2008),], type="l", col="orange")
lines(D3~date, spi_w[which(year(spi_w$date)>2008),], type="l", col="red")
lines(D4~date, spi_w[which(year(spi_w$date)>2008),], type="l", col="darkred")
