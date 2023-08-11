setwd("G:/N_Marine/04 A & R/Technical Guidance/Statistics Group ECMAS d/R code")

library(dygraphs)
library(tidyverse)
library(xts)
library(htmltools)
library(tseries)
library(astsa)


#load the wims_d dataset 

wims.d <- read.csv("G:\\N_Marine\\07 Training & Reference Documents\\A&R Technical Guidance\\Statistics Group ECMAS d\\R code\\datasets\\wims_d.csv")


#Subset by variables of interest - here DIN & Chlorophyll

DIN <- wims.d[which(wims.d$Det.Name %in% c('Nitrogen, Dissolved Inorganic : as N')), ]
DIN <- rename(DIN, DIN = Res)


Chla <- wims.d[which(wims.d$Det.Name %in% c('Chlorophyll : Acetone Extract')), ]
Chla <- rename(Chla, Chla = Res)



df <- merge(Chla, DIN, by= c("PtCode", "Date"), all.y=T, all.x=T)

#format date 

df$Date <- as.Date(df$Date, origin="1899-12-30") # convert from numeric to dates (note: Apparently, Excel's calendar starts on 1 January 1900.  Why one has to use an origin of 30 December 1899 in R)
 

# plot/check for outliers

plot(df$Date, df$DIN)

#hmmm it looks like there's a major outlier in 2013. Let's remove it - well let's just remove any DIN values >500

df <- df [which(df$DIN < 500),]
plot(df$Date, df$DIN)# that's better!

# plot/check for outliers

plot(df$Date, df$Chla) # looks ok?



#coerce to xts timeseries format 
df_d <- df$Date
vars <- c("Chla", "DIN")
df_ts <- df[vars]
ts1 <- xts(df_ts, order.by = df_d)


rm(DIN, Chla, wims.d) #cleanup

# Plot as interactive timeseries graph 

dyHide <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Hide",
    path = system.file("plugins/hide.js", package = "dygraphs")
  )
}

p1 <- dygraph(ts1, xlab="Date", group= "DinCha") %>% 
  dyHide() %>%
  dyAxis("y", label = "DIN (mg/L)", valueRange = c(0, 25.0), independentTicks = T)%>%
  dyAxis("y2", label = "Chla (ug/L)", valueRange = c(0, 600.0), independentTicks = T) %>%
  dySeries("Chla", axis=('y2'),color="green") %>% 
  dySeries("DIN", axis=('y'),color="navy") %>%
  dyRangeSelector(height=20) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
  dyOptions(axisLineWidth = 1.5, fillGraph = T, fillAlpha = 0.3, drawGrid = F, drawPoints=T, pointSize=1)
p1

# now let's make summary timeseries plots 

#first calculate summary statistics for Chla

ts_Chla <- df %>% group_by(Date) %>% summarise(
  Chla_sd = sd(Chla),
  Chla = mean(Chla) 
  )

# where there was only one observation for a date then the SD summarisation produces NaNs - let's replace these with 0

ts_Chla$Chla_sd[is.nan(ts_Chla$Chla_sd)] <- 0 

#and now use these to add upper and lower values to the means 

ts_Chla$Chla_uv <- (ts_Chla$Chla + ts_Chla$Chla_sd)
ts_Chla$Chla_lv <- (ts_Chla$Chla - ts_Chla$Chla_sd)
ts_Chla$Chla_lv[ts_Chla$Chla_lv < 0] <- 0
ts_Chla$Chla_uv [is.na (ts_Chla$Chla_uv)] <-0
Chla_d <- ts_Chla$Date
#now remove the variables Chla_sd and DIN_sd 

vars<- c("Chla", "Chla_uv", "Chla_lv")

ts_Chla <- ts_Chla[vars]

#and coerce to xts object 

ts_Chla <- xts(ts_Chla, order.by = Chla_d)

#plot 

p2 <- dygraph(ts_Chla, xlab="Date", group= "DinCha") %>% 
  dyAxis("y", label = "Chla (ug/L)", valueRange = c(0, 200), logscale = T)%>%
  dySeries(c("Chla_lv", "Chla", "Chla_uv"), label="Chla", axis=('y'), color="green") %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = F, drawPoints=T, pointSize=1)
p2

# Now for DIN

ts_DIN <- df %>% group_by(Date) %>% summarise(
  DIN_sd = sd(DIN),
  DIN = mean(DIN) 
)
ts_DIN$DIN_sd[is.nan(ts_DIN$DIN_sd)] <- 0 
ts_DIN$DIN_uv <- (ts_DIN$DIN + ts_DIN$DIN_sd)
ts_DIN$DIN_lv <- (ts_DIN$DIN - ts_DIN$DIN_sd)
ts_DIN$DIN_lv[ts_DIN$DIN_lv < 0] <- 0
ts_DIN$DIN_uv [is.na (ts_DIN$DIN_uv)] <-0
DIN_d <- ts_DIN$Date

vars<-c("DIN", "DIN_uv", "DIN_lv")
ts_DIN <- ts_DIN [vars]

ts_DIN <- xts(ts_DIN, order.by = DIN_d)

p3 <- dygraph(ts_DIN, xlab="Date", group= "DinCha") %>% 
  dySeries(c("DIN_lv", "DIN", "DIN_uv"), label="DIN", axis=('y'), color="navy") %>%
  dyAxis("y", label = "DIN (mg/L)", valueRange = c(0, 15), independentTicks = T)%>%  
  dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = F, drawPoints=T, pointSize=1)
p3

#sychronise & plot together - now try zooming in to any series...

browsable(
  tagList(
   p1,p2,p3
  )
)


# Explore seasonal trends by decomposing the timeseries using Loess

DIN.ts <- ts(df$DIN, start = c(2007,1), end = c(2018,3), frequency = 365)
DIN_decomposed <- stl(DIN.ts, "periodic")
plot(DIN_decomposed)

#The four graphs are the original data, seasonal component, trend component and the remainder. 
#The bar at the right indicates the relative magnitudes of each component.

Chla.ts <- df$Chla
Chla.ts [is.na (Chla.ts)] <-0
Chla.ts <- ts(Chla.ts,start = c(2007,1), end = c(2018,3), frequency = 365)
Chla_decomposed <- stl(Chla.ts, "periodic")
plot(Chla_decomposed)

#maybe we just went to extract a component of the decompostion i.e. to compare the trends

#this can be done like this: 



Chla_trend <- stl(Chla.ts, s.window="periodic")$time.series[,"trend"]

DIN_trend <- stl (DIN.ts, s.window="periodic")$time.series[,"trend"]


par(mfrow=c(2,1))
plot(Chla_trend)
plot(DIN_trend)
par(mfrow=c(1,1))
#Exploring lagged relationships 

# maybe we want to determine if changes in one series whether one series preceeds changes in another

# This can be done through a cross-correlation function. 
# For example, let's investigate whether there is similarity between pairs of DIN and Chla observations as a function of the time seperation between them 

# First compute the sample crosscorrelation (covariance) function

Chla <- df$Chla
Chla [is.na (Chla)] <-0
DIN <- df$DIN
cf<- ccf(DIN, Chla)

#so it looks like covariance is always positive and is greatest between lag times (h) of around -5 and +5 
# But it's  difficult to read the lags exactly from the plot. 

# so let's view the output.  

cf
max(cf$acf)

#so the covariance is greatest (0.120645) where the lag is -6 - but similar correlations exist for lags of +5 and +6 too (for example)

#another way to view this is by making scatterplots of the lagged relationhip between DIN (lagged series) and Chla

lag2.plot(DIN, Chla, max.lag = 20, corr = T) # In each plot, observed Chla is on the vertical and a past lag of DIN is on the horizontal.  Correlation values are given on each plot.

alldata <- ts.intersect(rec,reclag1=lag(rec,-1), reclag2=lag(rec,-2), soilag5 = lag(soi,-5),
                     soilag6=lag(soi,-6), soilag7=lag(soi,-7), soilag8=lag(soi,-8), soilag9=lag(soi,-9),
                     soilag10=lag(soi,-10)) 

