## PRODUCE TRACE PLOTS FOR CALIBRATION
rm(list=ls())                        # Clear all previous variables
graphics.off()                       # clear plots


## get nice colors
mycol=rbind(
        c(0,0,0),
        c(0,73,73),
        c(0,146,146),
        c(255,109,182),
        c(255,182,119),
        c(73,0,146),
        c(0,109,219),
        c(182,109,255),
        c(109,182,255),
        c(182,219,255),
        c(146,0,0),
        c(146,73,0),
        c(219,109,0),
        c(36,255,36),
        c(255,255,109)
)
mycol=mycol/max(mycol)

mycol.rgb <- rep(0,nrow(mycol))
for (i in 1:nrow(mycol)) {
        mycol.rgb[i] <- rgb(mycol[i,1],mycol[i,2],mycol[i,3])
}


## mycol colors, by row:
# 1 black
# 2 dark blue
# 3 aqua
# 4 pink
# 5 light orange
# 6 purple
# 7 blue
# 8 light purple
# 9 light blue
# 10 very light blue
# 11 dark red
# 12 brown
# 13 dark orange
# 14 neon green
# 15 neon yellow

mycol_experts <- 5
mycol_standard <- 1
mycol_complete <- 9
mycol_priors <- 1


par(mfrow=c(2,4))

# Australia

bins_australia <- c(6,5,6,6,6,6,6,6)
PCPI_australia <- c(28.76,24.65,37.86,33.38,35.05,37.96,36.87,54.85)
PCPI_australia_mean <- mean(PCPI_australia,na.rm=TRUE)

plot(bins_australia,PCPI_australia,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[13],
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="Australian Capital Cities")
abline(h=PCPI_australia_mean,col=mycol.rgb[13]) # Australia Mean

# US

bins_US <- c(5,1.5,5,1.5,1.5,6,1.5,6,4,1.5,2,2,2)
PCPI_US <- c(28.8,21.2,21.7,20.5,21.8,30.1,31.4,18.8,37.3,20.3,24.1,20.8,39.3)
PCPI_US_mean <- mean(PCPI_US)

plot(bins_US,PCPI_US,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[7],
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="US Metropolitan Areas")
abline(h=PCPI_US_mean,col=mycol.rgb[7]) #US mean

#EU
bins_EU <- c(6,6,5,5,6,6,6,6,6,6,6,6)
PCPI_EU <- c(26.1,25.9,15.4,21.8,25.9,29.4,29.6,28.9,27.6,35.5,29.8,26.8)
PCPI_EU_mean <- mean(PCPI_EU,na.rm=TRUE)

plot(bins_EU,PCPI_EU,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="European Countries")
abline(h=PCPI_EU_mean,col=mycol.rgb[1])

#China
bins_China <- c(1.5,2,6,6,6)
PCPI_China <- c(13.6,13,17,23.8,27.1)
PCPI_China_mean <- mean(PCPI_China,na.rm=TRUE)

plot(bins_China,PCPI_China,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[11],
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="China Metropolitan Areas")
abline(h=PCPI_China_mean,col=mycol.rgb[11])

#South America
bins_SA <- c(1.5,1.5,1.5,1.5,4,2.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2,1.5,6,6)
PCPI_SA <- c(16.3,5.3,6,11.7,2,7.4,3.9,3.7,2.9,2,1.9,2.3,2.2,.3,19.8,14.6,18.8)
PCPI_SA_mean <- mean(PCPI_SA,na.rm=TRUE)

plot(bins_SA,PCPI_SA,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[5],
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="South American Metro Areas")
abline(h=PCPI_SA_mean,col=mycol.rgb[5])

# Indian and Pacific Islands
bins_islands <- c(4,1.5,4,1.5,6,6,4)
PCPI_islands <- c(2.39,4.8,5.198,3.8,9.9,10.2,10.1)
PCPI_islands_mean <- mean(PCPI_islands,na.rm=TRUE)

plot(bins_islands,PCPI_islands,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col="grey",
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="Indian and Pacicific Island Cities")
abline(h=PCPI_islands_mean,col="grey") # Africa Mean

#india and Sri Lanka
bins_india <- c(4,1.5,6)
PCPI_india <- c(2.5,2.8,1.3)
PCPI_india_mean <- mean(PCPI_india,na.rm=TRUE)

plot(bins_india,PCPI_india,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col="purple",
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="India (purple) and Sri Lanka (pink)")
abline(h=PCPI_india_mean,col="purple") # India Mean

bins_SL <- 1.5
PCPI_SL <- c(1.4)
PCPI_SL_mean <- 1.4

points(bins_SL,PCPI_SL, pch=19, col="pink")
abline(h=PCPI_SL_mean,col="pink")

# legend(x="topright",
#        legend=c("US 2010 Personal Income Per Capita"))

# Africa
bins_africa <- c(2,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2,2,1.5,1.5,1.5,1.5,2)
PCPI_africa <- c(5.4,4.5,2.2,1.8,6.9,1.5,2,.5,.4,.7,1.6,2.9,.46,.3,.89,.65,.3)
PCPI_africa_mean <- mean(PCPI_africa,na.rm=TRUE)

plot(bins_africa,PCPI_africa,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col="green",
     ylim=c(0,40),
     xlim=c(1.5,6),
     main="African Countries")
abline(h=PCPI_africa_mean,col="green") # Africa Mean





