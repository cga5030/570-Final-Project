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

layout(cbind(c(1,5,9),c(2,6,9),c(3,7,9),c(4,8,9)))
# par(mfrow=c(2,4),
   par(mai=c(.6,.6,.3,.2)) #c(bottom, left, top, right) )

# Australia

bins_australia <- c(6,5,6,6,6,6,6,6)
PCPI_australia <- c(28.76,24.65,37.86,33.38,35.05,37.96,36.87,54.85)
PCPI_australia_mean <- mean(PCPI_australia,na.rm=TRUE)


plot(NULL,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',        
     main="a) Australian Capital Cities")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_australia,y=PCPI_australia,
       pch=19,
       col=mycol.rgb[1],
       )
abline(h=PCPI_australia_mean,col=mycol.rgb[1]) # Australia Mean
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_australia <- bins_australia[bins_australia!= 6]
trigger_mean_australia <- mean(trigger_keep_australia)
abline(v=trigger_mean_australia,col=mycol.rgb[1], lty="dashed")


# US

bins_US <- c(5,1.5,5,1.5,1.5,6,1.5,2,4,1.5,2,2,2)
PCPI_US <- c(28.8,21.2,21.7,20.5,21.8,30.1,31.4,18.8,37.3,20.3,24.1,20.8,39.3)
PCPI_US_mean <- mean(PCPI_US)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="b) US Metropolitan Areas")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_US,y=PCPI_US,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_US_mean,col=mycol.rgb[1]) #US mean
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_US <- bins_US[bins_US!= 6]
trigger_mean_US <- mean(trigger_keep_US)
abline(v=trigger_mean_US,col=mycol.rgb[1], lty="dashed")

#EU
bins_EU <- c(6,6,5,5,6,6,6,6,6,6,6,6)
PCPI_EU <- c(26.1,25.9,15.4,21.8,25.9,29.4,29.6,28.9,27.6,35.5,29.8,26.8)
PCPI_EU_mean <- mean(PCPI_EU,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="c) European Countries")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_EU,y=PCPI_EU,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_EU_mean,col=mycol.rgb[1])
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_EU <- bins_EU[bins_EU!= 6]
trigger_mean_EU <- mean(trigger_keep_EU)
abline(v=trigger_mean_EU,col=mycol.rgb[1], lty="dashed")

#China
bins_China <- c(1.5,2,6,6,6)
PCPI_China <- c(13.6,13,17,23.8,27.1)
PCPI_China_mean <- mean(PCPI_China,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="d) China Metropolitan Areas")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_China,y=PCPI_China,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_China_mean,col=mycol.rgb[1])
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_China <- bins_China[bins_China!= 6]
trigger_mean_China <- mean(trigger_keep_China)
abline(v=trigger_mean_China,col=mycol.rgb[1], lty="dashed")


#South America
bins_SA <- c(1.5,1.5,1.5,1.5,4,2.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2,1.5,6,6)
PCPI_SA <- c(16.3,5.3,6,11.7,2,7.4,3.9,3.7,2.9,2,1.9,2.3,2.2,.3,19.8,14.6,18.8)
PCPI_SA_mean <- mean(PCPI_SA,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="e) South American Metro Areas")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_SA,y=PCPI_SA,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_SA_mean,col=mycol.rgb[1])
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_SA <- bins_SA[bins_SA!= 6]
trigger_mean_SA <- mean(trigger_keep_SA)
abline(v=trigger_mean_SA,col=mycol.rgb[1], lty="dashed")


# Indian and Pacific Islands
bins_islands <- c(4,1.5,4,1.5,6,6,4)
PCPI_islands <- c(2.39,4.8,5.198,3.8,9.9,10.2,10.1)
PCPI_islands_mean <- mean(PCPI_islands,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="f) Indian and Pacific Islands")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_islands,y=PCPI_islands,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_islands_mean,col=mycol.rgb[1]) # Africa Mean
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_islands <- bins_islands[bins_islands!= 6]
trigger_mean_islands <- mean(trigger_keep_islands)
abline(v=trigger_mean_islands,col=mycol.rgb[1], lty="dashed")

#india and Sri Lanka
bins_india <- c(4,1.5,6,1.5)
PCPI_india <- c(2.5,2.8,1.3,1.4)
PCPI_india_mean <- mean(PCPI_india,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="g) Indian and Sri Lankan Cities")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_india,y=PCPI_india,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_india_mean,col=mycol.rgb[1]) # India Mean

axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'));
trigger_keep_india <- bins_india[bins_india!= 6]
trigger_mean_india <- mean(trigger_keep_india)
abline(v=trigger_mean_india,col=mycol.rgb[1], lty="dashed")

# bins_SL <- 1.5
# PCPI_SL <- c(1.4)
# PCPI_SL_mean <- 1.4
# 
# points(bins_SL,PCPI_SL, pch=19, col=mycol.rgb[1]) # SL mean
# abline(h=PCPI_SL_mean,col=mycol.rgb[1], lty="dashed")

# legend(
#         x="topright",
#         legend=c("India","Sri Lanka"),
#         lty=c("solid","dashed"),
#         y.intersp=2
# )

# legend(x="topright",
#        legend=c("US 2010 Personal Income Per Capita"))

# Africa
bins_africa <- c(2,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,2,2,1.5,1.5,1.5,1.5,2)
PCPI_africa <- c(5.4,4.5,2.2,1.8,6.9,1.5,2,.5,.4,.7,1.6,2.9,.46,.3,.89,.65,.3)
PCPI_africa_mean <- mean(PCPI_africa,na.rm=TRUE)

plot(NULL,
     pch=19,
     xlab="Trigger Temperature [deg C]",
     ylab="Income Per Capita [Thousand USD]",
     col=mycol.rgb[1],
     ylim=c(0,40),
     xlim=c(1,6.5),
     xaxt='n',     xaxs='i',
     main="h) African Countries")
polygon(x=c(5.7,6.3,6.3,5.7),y=c(-1.5,-1.5,41.5,41.5),
        border=FALSE,col=rgb(mycol[15,1],mycol[15,2],mycol[15,3],.5)
)
points(x=bins_africa,y=PCPI_africa,
       pch=19,
       col=mycol.rgb[1],
)
abline(h=PCPI_africa_mean,col=mycol.rgb[1]) # Africa Mean
axis(1, seq(1,6,by=1), lab=c('1.0','2.0','3.0','4.0','5.0','None'))
trigger_keep_africa <- bins_africa[bins_africa!= 6]
trigger_mean_africa <- mean(trigger_keep_africa)
abline(v=trigger_mean_africa,col=mycol.rgb[1], lty="dashed")


plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,
       legend=c("mean income per capita","mean trigger temperature"),
       lty=c("solid","dashed"),
       horiz=TRUE,
       cex=1.2
       # bty='n'
       )





