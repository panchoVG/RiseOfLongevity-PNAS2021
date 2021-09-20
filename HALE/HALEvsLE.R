#############################################################################
#                                                                           #  
# THE UNCERTAINTY SURROUNDING HEALTHY LIFE EXPECTANCY INDICATORS            #
#                                                                           #
# Francisco VILLAVICENCIO, Marie-Pier BERGERON-BOUCHER,                     #
# and James W. VAUPEL                                                       #
#                                                                           #
#                                                                 Sept 2021 #
#############################################################################

############################################################
### DATA                                                 ###  
############################################################

#------#
# HALE #
#------#

# Healthy life expectancy (HALE) data from GBD 2019
HALE <- read.csv("IHME-GBD_2019_DATA-HALE.csv")
head(HALE)

# Select FEMALE DATA (Same as in Permanyer et al.)
femHALE <- HALE[HALE$sex == 'Female', ]
femHALE <- femHALE[(femHALE$location == 'Japan' & femHALE$year < 2016) |
                     (femHALE$location == 'Singapore' & femHALE$year > 2015), ]
femHALE <- droplevels(femHALE[, c('location', 'year', 'lower', 'val', 'upper')])
femHALE <- femHALE[order(femHALE$year), ]
rownames(femHALE) <- NULL
head(femHALE)

# Select MALE DATA (Same as in Permanyer et al.)
menHALE <- HALE[HALE$sex == 'Male', ]
menHALE <- menHALE[(menHALE$location == 'Japan' & menHALE$year < 2005) |
                     (menHALE$location == 'Singapore' & menHALE$year > 2004), ]
menHALE <- droplevels(menHALE[, c('location', 'year', 'lower', 'val', 'upper')])
menHALE <- menHALE[order(menHALE$year), ]
rownames(menHALE) <- NULL
head(menHALE)


#--------------------------#
# LIFE EXPECTANCY AT BIRTH #
#--------------------------#

# LIFE TABLES from GBD 2019
lt <- read.csv('IHME_GBD_2019_LIFE_TABLES_LE.csv')

# Select FEMALE DATA (Same as in Permanyer et al.)
femLE <- lt[lt$sex_name == 'female', ]
femLE <- femLE[femLE$location_name == 'Japan', ]
femLE <- droplevels(femLE[, c('location_name', 'year_id', 'lower', 'val', 'upper')])
names(femLE)[1:2] <- c('location', 'year')
femLE <- femLE[order(femLE$year), ]
rownames(femLE) <- NULL
head(femLE)

# Select MALE DATA (Same as in Permanyer et al.)
menLE <- lt[lt$sex_name == 'male', ]
menLE <- menLE[(menLE$location_name == 'Japan' & menLE$year_id %in% 1990:1998) |
                 (menLE$location_name == 'Iceland' & menLE$year_id %in% 1999:2006) |
                 (menLE$location_name == 'Switzerland' & menLE$year_id %in% 2007:2010) |
                 (menLE$location_name == 'Singapore' & menLE$year_id %in% 2011:2019), ]
menLE <- droplevels(menLE[, c('location_name', 'year_id', 'lower', 'val', 'upper')])
names(menLE)[1:2] <- c('location', 'year')
menLE <- menLE[order(menLE$year), ]
rownames(menLE) <- NULL
head(menLE)

# Remove unnecessary objects
rm(HALE, lt)


############################################################
### FIGURE 1                                             ###  
############################################################

# Years of interest
Years <- sort(unique(femHALE$year))

# Y-axis limit
yLim <- c(65, 90)

# Output pdf
PNG <- F
if (PNG) png(filename = 'Fig1-BPHLEandBPLE.png',
             width = 6, height = 4.5, units = 'in', res = 300)

# LAYOUT
layout(mat = rbind(c(1, 3, 4), 
                   c(0, 2, 2),
                   c(0, 5, 5)),
       widths = c(.1, 1, 1), heights = c(1, .08, .1))

# Y-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = "", ylab = "", col = NA, axes = F)
text(0.4, 0.5, "Age (years)", cex = 1.4, srt = 90)

# X-AXIS
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = "", ylab = "", col = NA, axes = F)
text(0.5, 0.25, "Year", cex = 1.4)

# Plot females and males on different panels
for (sex in c('Females', 'Males')) {
  
  # Data and Margins
  if (sex == 'Females') {
    dat1 <- femHALE
    dat2 <- femLE  
    marPlot <- c(1.3, 2, 1.8, 0.5)
    ylab <- seq(min(yLim), max(yLim), 5)
  } else {
    dat1 <- menHALE
    dat2 <- menLE  
    marPlot <- c(1.3, 1.5, 1.8, 1)
    ylab <- NA
  }
  
  # BPHLE: Color palette and points
  col1 <- rep('red', length(Years))
  col1[which(dat1$location == 'Iceland')] <- 'dodgerblue2'
  col1[which(dat1$location == 'Singapore')] <- 'darkgoldenrod'
  col1[which(dat1$location == 'Switzerland')] <- 'forestgreen'
  pch1 <- rep(2, length(Years))
  pch1[which(dat1$location == 'Iceland')] <- 1
  pch1[which(dat1$location == 'Singapore')] <- 0
  pch1[which(dat1$location == 'Switzerland')] <- 6
  
  # BPLE: Color palette and points
  col2 <- rep('red', length(Years))
  col2[which(dat2$location == 'Iceland')] <- 'dodgerblue2'
  col2[which(dat2$location == 'Singapore')] <- 'darkgoldenrod'
  col2[which(dat2$location == 'Switzerland')] <- 'forestgreen'
  pch2 <- rep(2, length(Years))
  pch2[which(dat2$location == 'Iceland')] <- 1
  pch2[which(dat2$location == 'Singapore')] <- 0
  pch2[which(dat2$location == 'Switzerland')] <- 6
  
  # Main plot
  par(mar = marPlot, las = 1)
  plot(NA, xlab = '', ylab = '', xlim = range(Years), bty = 'L', ylim = yLim,
       xaxt = 'n', yaxt = 'n')
  
  # Grid
  abline(h = seq(min(yLim), max(yLim), 5), lty = 2, lwd = .6, 
         col = grey(.8))
  
  # X-Axis
  axis(1, at = c(seq(min(Years), max(Years), 5)))
  axis(1, at = max(Years))
  
  # Y-Axis
  axis(2, at = min(yLim):max(yLim), labels = NA, tck = -.012)
  axis(2, at = seq(min(yLim), max(yLim), 5), labels = ylab)
  
  # BPHLE: Uncertainty
  segments(x0 = Years, y0 = dat1$lower, x1 = Years, y1 = dat1$upper,
           col = grey(.5), lwd = .8)

  # BPLE: Uncertainty
  segments(x0 = Years, y0 = dat2$lower, x1 = Years, y1 = dat2$upper,
           col = grey(.5), lwd = .8)

  # BPHLE: Data points
  points(Years, dat1$val, col = col1, pch = pch1)
  
  # BPLE: Data points
  points(Years, dat2$val, col = col2, pch = pch2)
  
  # Title
  title(bquote(italic(.(sex))), 
        adj = 0.02, line = .8, cex.main = 1.15)
  
  # Position of the text
  if (sex == 'Females') {
    pos1 <- 89
    pos2 <- 69.5
  } else {
    pos1 <- 84
    pos2 <- 68.2
  }
  
  # Text
  text(max(Years) - 3, pos1, labels = 'Best-practice life\nexpectancy (BPLE)', 
       cex = .9, adj = c(1, .5))
  text(max(Years)+.5, pos2, labels = 'Best-practice\nhealthy life\nexpectancy (BPHLE)', 
       cex = .9, adj = c(1, .5))
  
}

# LEGEND
par(mar = rep(0, 4))
plot(c(0, 1), c(0, 1), xlab = "", ylab = "", col = NA, axes = F)
legend('center', cex = 1.2,
       legend = c('Iceland', 'Japan', 'Singapore', 'Switzerland'),
       col = c('dodgerblue2', 'red', 'darkgoldenrod', 'forestgreen'),
       pch = c(1, 2, 0, 6), horiz = T, bty = 'n')

if (PNG) dev.off()


############################################################
### TABLE 1                                              ###
############################################################

# MONTE CARLO SIMULATION TO ESTIMATE UNCERTAINTY INTERVALS
CalcConfSim <- function(dat, niter) {
  
  ## dat    Data with mortality estimates and uncertainty intervals
  ## niter  Number of iterations      
  
  # Estimate standard deviation
  stdDev <- (dat$upper - dat$lower) / 4
  
  # Object to store estimated slopes
  slopes <- c()
  
  # Simulation
  for (i in 1:niter) {
    # Random sampling
    rand <- rnorm(n = nrow(dat), mean = dat$val, sd = stdDev)
    # Fit linear model
    fit <- lm(rand ~ dat$year)
    # Store estimated slope
    slopes <- c(slopes, coefficients(fit)[2])
  }
  
  # Median and uncertainty intervals of the slope
  return(CI = round(quantile(slopes, probs = c(0.025, .5, .975)), 3))
  
}

# Female BPHLE (Best-practice healthy life expectancy) SLOPE
femBPHLE <- CalcConfSim(dat = femHALE, niter = 20000)
femBPHLE

# Male BPHLE SLOPE
menBPHLE <- CalcConfSim(dat = menHALE, niter = 20000)
menBPHLE

# Female BPLE (Best-practice life expectancy) SLOPE
femBPLE <- CalcConfSim(dat = femLE, niter = 20000)
femBPLE

# Male BPLE SLOPE
menBPLE <- CalcConfSim(dat = menLE, niter = 20000)
menBPLE

# TABLE 1
Table1 <- cbind(c(paste0(femBPHLE[2], ' [', femBPHLE[1], '-', femBPHLE[3], ']'),
                  paste0(menBPHLE[2], ' [', menBPHLE[1], '-', menBPHLE[3], ']')),
                c(paste0(femBPLE[2], ' [', femBPLE[1], '-', femBPLE[3], ']'),
                  paste0(menBPLE[2], ' [', menBPLE[1], '-', menBPLE[3], ']')))
colnames(Table1) <- c('slopeBPHLE', 'slopeBPLE')
rownames(Table1) <- c('Females', 'Males')
Table1

