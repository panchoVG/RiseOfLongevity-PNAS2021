#############################################################################
#                                                                           #  
# DEMOGRAPHIC PERSPECTIVES ON THE RISE OF LONGEVITY                         #
# James W. VAUPEL - Francisco VILLAVICENCIO - Marie-Pier BERGERON-BOUCHER   #
#                                                                           #
# Proceedings of the National Academy of Sciences of USA (PNAS)             #
#                                                                           #
#                                                                           #
#                                                              January 2021 #
#############################################################################

# This R code reproduces all the Figures in the published article.
# All the data are available in the 'Data' subfolder and can be read with this R code.


###########################################################
### PACKAGES                                            ###  
###########################################################

# install.packages('ggplot2')
library(ggplot2)
# install.packages('RColorBrewer')
library(RColorBrewer)
# install.packages('tidyverse')
library(tidyverse)
# install.packages('lubridate')
library(lubridate)


###########################################################
### FIGURE 1. RATES OF MORTALITY IMPROVEMENT (RMI)      ###  
###########################################################

# The data for this figure comes from the Human Mortality Database.
# The code and figure were created by Marie-Pier Bergeron-Boucher.

# Clear workspace
rm(list = ls())

# Data
dtaRMI <- read.csv('Data/Fig1-RMI.csv')
head(dtaRMI) 

# The Rates of Mortality Improvement (RMI) are calculated for all ages,
#   but ploted for ages 80 to 100 only.
dtaP <- dtaRMI[dtaRMI$Age %in% c(80:100) & dtaRMI$Year > 1979, ]

# Plot
ggplot(dtaP, aes(x = Year, y = Age)) +
  geom_tile(aes(fill = RMI)) +
  scale_fill_gradient2(low = "red", high = "blue", mid="white") +
  facet_wrap(~Country) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.title = element_text(size = 12, vjust = 0.9),
        legend.text = element_text(size = 12),
        legend.text.align = 0.5, 
        legend.position = "bottom",
        legend.key.width = unit(1.3, "cm"))

# Save figure in output files
# ggsave("Figures/Figure1_RMI.pdf", height = 7, width = 7)
# ggsave("Figures/Figure1_RMI.png", height = 7, width = 7)


###########################################################
### FIGURE 2. BEST-PRACTICE LIFE EXPECTANCY             ###  
###########################################################

# This figure is an adaptation of the original figure by Oeppen and Vaupel (2002) 
#   published in Science, using the most recent data of the Human Mortality Database.
# The code and figure were created by Marie-Pier Bergeron-Boucher and 
#   Francisco Villavicencio, with support from Jim Oeppen.

# Clear workspace
rm(list = ls())

# Data on best practice life expectancy
bestPractice <- read.table("Data/bestPracticeFullNames.txt", header = T)
head(bestPractice)
tail(bestPractice)

# Best practice regression line
fit <- lm(as.numeric(bestPractice$e0f) ~ as.numeric(bestPractice$Year))
r2 <- round(summary(fit)$r.squared, 3)
r2
BPfit <- data.frame(Year = bestPractice$Year,
                    e0 = predict(fit),
                    Country = "Best-practice linear trend")

# Data on France
e0FRA <- read.table("Data/e0FRA.txt", skip = 2, header = T)
e0FRA <- e0FRA[e0FRA$Year > 1840, ]
e0FRA$Country <- "French females"

# Data on USA
e0USA <- read.table("Data/e0USA.txt", skip = 2, header = T)
e0USA$Country <- "US females"

# Data on Hong Kong
e0HK <- read.table("Data/e0HK.txt", skip = 2, header = T)
e0HK$Country <- "Hong Kong females"

# Color palette
mycol <- brewer.pal(7, "Dark2")[c(2, 1, 3:7)]

# Plot
ggplot() +
  geom_point(data = bestPractice, aes(x = Year, y = e0f, 
                                      shape = Country, color = Country, linetype = Country)) +
  geom_line(data = e0FRA, aes(x = Year, y = Female,  
                              shape = Country, color = Country, linetype = Country)) +
  geom_line(data = e0USA, aes(x = Year, y = Female,  
                              shape = Country, color = Country, linetype = Country)) +
  geom_line(data = e0HK, aes(x = Year, y = Female,  
                             shape = Country, color = Country, linetype = Country)) +
  geom_line(data = BPfit, aes(x = Year, y = e0,
                              shape = Country, color = Country, linetype = Country)) +
  geom_text(aes(x = 1935, y = 40, label = "Best-practice regression line"), 
            hjust = 0, size = 4) +
  geom_text(aes(x = 1935, y = 38,label = paste("Slope =", round(fit$coefficients[2], 2))),
            hjust = 0, size = 4) +
  geom_text(aes(x = 1935, y = 36, label = "R^2==0.988"),
            parse = T, hjust = 0, size = 4) +
  scale_shape_manual(values = c(7, NA, 8, NA, NA, 0, 1, 2, 6, 10, NA), 
                     name = "", breaks = levels(bestPractice$Country)) +
  scale_linetype_manual(values = c(NA, 1, NA, 1, 1, NA, NA, NA, NA, NA, 1), 
                        name = "", breaks = levels(bestPractice$Country)) +
  scale_color_manual(values = c(mycol[1], "black", mycol[2], "dodgerblue", "forestgreen",
                                mycol[3:7], "red"), 
                     name = "", breaks = levels(bestPractice$Country)) +
  ylab("Life expectancy at birth") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 11, vjust = 0.9),
        legend.text = element_text(size = 11),
        legend.position = c(.22, .8))

# Save figure in output files
# ggsave("Figures/Figure2_BestPractice.pdf", height = 6, width = 6)
# ggsave("Figures/Figure2_BestPractice.png", height = 6, width = 6)


###########################################################
### FIGURE 3. LIFE EXPECTANCY VS LIFE SPAN EQUALITY     ###  
###########################################################

# The figure is an adaptation of the original figure by Colchero et al. (2016)
#   published in PNAS, using the most recent data.
# The data and R code were provided by Fernando Colchero and updated by
#   Marie-Pier Bergeron-Boucher and Francisco Villavicencio.

# Clear workspace
rm(list = ls())

# Load data
taball <- read.csv('Data/Fig3-Data.csv')

# Plotting settings
inch <- 0.0393701
wdt <- c(89, 183, 247) * inch
wdt <- c(3.42, 4.5, 7)

# Linear regression
x <- taball$e0.f[as.character(taball$Species) == "Human"]
y <- taball$s0.f[as.character(taball$Species) == "Human"]
l1 <- lm(y ~ x)$coefficients
x1 <- range(x) + c(0, 1)
y1 <- x1 * l1[2] + l1[1]

# Define color category
taball$Type <- "Other 2013"
taball$Type[taball$Country == "Sweden"] <- "Sweden"
taball$Type[taball$Species != "Human"] <- "Non-human primates"
taball$Type[taball$Country %in% c("Iceland", "Trinidad", "Ukraine", "Liberia")] <- "High mortality"
taball$Type[taball$Country %in% c("Hadza", "Ache", "Acculturated")] <- "Hunter gatherers"
taball$Type[taball$Country == "England"] <- "England"

# Define labels
taball$label <- as.character(taball$Country)
taball$label[taball$Country == "Sweden"] <- as.character(taball$Period[taball$Country == "Sweden"])
taball$label[taball$Type == "Non-human primates"] <- as.character(taball$Species[taball$Type == "Non-human primates"])
taball$label[taball$Type == "High mortality"] <- paste(as.character(taball$Country[taball$Type == "High mortality"]), 
                                                       as.character(taball$Period[taball$Type == "High mortality"]))
taball$label[taball$label == "1773-1773"] <- "1773"
taball$label[taball$label == "England"] <- paste(as.character(taball$Country[taball$Country == "England"]), 
                                                 as.character(taball$Period[taball$Country == "England"]))
taball$label[taball$label == "Iceland 1882-1882"] <- "Iceland 1882"

# Label position
taball2 <- taball[order(taball$Type), ]
taball2$textx <- taball2$e0.f + c(
  16,
  15, 15, 12, 9,
  5, 4.5, 7.5,
  -5, -2, -7, -6, 0, -5,
  4, 6, 5, 4, 5.5, 5,  6, 
  -10, -7, -9, -11, -7, -8, -4, -10, -3
)
taball2$texty <- taball2$s0.f + c(
  0.06,
  -0.06,-0.08, 0, 0,
  0,-0.04, -0.05,
  0,-0.1, 0, 0,-0.05, 0,
  0, 0, -0.07, -0.02, -0.02,-0.02,-0.08,
  0.06,0.09,0.02,0.02,0,0,0,0, 0.05
)

# Color palette
mycol<-c("sienna4", "#FC4E2A", "#7FBC41", "#810F7C", "orange", "#4292C6")

#Plot
ggplot() +
  geom_line(aes(x = x1, y = y1), col = "black", size = 2) +
  geom_segment(data = taball2, aes(x = e0.m, xend = e0.f, y = s0.m, yend = s0.f, color = Type)) +
  geom_point(data = taball2, aes(x = e0.f, y = s0.f, color = Type, shape = Type), size = 4) +
  geom_text(data  =  taball2, aes(x  =  textx, y  =  texty, label  =  label, color  =  Type), size  =  4) +
  geom_text(aes(x = 40, y = -0.7, label = "High\nmortality"), size = 6, col = mycol[2], fontface = "bold") +
  geom_text(aes(x = 55, y = 0.1, label = "Hunter\ngatherers"), size = 6, col = mycol[3], fontface = "bold") +
  geom_text(aes(x = 10, y = 0.7, label = "Non-human\nprimates"), size = 6, col = mycol[4], fontface = "bold") +
  geom_text(aes(x = 88, y = 1.4, label = "Other\n2016-2017"), size = 6, col = mycol[5], fontface = "bold") +
  geom_text(aes(x = 50, y = 1.7, label = "Sweden"), size = 6, col = mycol[6], fontface = "bold") +
  scale_shape_manual(values =  c(16, 15, 16, 8, 17, 18)) +
  scale_color_manual(values = mycol) +
  ylab("Lifespan equality") +
  xlab("Life expectancy at birth") +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13, vjust = 0.9),
        legend.text = element_text(size = 13),
        legend.position = "none")

# Save figure in output files
# ggsave("Figures/Figure3_LifespanIneqalities.pdf", height = 7, width = 7)
# ggsave("Figures/Figure3_LifespanIneqalities.png", height = 7, width = 7)


###########################################################
### FIGURE 4. AGE OF WORLD'S OLDEST PERSON              ###  
###########################################################

# The figure is an adaptation of a figure by Jonas Schoeley, 
#   inspired by a graph by Robert D. Young.
# Data extracted from the Gerontology Research Group website.
# The data and R code were provided by Jonas Schoeley and updated by
#   Marie-Pier Bergeron-Boucher and Francisco Villavicencio.

# Clear workspace
rm(list = ls())

# Data: Oldest living by year
old <-read_csv('Data/Fig4-grg_oldest_living_person_by_year.csv') %>%
  mutate(date_of_accession = dob +
         years(age_at_accession_y) +
         days(age_at_accession_d))

# Scale
verScale <- 365.25

# Plot
old %>%
  ggplot() +
  geom_smooth(method = "lm", se = FALSE, size = 1, color = "black",
              aes(x = date_of_accession,
                  y = age_at_accession_y*verScale + age_at_accession_d)) +
  geom_segment(aes(
    x = date_of_accession, xend = dod,
    y = age_at_accession_y*verScale + age_at_accession_d,
    yend = age_at_death_y*verScale + age_at_death_d, color = sex)
  ) +
  geom_point(aes(
    x = dod, y = age_at_death_y*verScale + age_at_death_d, color = sex), size = 1
  ) +
  geom_text(aes(x = date_of_accession[30], 
                y = 40500, 
                label = "Regression line:"), 
            hjust = 0) +
  geom_text(aes(x = date_of_accession[30], 
                y = 40300, label = "Slope = 0.12"),
            hjust = 0) +
  geom_text(aes(x = date_of_accession[30], 
                y = 40100, label = "R^2==0.860"), 
            parse = T, hjust = 0) +
  scale_y_continuous("Age", breaks = c(seq(110, 120, 5), 122)*verScale,
                     limits = c(107, 123)*verScale,
                     minor_breaks = seq(105, 125, 1)*verScale,
                     labels = function (x) x %/% verScale,
                     expand = c(0,0)) +
  scale_x_date(expand = c(0.01, 0.01)) +
  xlab("Year") +
  scale_color_manual(values = c("#F8766D" ,"#00B3FF"), name="",
                     labels = c("Females", "Males")) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 13, vjust = 0.9),
        legend.text = element_text(size = 13),
        legend.position = c(.15, .9))

# Save figure in output files
# ggsave("Figures/Figure4_OldestLiving.pdf", height = 5.5, width = 7)
# ggsave("Figures/Figure4_OldestLiving.png", height = 5.5, width = 7)

# Linear regression on the age of accession 
old <-
  old %>%
  mutate(date_of_accession_centered_y = (date_of_accession - min(date_of_accession, na.rm = TRUE)) / verScale,
         age_at_accession_centered_y = (age_at_accession_y*verScale + age_at_accession_d) / verScale)
model_fit <- lm(data = old, formula = age_at_accession_centered_y ~ date_of_accession_centered_y)

# Mean annual increase of accession age
coef(model_fit)[2]
# R-squared
round(summary(model_fit)$r.squared, 3)


###########################################################
### FIGURE 5: FORECASTS FOR FEMALES                     ###  
###########################################################

# The forecast are calculated using data from the Human Mortality Database.
# The official national forecast data were extracted from National Statistical Offices
#   (see references in the main manuscript).
# The code and figure were created by Marie-Pier Bergeron-Boucher.

# Clear workspace
rm(list = ls())

# Forecast data
dtaP <- read.csv('Data/Fig5-dtaP.csv')
head(dtaP) 

# Observed data
dtaObs <- read.csv('Data/Fig5-dtaObs.csv')
head(dtaObs)

# Best-practice data
dtaBP <- read.csv('Data/Fig5-dtaBP.csv')
head(dtaBP)

# Data for text 
dta_short <- dtaP[dtaP$Year == 2070, !(colnames(dtaP) %in% c("Model", "Year"))]
dta_short[is.na(dta_short)] <- dtaP$exF[dtaP$Year == 2065 & 
                                          dtaP$Country == "Japan" & dtaP$Model == "Official"]
dtaText <- dta_short %>%
  group_by(Country) %>%
  summarise(Max = max(exF),
            Min = min(exF),
            Diff = max(exF) - min(exF))

# Parameters
mycol <- brewer.pal(7, "Paired")
mylty <- rep(c(1, 4), 4)
legend_ord <- levels(dtaP$Model)
modelLabels <- c("Lee-Carter", 
                 "Li-Lee",
                 "CoDA", 
                 "CoDA-coherent",
                 expression(e[o]*" extrapolation"),
                 "Double-gap",
                 "Official",
                 "Best-practice")

# Plot
ggplot() +
  geom_line(data = dtaObs, aes(x = Year, y = exF, group = Country)) +
  geom_line(data = dtaP, aes(x = Year, y = exF, group = Model, color = Model, 
                             linetype = Model, size=Model)) +
  geom_line(data = dtaBP, aes(x = Year, y = ex, linetype = "BP", color = "BP", size = "BP")) +
  geom_text(data = dtaText, aes(x = 2072, y = Max, label = paste(round(Max, 1))), hjust = 0) +
  geom_text(data = dtaText, aes(x = 2072, y = Min, label = paste(round(Min, 1))), hjust = 0) +
  geom_text(data = dtaText, aes(x = 2072, y = Min-3.5, label = paste(round(Diff, 1))), hjust = 0) +
  geom_segment(data = dtaText, aes(x = 2072, xend = 2085, y = Min-2, yend = Min-2)) +
  facet_wrap(~ Country) +
  ylab("Life expectancy at birth") +
  scale_color_manual(name = "", 
                     values = c(BP = "darkgrey", 
                                "Lee-Carter" = mycol[1],
                                "Li-Lee" = mycol[2],
                                "CoDA" = mycol[3], 
                                "CoDA-Coherent" = mycol[4],
                                "e(0) extrapolation" = mycol[5],
                                "Double-Gap" = mycol[6],
                                "Official" = "darkorange"),
                     breaks = c(legend_ord, "BP"),
                     labels = modelLabels) +
  scale_linetype_manual(name = "", 
                        values = c(BP = 2, 
                                   "Lee-Carter" = 1,
                                   "Li-Lee" = 1,
                                   "CoDA" = 1,
                                   "CoDA-Coherent" = 1,
                                   "e(0) extrapolation" = 1,
                                   "Double-Gap" = 1,
                                   "Official" = 1),
                        breaks = c(legend_ord, "BP"),
                        labels = modelLabels) +
  scale_size_manual(name = "", 
                    values = c(BP = 1.3, 
                               "Lee-Carter" = 1.3,
                               "Li-Lee" = 1,
                               "CoDA" = 1.3,
                               "CoDA-Coherent" = 1,
                               "e(0) extrapolation" = 1.3,
                               "Double-Gap" = 1,
                               "Official" = 1),
                    breaks = c(legend_ord, "BP"),
                    labels = modelLabels) +
  scale_x_continuous(breaks = c(1965, 2017, 2070),
                     limits = c(1965, 2090)) +
  theme_minimal() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.title = element_text(size = 12, vjust = 0.9),
        legend.text = element_text(size = 12),
        legend.text.align = 0, 
        legend.position = "bottom",
        legend.key.width = unit(1.3,"cm"),
        panel.spacing.x = unit(1, "lines")) +
  guides(col = guide_legend(ncol = 3))

# Save figure in output files
# ggsave("Figures/Figure5_Forecast.pdf", height = 7, width = 6)
# ggsave("Figures/Figure5_Forecast.png", height = 7, width = 6)
