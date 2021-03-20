# Load packages
library(reshape2) # melt function
library(dplyr)  # data manuplation function
library(ggplot2)  # ggplot visualization
library(ggalt)  # geom_dumbbell
library(RColorBrewer) # color selection
library(colorspace) # color selection
library(margins)  # margins/cplot
library(lmtest) # bptest()
library(stargazer)  # stargazer()
library(sandwich) # vcovHC()
library(scales) # comma

# Set up the working directory
# setwd("C:\\Users\\")

# Clean data in the global environment
rm(list = ls())

# 1. Load data ------------------------------------------------------------------------------------------
# Projection data set ------------------------------------------------------------------------------------------
# Climate projection data source: KNMI website
# prec: GCM: CMIP5, monthly, world, rcp45/65/85
# tmax: GCM: CMIP5, monthly, world, rcp45/65/85
# dryshell (maximum length of dry spell, CDD): GCM: CMIP5 extremes (one ensemble member), monthly, world, rcp45/65/85
DF_RCP_weather_raw <- read.csv("Climate_Projection_Change_Rate.csv")
colnames(DF_RCP_weather_raw)[1] <- "Year"

# CO2 projection data set
DF_RCP_raw <- read.csv("DF_RCP_Projection.csv")

# Rice production (Unit: tonnes)
DF_Rice_production_raw <- read.csv("DF_Rice_production.csv")

# Main analysis data set ------------------------------------------------------------------------------------------
DF_Analysis_raw <- read.csv("DF_Analysis.csv")
DF_Analysis_Tech_raw <- read.csv("DF_Analysis_Tech.csv")

# Delete the first unneeded column
DF_Analysis <- DF_Analysis_raw[,-1]
DF_Analysis_Tech <- DF_Analysis_Tech_raw[,-1]

# Available duration by country
DF_Analysis %>% 
  filter(Asia == 1) %>%
  group_by(Country) %>% 
  summarise(Start = min(Year), End = max(Year)) %>%
  data.frame()

# Rename country name
DF_Analysis$Country <- as.character(DF_Analysis$Country)
DF_Analysis[DF_Analysis$Country=="Hong_Kong", "Country"] <- "Hong Kong"
DF_Analysis[DF_Analysis$Country=="South_Korea", "Country"] <- "South Korea"
DF_Analysis[DF_Analysis$Country=="Sri_Lanka", "Country"] <- "Sri Lanka"
DF_Analysis[DF_Analysis$Country=="Saudi_Arabia", "Country"] <- "Saudi Arabia"
DF_Analysis[DF_Analysis$Country=="North_Korea", "Country"] <- "North Korea"

DF_Analysis_Tech$Country <- as.character(DF_Analysis_Tech$Country)
DF_Analysis_Tech[DF_Analysis_Tech$Country=="Hong_Kong", "Country"] <- "Hong Kong"
DF_Analysis_Tech[DF_Analysis_Tech$Country=="South_Korea", "Country"] <- "South Korea"
DF_Analysis_Tech[DF_Analysis_Tech$Country=="Sri_Lanka", "Country"] <- "Sri Lanka"

DF_Rice_production <- DF_Rice_production_raw
DF_Rice_production$Country <- as.character(DF_Rice_production$Country)

# Create dummy variable for China
DF_Analysis$China <- ifelse(DF_Analysis$Country == "China", 1, 0)
DF_Analysis_Tech$China <- ifelse(DF_Analysis_Tech$Country == "China", 1, 0)

# Create interaction term between China and FACE dummy
DF_Analysis$FACE_China <- DF_Analysis$China*DF_Analysis$FACE_dummy
DF_Analysis_Tech$FACE_China <- DF_Analysis_Tech$China*DF_Analysis_Tech$FACE_dummy

# Convert the unit of yields from "hectogram/hectare" to "ton/hectare"
DF_Analysis$Yields <- DF_Analysis$Yields/10000
DF_Analysis_Tech$Yields <- DF_Analysis_Tech$Yields/10000

# Extract observations in Asia 
DF_Analysis_Asia <- DF_Analysis[DF_Analysis$Asia==1, ]
DF_Analysis_Asia_NOFACE <- DF_Analysis[DF_Analysis$Asia==1 & DF_Analysis$FACE_dummy!=1, ]
DF_Analysis_Tech_Asia <- DF_Analysis_Tech[DF_Analysis_Tech$Asia==1, ]
DF_Analysis_Tech_Asia_NOFACE <- DF_Analysis_Tech[DF_Analysis_Tech$Asia==1 & DF_Analysis_Tech$FACE_dummy!=1, ]

# Convert the country column as a character object
DF_Analysis_Asia_NOFACE$Country <- as.character(DF_Analysis_Asia_NOFACE$Country)
DF_Analysis_Tech_Asia_NOFACE$Country <- as.character(DF_Analysis_Tech_Asia_NOFACE$Country)

# Set the factor level for the region 
DF_Analysis_Asia$Region <- factor(DF_Analysis_Asia$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                             labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))
DF_Analysis_Tech_Asia$Region <- factor(DF_Analysis_Tech_Asia$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                                  labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))
DF_Analysis_Asia_NOFACE$Region <- factor(DF_Analysis_Asia_NOFACE$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                                  labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))
DF_Analysis_Tech_Asia_NOFACE$Region <- factor(DF_Analysis_Tech_Asia_NOFACE$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                                              labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))

# Create lagged time trend ------------------------------------------------------------------------------------------
# Function for extracting lagged terms
tlag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}

# Add AGRD lagged terms
DF_Analysis_lagged <- DF_Analysis_Tech_Asia %>% 
  group_by(Country) %>% 
  mutate(RDGDP_1yr = tlag(RDGDP, 1, time = Year), 
         RDGDP_5yr = tlag(RDGDP, 5, time = Year), 
         RDGDP_6yr = tlag(RDGDP, 6, time = Year),
         RDGDP_10yr = tlag(RDGDP, 10, time = Year),
         RDGDP_15yr = tlag(RDGDP, 15, time = Year)) %>% data.frame()

# Extract observations in Asia 
DF_Analysis_lagged_Asia_NOFACE <- DF_Analysis_lagged[DF_Analysis_lagged$FACE_dummy!=1, ]

# 2. Exploration data ------------------------------------------------------------------------------------------
# 2.1 Summary statistics ------------------------------------------------------------------------------------------
DF_Summary_stat <- DF_Analysis_Asia %>% select("Yields","Prec","Tmax","SPEI","CO2","IrrRate")

# Summarize output 
stargazer(DF_Summary_stat, type = "text",
          header = F, digits=2, no.space=TRUE, omit.table.layout=c("l"),  
          #out = "Summary_Stat_Tech.txt", 
          omit.summary.stat = c("p25","p75"),
          covariate.labels=c("Yield (ton/hectare)", "Precipitation (mm)", "Mean temperature (Celsius)", 
                             "SPEI (-: drought, +: wet)","CO2 concentration (ppm)", "Irrigation rate"))

# Correlation between Trend and CO2
cor(DF_Analysis_Asia$CO2,DF_Analysis_Asia$Trend)
cor(DF_Analysis_Asia_NOFACE$CO2,DF_Analysis_Asia_NOFACE$Trend)

# Correlation between AGRD and CO2
cor(DF_Analysis_Tech_Asia$CO2,DF_Analysis_Tech_Asia$RDGDP)
cor(DF_Analysis_Tech_Asia_NOFACE$CO2,DF_Analysis_Tech_Asia_NOFACE$RDGDP)

# Correlation between AGRD and yields (For the Discussion section in the paper)
cor(DF_Analysis_lagged_Asia_NOFACE$Yields, DF_Analysis_lagged_Asia_NOFACE$RDGDP)
cor(DF_Analysis_lagged_Asia_NOFACE$Yields, DF_Analysis_lagged_Asia_NOFACE$RDGDP_1yr, use = "pairwise.complete.obs")
cor(DF_Analysis_lagged_Asia_NOFACE$Yields, DF_Analysis_lagged_Asia_NOFACE$RDGDP_5yr, use = "pairwise.complete.obs")
cor(DF_Analysis_lagged_Asia_NOFACE$Yields, DF_Analysis_lagged_Asia_NOFACE$RDGDP_10yr, use = "pairwise.complete.obs")
cor(DF_Analysis_lagged_Asia_NOFACE$Yields, DF_Analysis_lagged_Asia_NOFACE$RDGDP_15yr, use = "pairwise.complete.obs")

# 2.2 Visualize key variables: rice yields, AGRD, CO2 ------------------------------------------------------------------------------------------
# Country-level rice yield trend (Figure S1)
DF_plot_rice_yield_Country <- DF_Analysis_Asia_NOFACE %>% select(Region, Country, Year, Yields) %>% arrange(Region, Country)
DF_plot_rice_yield_Country$Country <- factor(DF_plot_rice_yield_Country$Country, 
                                             levels = c(unique(DF_plot_rice_yield_Country$Country)))
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=Yields, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Rice Yield (ton/hectare)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Region-level rice yield trend
DF_Analysis_Asia_NOFACE %>%
  group_by(Region,Year) %>%
  summarize(Yields = mean(Yields)) %>%
  ggplot(aes(x=Year, y=Yields, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Rice Yield (ton/hectare)") +
  scale_x_continuous(breaks = seq(1960,2015, 5)) +
  scale_y_continuous(label=comma) +
  scale_color_discrete(name = "Region", labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.85), legend.text = element_text(size=20), 
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Rice yield growth rate over 1992 (earliest year available to Central Asia) to 2016
DF_Analysis_Asia_NOFACE %>%
  group_by(Year) %>%
  summarize(Yields = mean(Yields)) %>%
  filter(Year %in% c(1992, 2016)) %>% 
  data.frame()

# CO2 trend during 1960 - 2016 (Figure 1a)
DF_Analysis_Asia_NOFACE %>%
  group_by(Year) %>%
  summarize(CO2 = mean(CO2)) %>%
  ggplot(aes(x=Year, y=CO2)) +
  geom_line(size=1.5, color="red", alpha = 0.6) +
  labs(x = "Year", y = "CO2 (ppm)") +
  scale_x_continuous(breaks = seq(1960,2015, 5)) +
  scale_y_continuous(label=comma) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.8), legend.text = element_text(size=28, face="bold"), text = element_text(size=28))

# Projected CO2 trajectory
melt(DF_RCP_raw, id.vars = "Year") %>%
  ggplot(aes(x=Year, y= value, color=factor(variable))) + 
  geom_line(size=1.2) +
  theme_bw() 

# Temperature trend by regions
DF_Analysis_Asia %>%
  group_by(Region,Year) %>%
  summarize(Tmax = mean(Tmax)) %>%
  ggplot(aes(x=Year, y=Tmax)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Mean max. temperature (°C)") +
  facet_wrap(~ Region, scales = "free", nrow = 1) +
  geom_smooth(aes(x = Year, y = Tmax), method = "lm", size = 1.2, color = "red") +
  theme_bw() +
  theme(legend.position = "none", legend.text = element_text(size=20), 
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# AGRD trend by country 
DF_Analysis_Tech_Asia_NOFACE %>%
  group_by(Country,Year) %>%
  summarize(RDGDP = mean(RDGDP), Region) %>%
  ggplot(aes(x=Year, y=RDGDP, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "AGRD expenditure (million USD)") +
  scale_x_continuous(breaks = seq(1980,2020, 10)) +
  facet_wrap(~ Country, scales = "free") +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), 
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# AGRD trend by regions
DF_Analysis_Tech_Asia_NOFACE %>%
  group_by(Region,Year) %>%
  summarize(RDGDP = mean(RDGDP)) %>%
  ggplot(aes(x=Year, y=RDGDP)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "AGRD expenditure (million USD)") +
  scale_x_continuous(breaks = seq(1980,2020, 10)) +
  facet_wrap(~ Region, scales = "free", nrow = 1) +
  geom_smooth(aes(x = Year, y = RDGDP), method = "lm", size = 1.2, color = "red") +
  theme_bw() +
  theme(legend.position = "none", legend.text = element_text(size=20), 
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Scatterplot between R&D expenditure and rice yields
ggplot(DF_Analysis_Tech_Asia_NOFACE, aes(x=RDGDP, y=Yields)) +
  geom_point(aes(colour=factor(Country)), size=2.5) + 
  labs(x = "R&D expenditure (million USD)", y = "Yield (ton/hectare)") +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size=16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Scatterplot between R&D expenditure and rice yields (excl China, Uzbekistan)
ggplot(DF_Analysis_Tech_Asia_NOFACE[DF_Analysis_Tech_Asia_NOFACE$Country!="China" & 
                                      DF_Analysis_Tech_Asia_NOFACE$Country!="Uzbekistan",], aes(x=RDGDP, y=Yields)) +
  geom_point(aes(colour=factor(Country)), size=2.5) + 
  labs(x = "R&D expenditure (million USD)", y = "Yield (ton/hectare)") +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size=16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# 2.3 Close look at rice yield, CO2, and FACE obs ------------------------------------------------------------------------------------------
# Scatterplot between CO2 and rice yields
ggplot(DF_Analysis_Tech_Asia, aes(x=CO2, y=Yields)) +
  geom_point(aes(colour=factor(FACE_dummy)), size=2.5) + 
  labs(x = "CO2 (ppm)", y = "Yield (ton/hectare)", color = "FACE (1 indicates FACE obs.)") +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size=16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Create a dataframe for scatter plot
DF_Scatter <- DF_Analysis_Asia %>% select("Country", "Yields", "CO2", "FACE_dummy")

# Create dummies to indicate FACE data and major countries
DF_Scatter$Group <- ifelse(DF_Scatter$Country != "China" & DF_Scatter$Country != "Japan", "Other Asian countries", 
                           ifelse(DF_Scatter$Country == "China", "China", "Japan"))
DF_Scatter$Group <- factor(DF_Scatter$Group, levels = c("China", "Japan", "Other Asian countries"))

# Combine three condition datasets
Scatter_Yield0 <- DF_Scatter %>% filter(FACE_dummy == 0) %>% data.frame()
Scatter_Yield0$Panel <- "Non FACE"

Scatter_Yield1 <- DF_Scatter %>% filter(FACE_dummy == 1) %>% data.frame()
Scatter_Yield1$Panel <- "FACE"

DF_Scatter$Panel <- "All"

Scatter_Yield <- rbind.data.frame(DF_Scatter, Scatter_Yield0, Scatter_Yield1)
Scatter_Yield$Panel <- factor(Scatter_Yield$Panel, levels = c("All", "Non FACE", "FACE"))

# Scatter plot: Rice yield v.s. CO2
Scatter_Yield %>%
  ggplot(aes(x=CO2, y=Yields)) +
  geom_point(aes(colour=Group), size=3, alpha =0.8) + 
  geom_smooth(aes(x=CO2, y=Yields), method = "lm", size = 1.2, color = "red") +
  labs(x = "CO2 (ppm)", y = "Yield (ton/hectare)", colour = "") +
  facet_wrap(~Panel, nrow=1) +
  theme_bw() +
  theme(legend.position="top", text = element_text(size=28), 
        legend.text = element_text(size=20), legend.title = element_blank()) + 
  guides(colour = guide_legend(keywidth = 3))


# 3. Estimation ------------------------------------------------------------------------------------------
# Create a function producing OLS+HC and FGLS
Fn_FGLS <- function(Spec, DF_input){
  
  # Create formula objects
  S1_form1 <- as.formula(paste("log(Yields)", Spec, sep = " ~ "))
  S2_form1 <- as.formula(paste("log(residuals(JP1)^2)", Spec, sep = " ~ "))
  
  # Three step FGLS
  JP1 <- lm(S1_form1, data = DF_input)
  JP2 <- lm(S2_form1, data = DF_input)
  JP3 <- lm(S1_form1, weights=1/(exp(fitted(JP2))), data = DF_input)
  
  # Testing for heteroskedasticity (Breusch-Pagan test: null is the absence of heteroskedasticity )
  bptest <- round(bptest(JP1)$p.value,4)
  JP1_vcovHC <- vcovHC(JP1)
  OLS_HC <- coeftest(JP1, vcov. = JP1_vcovHC)
  
  # Output
  List_result <- list(
    
    # Regression results
    FGLS1 = JP1, 
    FGLS2 = JP2, 
    FGLS3 = JP3, 
    
    FGLS1_vcovHC = JP1_vcovHC,
    OLS_HC = OLS_HC,
    
    # Breusch-Pagan test
    BPtest = bptest,
    
    # Number of countries
    Num_country = length(unique(DF_input$Country)))
  
  return(List_result)
}

# 3.1 Time trend model (Used in the paper) ------------------------------------------------------------------------------------------
# Model specifications: Trend (log-log)
Spec00_trend <- paste(c("log(Prec)","log(Tmax)","log(CO2)*SPEI"), collapse = " + ")
Spec0_trend <- paste(c("log(Prec)","log(Tmax)","log(CO2)*SPEI","IrrRate","Trend","I(Trend^2)","FACE_dummy","FACE_China"), collapse = " + ")
Spec1_trend <- paste(c("log(Prec)","log(Tmax)","SPEI","IrrRate","Trend","I(Trend^2)",
                       "FACE_dummy*FACE_China","factor(Country)","log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), collapse = " + ")
Spec2_trend <- paste(c("log(Prec)","log(Tmax)","log(CO2)*SPEI","IrrRate","Trend","I(Trend^2)",
                       "FACE_dummy*FACE_China","factor(Country)","log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), collapse = " + ")
Spec3_trend <- paste(c("log(Prec)","log(Tmax)","SPEI","IrrRate","Trend","I(Trend^2)",
                       "factor(Country)","log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), collapse = " + ")
Spec4_trend <- paste(c("log(Prec)","log(Tmax)","log(CO2)*SPEI","IrrRate","Trend","I(Trend^2)",
                       "factor(Country)","log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), collapse = " + ")

# Model00: Trend, FACE, w/o CO2 and FE (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_null <- Fn_FGLS(Spec00_trend, DF_Analysis_Asia)

# Model0: Trend, FACE, w/o CO2 and FE (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_wt_CO2_FE <- Fn_FGLS(Spec0_trend, DF_Analysis_Asia)

# Model1: Trend, FACE, w/o CO2 (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_wt_CO2 <- Fn_FGLS(Spec1_trend, DF_Analysis_Asia)

# Model2: Trend, FACE, w/ CO2 (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_CO2 <- Fn_FGLS(Spec2_trend, DF_Analysis_Asia)

# Model3: Trend, NOFACE, w/o CO2 (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_wt_CO2_NOFACE <- Fn_FGLS(Spec3_trend, DF_Analysis_Asia_NOFACE)

# Model4: Trend, NOFACE, w/ CO2 (33 Asian countries) ------------------------------------------------------
Lt_trend_Asia_CO2_NOFACE <- Fn_FGLS(Spec4_trend, DF_Analysis_Asia_NOFACE)

# Summary ------------------------------------------------------
# Summary Trend (w/ and w/o FACE using FGLS)(Table S4 in the paper)
stargazer(Lt_trend_Asia_wt_CO2$FGLS3, Lt_trend_Asia_CO2$FGLS3, 
          no.space=TRUE, type = "text",
          #out = "Model2_Comparison_Trend_Paper.txt",  
          omit=c("factor\\(Country"), 
          omit.stat = c("ser","f","rsq"),
          dep.var.labels=c("log of rice yield (in ton/hectare)"),
          column.labels=c("Without CO2", "With CO2"),
          covariate.labels=c("Log of Precipitation (in mm)", "Log of Max. Temperature (in degrees Celsius)", 
                             "Log of CO2 (in ppm)", "SPEI (-: drought, +: wet)","Irrigation rate (as a percentage)", 
                             "Time (Sequence of 1 to 56)", "Time squared", 
                             "FACE dummy", "China", "Log of CO2 x SPEI", "FACE dummy X China"),
          add.lines = list(
            c("Country FE", "Yes","Yes"),
            c("Country FE X Precipitation", "Yes","Yes"),
            c("Country FE X Temperature", "Yes", "Yes"),
            c("Country FE X SPEI", "Yes", "Yes"),
            c("Breusch-Pagan test(p-value)", 
              Lt_trend_Asia_wt_CO2$BPtest, Lt_trend_Asia_CO2$BPtest),
            c("Degree of freedom", 
              summary(Lt_trend_Asia_wt_CO2$FGLS3)$df[2], summary(Lt_trend_Asia_CO2$FGLS3)$df[2]),
            c("Num. of countries", 
              Lt_trend_Asia_wt_CO2$Num_country, Lt_trend_Asia_CO2$Num_country)))

# 3.2 Visualize conditional climate effects by region ------------------------------------------------------------------------------------------
# Create data frame for storing conditional mean results
List_Region <- unique(DF_Analysis_Asia$Region)
DF_cplot_climate <- data.frame()
Fn_climate_plot_DF <- function(Model,DF){
  
  for(i in 1:length(List_Region)){
    
    DF_cplot_Prec <- cplot(Model, x = "Prec", data = DF[DF[["Region"]]==List_Region[i],])
    DF_cplot_Tmax <- cplot(Model, x = "Tmax", data = DF[DF[["Region"]]==List_Region[i],])
    DF_cplot_Prec$Variable <- "Precipitation (mm)"
    DF_cplot_Tmax$Variable <- "Max temperature (°C)"
    DF_cplot_Prec$Region <- DF_cplot_Tmax$Region <- sprintf("%s",List_Region[i])
    DF_cplot_climate <- rbind.data.frame(DF_cplot_climate, DF_cplot_Prec, DF_cplot_Tmax)
  }
  DF_cplot_climate
}

# Time trend model
DF_cplot_climate_Time_trend <- Fn_climate_plot_DF(Lt_trend_Asia_CO2$FGLS3, DF_Analysis_Asia)

# Time trend model (Precipitation)(Figure 2a)
DF_cplot_climate_Time_trend %>%
  filter(Variable == "Precipitation (mm)") %>%
  ggplot(aes(x=xvals, y=yvals, fill = Region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(color = Region), size = 1.2) +
  scale_x_continuous(label=comma) + 
  facet_wrap( ~ Region, scales = "free", nrow = 1) +
  labs(y = "Rice yield (ton/hectare)", x = "Precipitation (mm)") + 
  theme_bw() +
  theme(text = element_text(size=24),legend.position = "none")

# Time trend model (Max. temperature)(Figure 2b)
Fn_Round <- function(x){sprintf("%.1f", x)}
DF_cplot_climate_Time_trend %>%
  filter(Variable == "Max temperature (°C)") %>%
  ggplot(aes(x=xvals, y=yvals, fill = Region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(color = Region), size = 1.2) +
  scale_y_continuous(labels = Fn_Round) +
  facet_wrap( ~ Region, scales = "free", nrow = 1) +
  labs(y = "Rice yield (ton/hectare)", x ="Max temperature (°C)") + 
  theme_bw() +
  theme(text = element_text(size=24), legend.position = "none")


# 3.3 Visualize conditional climate effects by country ------------------------------------------------------------------------------------------
List_Country <- unique(DF_Analysis_Asia$Country)
DF_cplot_climate_Country <- data.frame()
Fn_climate_plot_Country <- function(Model,DF){
  
  for(i in 1:length(List_Country)){
    
    DF_cplot_Prec <- cplot(Model, x = "Prec", data = DF[DF[["Country"]]==List_Country[i],])
    DF_cplot_Tmax <- cplot(Model, x = "Tmax", data = DF[DF[["Country"]]==List_Country[i],])
    DF_cplot_Prec$Variable <- "Precipitation (mm)"
    DF_cplot_Tmax$Variable <- "Max temperature (°C)"
    DF_cplot_Prec$Country <- DF_cplot_Tmax$Country <- sprintf("%s",List_Country[i])
    DF_cplot_climate <- rbind.data.frame(DF_cplot_climate, DF_cplot_Prec, DF_cplot_Tmax)
  }
  DF_cplot_climate
}

# Time trend model
DF_cplot_climate_Time_trend_Country <- Fn_climate_plot_Country(Lt_trend_Asia_CO2$FGLS3, DF_Analysis_Asia)

# Aggregate the prediction result from country-level to regional-level
List_Region_Country <- DF_Analysis_Asia %>% select(Region, Country)
List_Region_Country <- List_Region_Country[duplicated.data.frame(List_Region_Country)==FALSE,]
DF_cplot_climate_Time_trend_Country <- merge(List_Region_Country, DF_cplot_climate_Time_trend_Country, by = "Country")
DF_cplot_climate_Time_trend_Country <- DF_cplot_climate_Time_trend_Country %>% arrange(Region, Country)

DF_cplot_climate_Time_trend_Country$Country <- factor(DF_cplot_climate_Time_trend_Country$Country, 
                                                      levels = c(unique(DF_cplot_climate_Time_trend_Country$Country)))

# Time trend model (Precipitation)(Figure S2a)
Fn_Round <- function(x){sprintf("%.0f", x)}
DF_cplot_climate_Time_trend_Country %>%
  filter(Variable == "Precipitation (mm)") %>%
  ggplot(aes(x=xvals/10, y=yvals, fill = Region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(color = Region), size = 1.5) +
  #scale_x_continuous(label=Fn_Round) + 
  labs(y = "Rice yield (ton/hectare)", x = "Precipitation (cm)") + 
  facet_wrap(~ Country, scales = "free", nrow = 11) +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size=24), 
        axis.text.x = element_text(hjust = 0.3, size = 16), axis.text.y = element_text(size = 16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Time trend model (Max. temperature)(Figure S2b)
Fn_Round <- function(x){sprintf("%.1f", x)}
DF_cplot_climate_Time_trend_Country %>%
  filter(Variable == "Max temperature (°C)") %>%
  ggplot(aes(x=xvals, y=yvals, fill = Region)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  geom_line(aes(color = Region), size = 1.5) +
  #scale_x_continuous(labels = Fn_Round) +
  facet_wrap( ~ Country, scales = "free", nrow = 11) +
  labs(y = "Rice yield (ton/hectare)", x ="Max temperature (°C)") + 
  theme_bw() +
  theme(legend.position = "top", text = element_text(size=24), 
        axis.text.x = element_text(hjust = 0.3, size = 16), axis.text.y = element_text(size = 16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# 4. Forecasting  (Time trend) ------------------------------------------------------------------------------------------
# 4.1 Create a input dataframe for forecasting (using Predict()) ------------------------------------------------------------
# Specifying in R points to predict using lm() and predict() with interactions and as.factor vars. 
# Reference: 
# https://stackoverflow.com/questions/31124440/specifying-in-r-points-to-predict-using-lm-and-predict-with-interactions-and

# Create a formula object
Form_CO2_TimeTrend <- as.formula(paste("log(Yields)", Spec2_trend, sep = " ~ "))

# Create a prediction dataframe
DF_Forecast_TimeTrend <- data.frame(model.matrix(Form_CO2_TimeTrend, DF_Analysis_Asia)[,-1]) # Remove the intercept

# Extract the variable name
colnames(DF_Forecast_TimeTrend) <- names(model.matrix(Form_CO2_TimeTrend, DF_Analysis_Asia)[,-1][1,])
colnames(DF_Forecast_TimeTrend)[1:6] <- c("Prec","Tmax","CO2","SPEI","IrrRate","Trend")

# Add Year, Region, and Country variables
DF_Forecast_TimeTrend$Year <- DF_Analysis_Asia$Year
DF_Forecast_TimeTrend$Region <- DF_Analysis_Asia$Region
DF_Forecast_TimeTrend$Country <- DF_Analysis_Asia$Country

# Add original values of Prec, Tmax, and CO2 instead of in a log form
DF_Forecast_TimeTrend$Prec <- exp(DF_Forecast_TimeTrend$Prec)
DF_Forecast_TimeTrend$Tmax <- exp(DF_Forecast_TimeTrend$Tmax)
DF_Forecast_TimeTrend$CO2 <- exp(DF_Forecast_TimeTrend$CO2)

# Extract countries names
Lt_country_Direct <- rownames(Lt_trend_Asia_CO2$OLS_HC)[c(which(rownames(Lt_trend_Asia_CO2$OLS_HC)=="factor(Country)Bangladesh"):
                                                          which(rownames(Lt_trend_Asia_CO2$OLS_HC)=="factor(Country)Vietnam"))]
Lt_country_Direct <- substring(Lt_country_Direct, regexpr(")", Lt_country_Direct)+1)

# Prec x Country FE
DF_Forecast_TimeTrend[,sprintf("Prec:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$Prec

# Tmax x Country FE
DF_Forecast_TimeTrend[,sprintf("Tmax:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$Tmax

# SPEI x Country FE
DF_Forecast_TimeTrend[,sprintf("SPEI:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$SPEI

# 4.2 Predict rice yields (Time trend, in-sample period) ------------------------------------------------------------------------------------------
# Create in-sample forecast remaining CO2 at the 1990 level
DF_Forecast_TimeTrend_1990CO2 <- DF_Forecast_TimeTrend

DF_Forecast_TimeTrend_1990CO2[DF_Forecast_TimeTrend_1990CO2$Year>=1990,"CO2"] <- DF_Forecast_TimeTrend_1990CO2 %>%
  filter(Year == 1990) %>%
  group_by(Year) %>% 
  summarize(CO2 = min(CO2)) %>%
  select(CO2) %>% 
  data.frame()

# Apply predict()
DF_Pred_TimeTrend_1990CO2 <- predict(Lt_trend_Asia_CO2$FGLS3, newdata = subset.data.frame(DF_Forecast_TimeTrend_1990CO2, FACE_dummy == 0), 
                                     type = "response", interval = "confidence", level = 0.95)
DF_Pred_TimeTrend_realCO2 <- predict(Lt_trend_Asia_CO2$FGLS3, newdata = subset.data.frame(DF_Forecast_TimeTrend, FACE_dummy == 0),  
                                     type = "response", interval = "confidence", level = 0.95)

DF_Pred_TimeTrend_1990CO2 <- data.frame(DF_Pred_TimeTrend_1990CO2)
DF_Pred_TimeTrend_realCO2 <- data.frame(DF_Pred_TimeTrend_realCO2)

# Convert rice yields to the original unit
DF_Pred_TimeTrend_1990CO2[,1:3] <- exp(DF_Pred_TimeTrend_1990CO2[,1:3])
DF_Pred_TimeTrend_realCO2[,1:3] <- exp(DF_Pred_TimeTrend_realCO2[,1:3])

# Add a year column
DF_Pred_TimeTrend_1990CO2$Year <- subset.data.frame(DF_Forecast_TimeTrend_1990CO2, FACE_dummy == 0)$Year
DF_Pred_TimeTrend_realCO2$Year <- subset.data.frame(DF_Forecast_TimeTrend, FACE_dummy == 0)$Year

# Add a region column
DF_Pred_TimeTrend_1990CO2$Region <- subset.data.frame(DF_Forecast_TimeTrend_1990CO2, FACE_dummy == 0)$Region
DF_Pred_TimeTrend_realCO2$Region <- subset.data.frame(DF_Forecast_TimeTrend, FACE_dummy == 0)$Region

# Add a country column
DF_Pred_TimeTrend_1990CO2$Country <- subset.data.frame(DF_Forecast_TimeTrend_1990CO2, FACE_dummy == 0)$Country
DF_Pred_TimeTrend_realCO2$Country <- subset.data.frame(DF_Forecast_TimeTrend, FACE_dummy == 0)$Country

# Add a scenario column
DF_Pred_TimeTrend_1990CO2$Scenario <- "1990-level"
DF_Pred_TimeTrend_realCO2$Scenario <- "Observed-level"

# Merge all forecast dataframe
DF_Predict_TimeTrend_CO2 <- rbind.data.frame(DF_Pred_TimeTrend_realCO2, DF_Pred_TimeTrend_1990CO2)

# Convert Scenario as a factor object
DF_Predict_TimeTrend_CO2$Scenario <- factor(DF_Predict_TimeTrend_CO2$Scenario, level = c("Observed-level","1990-level"))

# Visualize the forecasting result (Subregional-level)(Figure 3)
DF_Predict_TimeTrend_CO2 %>% 
  group_by(Year, Region, Scenario) %>%
  summarise(fit = mean(fit), lwr = mean(lwr), upr = mean(upr)) %>%
  
  ggplot(aes(x=Year, y=fit, fill = Scenario)) +
  geom_line(size = 1.5, aes(color = Scenario), alpha=0.6) + 
  scale_color_manual(values = c("red","blue"), labels = c("Observed-level","1990-level")) + 
  labs(y = "Rice yield (ton/hectare)",color="CO2 scenario") + 
  facet_wrap(~Region, nrow = 1) +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size=28), 
        legend.key.width = unit(3,"cm"),legend.title = element_text(size=24, face="bold"))

# Change rate for rice yield at the observed-level CO2
DF_Predict_TimeTrend_CO2_rate <- DF_Predict_TimeTrend_CO2 %>% 
  group_by(Year,Region, Scenario) %>%
  filter(Region != "Central Asia") %>%
  summarise(fit = mean(fit)) %>%
  data.frame()

# Change rate for rice yield at the observed-level CO2
Rate_observed_TimeTrend <- ((DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 2016 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"] - 
                           DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"]) /
                          DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"]) * 100

# Change rate for rice yield at the 1990-level CO2
Rate_1990level_TimeTrend <- ((DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 2016 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"] - 
                            DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"]) /
                           DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"]) * 100

# Share of the observed rice year attributed to CO2 (Numbers used in the main findings)
(1-(Rate_1990level_TimeTrend/Rate_observed_TimeTrend)) * 100

# 4.3 Forecasting (out-of-sample period, time trend model) ------------------------------------------------------------------------------------------
# Adjust the change rate sign for the max length of dry spell because higher dryspell means drier, 
# which is opposit to SPEI, where higher value means wetter.
DF_RCP_weather_raw$Dryspell <- (-1) * DF_RCP_weather_raw$Dryspell

# 4.3.1 Country-level ------------------------------------------------------------------------------------------
# Step 1. Create out-of-sample dataset including each country
# Step 2. Forecast rice yields for each country in 2030 and 2050 under different RCP scenarios
# Step 3. Create a table / Plot a dumbbell chart

# Create an empty projection dataframe
DF_Forecast_proj_country <- setNames(data.frame(matrix(ncol = ncol(DF_Forecast_TimeTrend), nrow = length(2017:2100)*length(unique(DF_Forecast_TimeTrend$Country)))), 
                             colnames(DF_Forecast_TimeTrend))

# Year
DF_Forecast_proj_country$Year <- rep(2017:2100, times = length(unique(DF_Forecast_TimeTrend$Country)))

# Trend
DF_Forecast_proj_country$Trend <- DF_Forecast_proj_country$Year - 1960

# Country
DF_Forecast_proj_country$Country <- rep(unique(DF_Forecast_TimeTrend$Country), each = length(2017:2100))
DF_Forecast_proj_country$FACE_China <- 0

# Irrigate rate
Mean_IrrRate <- aggregate(IrrRate ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_IrrRate$Country))) {
  if(unique(Mean_IrrRate$Country)[i] == "Saudi Arabia"){
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == "Saudi Arabia","IrrRate"] <- Mean_IrrRate[Mean_IrrRate$Year==1980 & Mean_IrrRate$Country == "Saudi Arabia","IrrRate"]
  }
  else if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == "Syria","IrrRate"] <- Mean_IrrRate[Mean_IrrRate$Year==1996 & Mean_IrrRate$Country == "Syria","IrrRate"]
  }
  else{
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == unique(Mean_IrrRate$Country)[i],"IrrRate"] <- Mean_IrrRate[Mean_IrrRate$Year==2016 & Mean_IrrRate$Country == unique(Mean_IrrRate$Country)[i],"IrrRate"] 
  }
}

# Overview the assumed irrigation rate in 2050 in each country
DF_Forecast_proj_country %>%
  filter(Year == 2050) %>%
  select(Country, Year, IrrRate) %>%
  data.frame()

# Country FE function
Fn_Country_dummy <- function(country_list){
  ifelse(DF_Forecast_proj_country$Country == country_list, 1, 0)
}
DF_Forecast_proj_country[,sprintf("factor(Country)%s",Lt_country_Direct)] <- sapply(Lt_country_Direct, Fn_Country_dummy)

# Seperate scenarios
DF_Forecast_proj_country_RCP4.5 <- DF_Forecast_proj_country_RCP6 <- DF_Forecast_proj_country_RCP8.5 <- DF_Forecast_proj_country

# Prec
Mean_Prec <- aggregate(Prec ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_Prec$Country))) {
  if(unique(Mean_Prec$Country)[i] == "Saudi Arabia"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Saudi Arabia","Prec"] <- Mean_Prec[Mean_Prec$Year==1980 & Mean_Prec$Country == "Saudi Arabia","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Prec"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Saudi Arabia","Prec"] <- Mean_Prec[Mean_Prec$Year==1980 & Mean_Prec$Country == "Saudi Arabia","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Prec"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Saudi Arabia","Prec"] <- Mean_Prec[Mean_Prec$Year==1980 & Mean_Prec$Country == "Saudi Arabia","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Prec"])[-1]
  }
  else if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Prec"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Prec"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Prec"])[-1]
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2016 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Prec"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2016 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Prec"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2016 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Prec"])[-1]
  }
}

# Tmax
Mean_Tmax <- aggregate(Tmax ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_Tmax$Country))) {
  if(unique(Mean_Prec$Country)[i] == "Saudi Arabia"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])[-1]
  }
  else if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])[-1]
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2016 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2016 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2016 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])[-1]
  }
}

# SPEI
# For the SPEI projection, go to KNMI website and choose "GCM: CMIP5 extremes (one ensemble member)" at the topic "Select a dataset and variable"
# Since SPEI is a measure of climate variability, you may replace it by a variable such as maximum length of dry spell.
Mean_SPEI <- aggregate(SPEI ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_SPEI$Country))) {
  if(unique(Mean_SPEI$Country)[i] == "Saudi Arabia"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Saudi Arabia","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1980 & Mean_SPEI$Country == "Saudi Arabia","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Saudi Arabia","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1980 & Mean_SPEI$Country == "Saudi Arabia","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Saudi Arabia","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1980 & Mean_SPEI$Country == "Saudi Arabia","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Dryspell"])[-1]
  }
  else if(unique(Mean_SPEI$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Dryspell"])[-1]
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2016 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2016 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Dryspell"])[-1]
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2016 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Dryspell"])[-1]
  }
}

# FACE
DF_Forecast_proj_country_RCP4.5$FACE_dummy <- 0
DF_Forecast_proj_country_RCP6$FACE_dummy <- 0
DF_Forecast_proj_country_RCP8.5$FACE_dummy <- 0

# CO2 projection ------------------------------------------------------------------------------------------
DF_Forecast_proj_country_RCP4.5$CO2 <- (DF_RCP_raw %>% filter(Year >=2017) %>% select(CO2.RCP4.5) %>% unlist())
DF_Forecast_proj_country_RCP6$CO2 <- (DF_RCP_raw %>% filter(Year >=2017) %>% select(CO2.RCP6) %>% unlist())
DF_Forecast_proj_country_RCP8.5$CO2 <- (DF_RCP_raw %>% filter(Year >=2017) %>% select(CO2.RCP8.5) %>% unlist())

# Apply predict(): FGLS3 ------------------------------------------------------------------------------------------ 
DF_Pred_RCP4.5 <- predict(Lt_trend_Asia_CO2$FGLS3, newdata = DF_Forecast_proj_country_RCP4.5, type = "response", interval = "confidence", level = 0.95)
DF_Pred_RCP6 <- predict(Lt_trend_Asia_CO2$FGLS3, newdata = DF_Forecast_proj_country_RCP6, type = "response", interval = "confidence", level = 0.95)
DF_Pred_RCP8.5 <- predict(Lt_trend_Asia_CO2$FGLS3, newdata = DF_Forecast_proj_country_RCP8.5, type = "response", interval = "confidence", level = 0.95)

DF_Pred_RCP4.5 <- data.frame(DF_Pred_RCP4.5)
DF_Pred_RCP6 <- data.frame(DF_Pred_RCP6)
DF_Pred_RCP8.5 <- data.frame(DF_Pred_RCP8.5)

# Convert rice yields to the original unit
DF_Pred_RCP4.5[,1:3] <- exp(DF_Pred_RCP4.5[,1:3])
DF_Pred_RCP6[,1:3] <- exp(DF_Pred_RCP6[,1:3])
DF_Pred_RCP8.5[,1:3] <- exp(DF_Pred_RCP8.5[,1:3])

# Add a year column
DF_Pred_RCP4.5$Year <- 2017:2100
DF_Pred_RCP6$Year <- 2017:2100
DF_Pred_RCP8.5$Year <- 2017:2100

# Add a country column
DF_Pred_RCP4.5$Country <- DF_Forecast_proj_country_RCP4.5$Country
DF_Pred_RCP6$Country <- DF_Forecast_proj_country_RCP6$Country
DF_Pred_RCP8.5$Country <- DF_Forecast_proj_country_RCP8.5$Country

# Add a scenario column
DF_Pred_RCP4.5$Scenario <- "RCP4.5"
DF_Pred_RCP6$Scenario <- "RCP6"
DF_Pred_RCP8.5$Scenario <- "RCP8.5"

# Merge all forecast dataframe
DF_Predicted_Yields_AGRD_Country <- rbind.data.frame(DF_Pred_RCP4.5, DF_Pred_RCP6, DF_Pred_RCP8.5)

# Visualize the forecasting result: Country-level ------------------------------------------------------------------------------------------
# Extract the predicted result in 2030 and 2050 for each country under three different warming scenarios
DF_ProYields_Country <- DF_Predicted_Yields_AGRD_Country %>%
  filter(Year %in% c(2030, 2050)) %>%
  filter(!Country %in% c("Azerbaijan","Kazakhstan","Kyrgyzstan","Syria", "Hong Kong",
                         "Saudi Arabia","Tajikistan","Turkmenistan","Uzbekistan")) %>% 
  select(fit, Year, Country, Scenario)


# Store predicted yield in 2030 and 2050
DF_ProYields_Country <- data.frame(
  Country = rep(unique(DF_ProYields_Country$Country), times=length(unique(DF_ProYields_Country$Scenario))),
  Scenario = rep(unique(DF_ProYields_Country$Scenario), each=length(unique(DF_ProYields_Country$Country))),
  fit2030 = DF_ProYields_Country[DF_ProYields_Country$Year==2030,"fit"],
  fit2050 = DF_ProYields_Country[DF_ProYields_Country$Year==2050,"fit"]
)

# Extract yield data in 2016 for each country
DF_Benchmark_yield <- DF_Analysis_Asia %>% 
  filter(Year == 2016) %>%
  select(Country, Yields)

# Merge the benchmark data to the projected data set
DF_ProYields_Country <- merge(DF_ProYields_Country, DF_Benchmark_yield, by = "Country")

# Reorder the data set
DF_ProYields_Country <- DF_ProYields_Country[order(DF_ProYields_Country$Scenario, DF_ProYields_Country$Country),]

# Calculate yield differences in 2030 and 2050 for each country
DF_ProYields_Country$Diff2016_30 <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2030 - DF_ProYields_Country$Yields)/DF_ProYields_Country$Yields),0))
DF_ProYields_Country$Diff2016_50 <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2050 - DF_ProYields_Country$Yields)/DF_ProYields_Country$Yields),0))
DF_ProYields_Country$Diff <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2050 - DF_ProYields_Country$fit2030)/DF_ProYields_Country$fit2030),0))

# Set the right ordering of countries
DF_ProYields_Country$Country <- factor(DF_ProYields_Country$Country, levels = rev(unique(DF_ProYields_Country$Country)))

# Save the result as a csv file
write.csv(DF_ProYields_Country, "Projected_countries_yield2016.csv")

# Plot a dumbbell chart
DF_ProYields_Country %>% 
  ggplot() + 
  geom_dumbbell(aes(y=Country, x=fit2030, xend=fit2050), color="#b2b2b2", size=1.5, size_x =3, size_xend =3, colour_x = "#0171CE", colour_xend = "#DE4433") + 
  
  # Text above points
  geom_text(data=filter(DF_ProYields_Country, Country=="Bangladesh"), aes(x=fit2030, y=Country, label="2030"), color="#0171CE", size=6, hjust=1.7, fontface="bold") + 
  geom_text(data=filter(DF_ProYields_Country, Country=="Bangladesh"), aes(x=fit2050, y=Country, label="2050"), color="#DE4433", size=6, hjust=-0.5, fontface="bold") +
  
  # Diff
  geom_rect(data=DF_ProYields_Country, aes(xmin=27, xmax=30, ymin=-Inf, ymax=Inf), fill="gray92") + 
  geom_text(data=DF_ProYields_Country, aes(label=Diff, y=Country, x=28.5), size=6, vjust=0.5, color="gray30") + 
  geom_text(data=filter(DF_ProYields_Country, Country=="Afghanistan"), aes(x=28.5, y=Country, label="% DIFF"), size=6, vjust=-1, fontface="bold") +
  
  labs(x="Rice yield (ton/hectare)", y=NULL) +
  
  facet_wrap(~Scenario, strip.position="top") +
  theme_bw() +
  #theme_classic() +
  theme(text = element_text(size=30),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        axis.ticks=element_blank(),
        panel.border=element_blank())

# Visualize the forecasting result: Regional-level ------------------------------------------------------------------------------------------
# Aggregate the prediction result from country-level to regional-level
List_Region <- DF_Analysis_Asia %>% select(Region, Country)
List_Region <- List_Region[duplicated.data.frame(List_Region)==FALSE,]
DF_ProYields_Region <- DF_ProYields_Country
DF_ProYields_Region <- merge(List_Region, DF_ProYields_Region, by = "Country")

# Import rice production data
DF_Rice_production2016 <- DF_Rice_production %>% filter(Year == 2016 & Country %in% unique(DF_ProYields_Country$Country))
DF_Rice_production2016 <- merge(List_Region, DF_Rice_production2016, by = "Country")
DF_Rice_production_Region <- DF_Rice_production2016 %>%
  group_by(Region) %>%
  summarize(Region_Tot = sum(Production)) %>%
  data.frame()

DF_ProYields_Region <- merge(DF_ProYields_Region, DF_Rice_production2016, by = c("Region","Country"))
DF_ProYields_Region <- merge(DF_ProYields_Region, DF_Rice_production_Region, by = "Region")
DF_ProYields_Region$MktShare <- round(DF_ProYields_Region$Production/DF_ProYields_Region$Region_Tot, 4)

# Save the result as a csv file
write.csv(DF_ProYields_Region, "Projected_regions_yield2016_MktShare.csv")

DF_ProYields_Region <- DF_ProYields_Region %>%
  group_by(Scenario, Region) %>%
  summarize(Yield2016 = weighted.mean(Yields, MktShare), fit2030 = weighted.mean(fit2030, MktShare), fit2050 = weighted.mean(fit2050, MktShare)) %>%
  data.frame()

# Calculate yield differences in 2030 and 2050 for each country
DF_ProYields_Region$Diff2016_30 <- sprintf("%d%%",round(100*((DF_ProYields_Region$fit2030 - DF_ProYields_Region$Yield2016)/DF_ProYields_Region$Yield2016),0))
DF_ProYields_Region$Diff2016_50 <- sprintf("%d%%",round(100*((DF_ProYields_Region$fit2050 - DF_ProYields_Region$Yield2016)/DF_ProYields_Region$Yield2016),0))

# Save the result as a csv file
write.csv(DF_ProYields_Region, "Projected_regions_yield2016.csv")
