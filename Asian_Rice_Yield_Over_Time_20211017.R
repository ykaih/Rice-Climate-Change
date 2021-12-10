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
library(hrbrthemes) # theme_ipsum
library(plm)  # pdataframe()

# Set up the working directory
# setwd("C:\\Users\\")

# Clean data in the global environment
rm(list = ls())

# 1. Load data ------------------------------------------------------------------------------------------
# 1.1 Projection data set ------------------------------------------------------------------------------------------
# Climate projection data source: KNMI website
# prec: GCM: CMIP5, monthly, world, rcp45/65/85
# tmax: GCM: CMIP5, monthly, world, rcp45/65/85
# dryshell (maximum length of dry spell, CDD): GCM: CMIP5 extremes (one ensemble member), monthly, world, rcp45/65/85
DF_RCP_weather_raw <- read.csv("Climate_Projection_Change_Rate.csv")
colnames(DF_RCP_weather_raw)[1] <- "Year"

# CO2 projection data set
DF_RCP_raw <- read.csv("DF_RCP_Projection.csv")

# Rice production (Unit: tonnes)
DF_Rice_production <- read.csv("DF_Rice_production.csv")

# Convert the country column as a character object
DF_Rice_production$Country <- as.character(DF_Rice_production$Country)

# 1.2 Main analysis data set ------------------------------------------------------------------------------------------
DF_Analysis_raw <- read.csv("DF_Analysis_20211012.csv")

# Delete the first redundant column
DF_Analysis <- DF_Analysis_raw %>% dplyr::select(-X)

# Available duration by country
DF_Analysis %>% 
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

# Create dummy variable for China
DF_Analysis$China <- ifelse(DF_Analysis$Country == "China", 1, 0)

# Create interaction term between China and FACE dummy
DF_Analysis$FACE_China <- DF_Analysis$China*DF_Analysis$FACE_dummy

# Convert the unit of yields from "hectogram/hectare" to "ton/hectare"
DF_Analysis$Yields <- DF_Analysis$Yields/10000

# Extract observations in Asia 
DF_Analysis_Asia <- DF_Analysis[DF_Analysis$Asia==1, ]
DF_Analysis_Asia_NOFACE <- DF_Analysis[DF_Analysis$Asia==1 & DF_Analysis$FACE_dummy!=1, ]

# Convert the country column as a character object
DF_Analysis_Asia_NOFACE$Country <- as.character(DF_Analysis_Asia_NOFACE$Country)

# Set the factor level for the region 
DF_Analysis_Asia$Region <- factor(DF_Analysis_Asia$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                             labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))
DF_Analysis_Asia_NOFACE$Region <- factor(DF_Analysis_Asia_NOFACE$Region, levels = c("CAsia","EAsia","SAsia","SEAsia","WAsia"),
                                  labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia"))

# 2. Exploration data ------------------------------------------------------------------------------------------
# 2.1 Summary statistics ------------------------------------------------------------------------------------------
DF_Summary_stat <- DF_Analysis_Asia %>% select("Yields","Prec","Tmax","SPEI","CO2","FertApp","IrrRate")

# Summarize output 
stargazer(DF_Summary_stat, type = "text",
          header = F, digits=2, no.space=TRUE, omit.table.layout=c("l"),  
          #out = "Summary_Stat.txt", # Uncomment this line if the user wants to save the result as a text file
          omit.summary.stat = c("p25","p75"),
          covariate.labels=c("Yield (ton/hectare)", "Precipitation (mm)", "Mean max. temperature (Celsius)", 
                             "SPEI (-: drought, +: wet)","CO2 concentration (ppm)", "Fertilizer application (Kg N/ha)", "Irrigation rate"))

# Correlation between Trend and CO2 (with FACE data)
cor.test(DF_Analysis_Asia$CO2,DF_Analysis_Asia$Trend, method=c("pearson"))

# Correlation between Trend and CO2 (without FACE data)
cor.test(DF_Analysis_Asia_NOFACE$CO2,DF_Analysis_Asia_NOFACE$Trend, method=c("pearson"))

# 2.2 Visualize key variables: rice yields, AGRD, CO2 ------------------------------------------------------------------------------------------
# Country-level rice yield trend
DF_plot_rice_yield_Country <- DF_Analysis_Asia_NOFACE %>% 
  select(Region, Country, Year, Yields, Prec, Tmax, Tmean, Tmin, SPEI, FertApp, IrrRate) %>% 
  arrange(Region, Country)
DF_plot_rice_yield_Country$Country <- factor(DF_plot_rice_yield_Country$Country, 
                                             levels = c(unique(DF_plot_rice_yield_Country$Country)))

# Function for rounding numbers in a plot
Fn_Round <- function(x){sprintf("%.0f", x)}

# Time series plot: Country-level rice yield  (Figure S1)
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=Yields, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Rice Yield (ton/hectare)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=Fn_Round) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Calculate average region-level rice yield 
DF_Yield_Region <- DF_Analysis_Asia_NOFACE %>%
  group_by(Region,Year) %>%
  summarize(Yields = mean(Yields))

# Time series plot: Region-level rice yield 
ggplot() +
  geom_line(aes(x=DF_Analysis_Asia_NOFACE$Year, y=DF_Analysis_Asia_NOFACE$Yields, group = DF_Analysis_Asia_NOFACE$Country), color = "gray85", size=1) +
  geom_line(aes(x=DF_Yield_Region$Year, y=DF_Yield_Region$Yields, group = DF_Yield_Region$Region), color= "gray70", size=1.9) +
  geom_line(aes(x=DF_Yield_Region$Year, y=DF_Yield_Region$Yields, color = DF_Yield_Region$Region), size=1.2) +
  labs(x = "Year", y = "Rice Yield (ton/hectare)") +
  scale_x_continuous(breaks = seq(1960,2015, 5)) +
  scale_y_continuous(label=comma) +
  scale_color_manual(name = "Region", values = c("#D73027","#313695","#FDAE61","#74ADD1","#F46D43"), 
                     labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +  
  theme_bw() +
  theme(legend.position = c(0.15, 0.85), legend.text = element_text(size=20), 
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Rice yield growth rate from 1961 to 2015
DF_Analysis_Asia_NOFACE %>%
  group_by(Year) %>%
  summarize(Yields = mean(Yields)) %>%
  filter(Year %in% c(1961, 2015)) %>% 
  data.frame() %>%
  mutate(Rate = (4.228913-2.248348)/2.248348)

# Time series plot: Precipitation
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=Prec, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Precipitation (mm)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Time series plot: Temperature
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=Tmax, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Mean max. temperature (°C)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Time series plot: SPEI
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=SPEI, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "SPEI (-: drought, +: wet)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Correlation between precipitation and SPEI
cor(DF_plot_rice_yield_Country$Prec, DF_plot_rice_yield_Country$SPEI)

# Time series plot: Irrigation rate
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=IrrRate, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Irrigation Rate (%)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Time series plot: Fertilization rate
DF_plot_rice_yield_Country %>%
  ggplot(aes(x=Year, y=FertApp, color = Region)) +
  geom_line(size=1.5) +
  labs(x = "Year", y = "Fertilizer application (kg N/ha)") +
  scale_x_continuous(breaks = seq(1960,2015, 20)) +
  scale_y_continuous(label=comma) +
  facet_wrap(~Country, nrow = 3) +
  theme_bw() +
  theme(legend.position = "top", legend.text = element_text(size=20), axis.text.x = element_text(hjust = 0.3, size = 16),
        legend.title = element_text(size=24, face="bold"), text = element_text(size=28)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Time series plot: CO2 from 1960 to 2015 
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

# Time series plot: Projected CO2 trajectory
melt(DF_RCP_raw, id.vars = "Year") %>%
  ggplot(aes(x=Year, y= value, color=factor(variable))) + 
  geom_line(size=1.2) +
  theme_bw() 

# Figure 1 ------------------------------------------------------------------------------------------
# Scatter plot: CO2 and rice yields by region
ggplot(DF_Analysis_Asia_NOFACE, aes(x=CO2, y=Yields)) +
  geom_point(color = "blue4", size=1, alpha=0.6) + 
  geom_smooth(aes(fill=factor(Region), color=factor(Region)), method = "lm", formula = y ~ poly(x, 2), size = 1.2, se = TRUE) +
  scale_color_manual(name = "Region", values = rep("#D73027", times = 5), 
                     labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_fill_manual(name = "Region", values = rep("#D73027", times = 5), 
                    labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_y_continuous(labels = Fn_Round) +
  labs(x = "CO2 (ppm)", y = "Yield (ton/hectare)") +
  facet_wrap(~ Region, nrow = 5) +
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16))

# Scatter plot: precipitation and rice yields by region
ggplot(DF_plot_rice_yield_Country, aes(x=Prec, y=Yields)) +
  geom_point(color = "blue4", size=1, alpha=0.6) + 
  geom_smooth(aes(fill=factor(Region), color=factor(Region)), method = "lm", formula = y ~ poly(x, 2), size = 1.2, se = TRUE) +
  scale_color_manual(name = "Region", values = rep("#D73027", times = 5), 
                     labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_fill_manual(name = "Region", values = rep("#D73027", times = 5), 
                    labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_y_continuous(labels = Fn_Round) +
  labs(x = "Precipitation (mm)", y = "Yield (ton/hectare)") +
  facet_wrap(~ Region, nrow = 5, scales = "free_x") +
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16), axis.text.x = element_text(hjust = 0.8))

# Scatter plot: temperature and rice yields by region
ggplot(DF_plot_rice_yield_Country, aes(x=Tmax, y=Yields)) +
  geom_point(color = "blue4", size=1, alpha=0.6) + 
  geom_smooth(aes(fill=factor(Region), color=factor(Region)), method = "lm", formula = y ~ poly(x, 2), size = 1.2, se = TRUE) +
  scale_color_manual(name = "Region", values = rep("#D73027", times = 5), 
                     labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_fill_manual(name = "Region", values = rep("#D73027", times = 5), 
                    labels = c("Central Asia", "East Asia", "South Asia", "Southeast Asia", "West Asia")) +
  scale_y_continuous(labels = Fn_Round) +
  labs(x = "Temperature (Celsius)", y = "Yield (ton/hectare)") +
  facet_wrap(~ Region, nrow = 5, scales = "free") +
  theme_bw() +
  theme(legend.position="none", text = element_text(size=16))

# 2.3 Close look at rice yield, CO2, and FACE obs ------------------------------------------------------------------------------------------
# Scatter plot: CO2 and rice yields
ggplot(DF_Analysis_Asia, aes(x=CO2, y=Yields)) +
  geom_point(aes(colour=factor(FACE_dummy)), size=2.5) + 
  labs(x = "CO2 (ppm)", y = "Yield (ton/hectare)", color = "FACE (1 indicates FACE obs.)") +
  theme_bw() +
  theme(legend.position="bottom", text = element_text(size=16)) + 
  guides(colour = guide_legend(keywidth = 3), shape = guide_legend(keywidth = 3))

# Create a dataframe for creating a comparison scatter plot
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

# 2.4 Pre-modeling diagnostic test ----------------------------------------------------------------
# Replicate the original dataset
PDF_Analysis_Asia_NOFACE <- pdata.frame(DF_Analysis_Asia_NOFACE, index = c("Country", "Year"))

# Levin-Lin-Chu Unit-Root Test
summary(purtest(PDF_Analysis_Asia_NOFACE %>% 
                  dplyr::select(log_Yields,log_Prec,log_Tmax,SPEI,FertApp,IrrRate,log_CO2), 
                test = "levinlin", exo = "intercept", lags = "AIC", pmax = 3))
summary(purtest(PDF_Analysis_Asia_NOFACE %>% 
                  dplyr::select(log_Yields,log_Prec,log_Tmax,SPEI,FertApp,IrrRate,log_CO2), 
                test = "levinlin", exo = "trend", lags = "AIC", pmax = 3))

# Im-Pesaran-Shin Unit-Root Test
summary(purtest(PDF_Analysis_Asia_NOFACE %>% 
                  dplyr::select(log_Yields,log_Prec,log_Tmax,SPEI,FertApp,IrrRate,log_CO2), 
                test = "ips", exo = "intercept", lags = "AIC", pmax = 3))
summary(purtest(PDF_Analysis_Asia_NOFACE %>% 
                  dplyr::select(log_Yields,log_Prec,log_Tmax,SPEI,FertApp,IrrRate,log_CO2), 
                test = "ips", exo = "trend", lags = "AIC", pmax = 3))

# 3. Estimation ------------------------------------------------------------------------------------------
# 3.1 Set up model specifications ----------------------------------------------------------------
# Select variables
DF_main <- DF_Analysis_Asia %>%
  select("Yields","Prec","Tmax","Tmin","SPEI","CO2","FACE_dummy","FertApp","IrrRate","Trend","Sq_Trend","FACE_China","Year","Region","Country") %>%
  mutate(Country = factor(Country))

# Create list of models with 12 out of 13 controls
vars <- list(c("log(Prec)","log(Tmax)","SPEI"), # (1)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)"), # (2)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","Country",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (3)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","log(FertApp)","IrrRate","Country",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (4)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","log(FertApp)","IrrRate","Trend","Sq_Trend","Country",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (5)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","Trend","Sq_Trend","Country",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (6)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","log(FertApp)","IrrRate","Trend","Sq_Trend","FACE_dummy","FACE_China","Country",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (7)
             c("log(Prec)","log(Tmax)","SPEI","log(FertApp)","IrrRate","Trend","Sq_Trend","FACE_dummy","FACE_China",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)"), # (8)
             c("log(Prec)","log(Tmax)","SPEI","log(CO2)","log(CO2)*SPEI","log(FertApp)","IrrRate","Trend","Sq_Trend","FACE_dummy","FACE_China",
               "log(Prec)*factor(Country)","log(Tmax)*factor(Country)","SPEI*factor(Country)")) # (9)

# Link all element in "vars" with "+"
flist <- lapply(vars, function(x) paste(x, collapse=" + "))

# 3.2 Execute the estimation ----------------------------------------------------------------
# Conduct FGLS estimations using the provided list of the model specifications
List_SpecCheck <- lapply(flist, function(f) {
  print(f)
  
  # Create formula objects
  S1_form1 <- as.formula(paste("log(Yields)", f, sep = " ~ "))
  S2_form1 <- as.formula(paste("log(residuals(JP1)^2)", f, sep = " ~ "))
  
  # Three step FGLS
  JP1 <- lm(S1_form1, data = DF_main)
  JP2 <- lm(S2_form1, data = DF_main)
  JP3 <- lm(S1_form1, weights=1/(exp(fitted(JP2))), data = DF_main)
  
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
    Num_country = length(unique(DF_main$Country)))
  
  return(List_result)
})

# 3.3 Summarize results ----------------------------------------------------------------
# Summarize 9 models (w/ and w/o FACE using FGLS)(Table S4 in the paper)
stargazer(List_SpecCheck[[1]][["FGLS3"]], List_SpecCheck[[2]][["FGLS3"]], List_SpecCheck[[3]][["FGLS3"]], 
          List_SpecCheck[[4]][["FGLS3"]], List_SpecCheck[[5]][["FGLS3"]], List_SpecCheck[[6]][["FGLS3"]], 
          List_SpecCheck[[7]][["FGLS3"]], List_SpecCheck[[8]][["FGLS3"]], List_SpecCheck[[9]][["FGLS3"]],
          no.space=TRUE, type = "text",
          #out = "Model9_Comparison_Trend_Paper.txt",  # Uncomment this line if the user wants to save the result as a text file
          omit=c("Country*"), 
          omit.stat = c("ser","f","rsq"),
          dep.var.labels=c("log of rice yield (in ton/hectare)"),
          covariate.labels=c("Log of precipitation (in mm)", "Log of max. temperature (in degrees Celsius)", 
                             "SPEI (-: Drought, +: Wet)","Log of atmospheric CO2 (in ppm)", "Log of fertilizer application (in Kg N/hectare)", 
                             "Irrigation rate (as a percentage)", "Time (Sequence of 1 to 56)", "Time squared", "FACE dummy", "FACE dummy X China", 
                             "Log of atmospheric CO2 x SPEI"),
          add.lines = list(
            c("Country FE", 
              "No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
            c("Country FE × Climate variables", 
              "No","No","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
            c("Breusch-Pagan test(p-value)", 
              List_SpecCheck[[1]][["BPtest"]], List_SpecCheck[[2]][["BPtest"]], List_SpecCheck[[3]][["BPtest"]],
              List_SpecCheck[[4]][["BPtest"]], List_SpecCheck[[5]][["BPtest"]], List_SpecCheck[[6]][["BPtest"]],
              List_SpecCheck[[7]][["BPtest"]], List_SpecCheck[[8]][["BPtest"]],List_SpecCheck[[9]][["BPtest"]]),
            c("Degree of freedom", 
              summary(List_SpecCheck[[1]][["FGLS3"]])$df[2], summary(List_SpecCheck[[2]][["FGLS3"]])$df[2], summary(List_SpecCheck[[3]][["FGLS3"]])$df[2],
              summary(List_SpecCheck[[4]][["FGLS3"]])$df[2], summary(List_SpecCheck[[5]][["FGLS3"]])$df[2], summary(List_SpecCheck[[6]][["FGLS3"]])$df[2], 
              summary(List_SpecCheck[[7]][["FGLS3"]])$df[2], summary(List_SpecCheck[[8]][["FGLS3"]])$df[2], summary(List_SpecCheck[[9]][["FGLS3"]])$df[2]), 
            c("Num. of countries", 
              List_SpecCheck[[1]][["Num_country"]], List_SpecCheck[[2]][["Num_country"]], List_SpecCheck[[3]][["Num_country"]],
              List_SpecCheck[[4]][["Num_country"]], List_SpecCheck[[5]][["Num_country"]], List_SpecCheck[[6]][["Num_country"]], 
              List_SpecCheck[[7]][["Num_country"]], List_SpecCheck[[8]][["Num_country"]], List_SpecCheck[[9]][["Num_country"]])))

# Summary Trend (w/ and w/o FACE using FGLS)(Table S4 in the paper)
stargazer(List_SpecCheck[[8]][["FGLS3"]], List_SpecCheck[[9]][["FGLS3"]],
          no.space=TRUE, type = "text",
          #out = "Model2_Comparison_Trend_Paper.txt", # Uncomment this line if the user wants to save the result as a text file
          #omit=c("factor\\(Country","\\Country"), # Uncomment this line if the user wants to show all fixed effect coefficients
          omit.stat = c("ser","f","rsq"),
          dep.var.labels=c("log of rice yield (in ton/hectare)"),
          column.labels=c("Without CO2", "With CO2"),
          covariate.labels=c("Log of Precipitation (in mm)", "Log of Max. Temperature (in degrees Celsius)", "SPEI (-: drought, +: wet)",
                             "Log of atmospheric CO2 (in ppm)", "Log of fertilizer application (in Kg N/hectare)", 
                             "Irrigation rate (as a percentage)", "Time (Sequence of 1 to 56)", "Time squared", "FACE dummy", "FACE dummy X China", 
                             "Log of atmospheric CO2 x SPEI"),
          add.lines = list(
            c("Country FE", "Yes","Yes"),
            c("Country FE X Precipitation", "Yes","Yes"),
            c("Country FE X Temperature", "Yes", "Yes"),
            c("Country FE X SPEI", "Yes", "Yes"),
            c("Breusch-Pagan test(p-value)", 
              List_SpecCheck[[8]][["BPtest"]], List_SpecCheck[[9]][["BPtest"]]),
            c("Degree of freedom", 
              summary(List_SpecCheck[[8]][["FGLS3"]])$df[2], summary(List_SpecCheck[[9]][["FGLS3"]])$df[2]),
            c("Num. of countries", 
              List_SpecCheck[[8]][["Num_country"]], List_SpecCheck[[9]][["Num_country"]])))

# 4. Forecasting ------------------------------------------------------------------------------------------
# 4.1 Create a input dataframe for forecasting (using Predict()) ------------------------------------------------------------
# Specifying in R points to predict using lm() and predict() with interactions and as.factor vars. 
# Reference: 
# https://stackoverflow.com/questions/31124440/specifying-in-r-points-to-predict-using-lm-and-predict-with-interactions-and

# Create a formula object (using the final model, i.e., vars[9])
Form_CO2_TimeTrend <- as.formula(paste("log(Yields)", paste(vars[[9]], collapse=" + "), sep = " ~ "))

# Create a prediction dataframe
DF_Forecast_TimeTrend <- data.frame(model.matrix(Form_CO2_TimeTrend, DF_Analysis_Asia)[,-1]) # Remove the intercept

# Extract the variable name
colnames(DF_Forecast_TimeTrend) <- names(model.matrix(Form_CO2_TimeTrend, DF_Analysis_Asia)[,-1][1,])

# Rename columns (original data is not in log)
colnames(DF_Forecast_TimeTrend)[1:7] <- c("Prec","Tmax","SPEI","CO2","FertApp","IrrRate","Trend")

# Add Year, Region, and Country variables
DF_Forecast_TimeTrend$Year <- DF_Analysis_Asia$Year
DF_Forecast_TimeTrend$Region <- DF_Analysis_Asia$Region
DF_Forecast_TimeTrend$Country <- DF_Analysis_Asia$Country

# Add original values of Prec, Tmax, and CO2 instead of in a log form
DF_Forecast_TimeTrend$Prec <- exp(DF_Forecast_TimeTrend$Prec)
DF_Forecast_TimeTrend$Tmax <- exp(DF_Forecast_TimeTrend$Tmax)
DF_Forecast_TimeTrend$CO2 <- exp(DF_Forecast_TimeTrend$CO2)
DF_Forecast_TimeTrend$FertApp <- exp(DF_Forecast_TimeTrend$FertApp)

# Extract countries names
Lt_country_Direct <- names(List_SpecCheck[[9]][["FGLS3"]]$coefficients)[c(which(names(List_SpecCheck[[9]][["FGLS3"]]$coefficients)=="factor(Country)Bangladesh"):
                                                          which(names(List_SpecCheck[[9]][["FGLS3"]]$coefficients)=="factor(Country)Vietnam"))]
Lt_country_Direct <- substring(Lt_country_Direct, regexpr(")", Lt_country_Direct)+1)

# Prec x Country FE
DF_Forecast_TimeTrend[,sprintf("Prec:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$Prec

# Tmax x Country FE
DF_Forecast_TimeTrend[,sprintf("Tmax:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$Tmax

# SPEI x Country FE
DF_Forecast_TimeTrend[,sprintf("SPEI:factor(Country)%s",Lt_country_Direct)] <- DF_Forecast_TimeTrend[,sprintf("factor(Country)%s",Lt_country_Direct)]*DF_Forecast_TimeTrend$SPEI

# 4.2 Predict rice yields (In-sample period) ------------------------------------------------------------------------------------------
# Create in-sample forecast remaining CO2 at the 1990 level
DF_Forecast_TimeTrend_1990CO2 <- DF_Forecast_TimeTrend

# Retrieve the CO2 level in 1990
DF_Forecast_TimeTrend_1990CO2[DF_Forecast_TimeTrend_1990CO2$Year>=1990,"CO2"] <- DF_Forecast_TimeTrend_1990CO2 %>%
  filter(Year == 1990) %>%
  group_by(Year) %>% 
  summarize(CO2 = min(CO2)) %>%
  select(CO2) %>% 
  data.frame()

# Apply predict() to calculate predicted values
DF_Pred_TimeTrend_1990CO2 <- predict(List_SpecCheck[[9]][["FGLS3"]], newdata = subset.data.frame(DF_Forecast_TimeTrend_1990CO2, FACE_dummy == 0), 
                                     type = "response", interval = "confidence", level = 0.95)
DF_Pred_TimeTrend_realCO2 <- predict(List_SpecCheck[[9]][["FGLS3"]], newdata = subset.data.frame(DF_Forecast_TimeTrend, FACE_dummy == 0),  
                                     type = "response", interval = "confidence", level = 0.95)

# Store the resulting predicted values
DF_Pred_TimeTrend_1990CO2 <- data.frame(DF_Pred_TimeTrend_1990CO2)
DF_Pred_TimeTrend_realCO2 <- data.frame(DF_Pred_TimeTrend_realCO2)

# Convert rice yields to the original unit (from log to original unit)
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

# Figure 3: Visualize the forecasting result (Subregional-level) ------------------------------------------------------------------------------------------
DF_Predict_TimeTrend_CO2 %>% 
  group_by(Year, Region, Scenario) %>%
  summarise(fit = mean(fit), lwr = mean(lwr), upr = mean(upr)) %>%
  
  ggplot(aes(x=Year, y=fit)) +
  geom_ribbon(aes(ymax=upr, ymin=lwr, fill = Scenario), alpha=0.2) +
  geom_line(size = 1.5, aes(color = Scenario), alpha=0.95) + 
  geom_vline(xintercept = 1990, linetype = "dashed", color = "gray40", size = 0.8) +
  scale_color_manual(values = c("#DE4433","#0171CE"), labels = c("Observed-level","1990-level")) + 
  scale_x_continuous(breaks = seq(1960,2020, by= 10)) +
  labs(y = "Rice yield (ton/hectare)", color="CO2 scenario", fill="CO2 scenario") + 
  facet_wrap(~Region, nrow = 1) +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size=28), axis.text.x = element_text(size=18), 
        legend.key.width = unit(3,"cm"),legend.title = element_text(size=24, face="bold"))

# Change rate for rice yield at the observed-level CO2
DF_Predict_TimeTrend_CO2_rate <- DF_Predict_TimeTrend_CO2 %>% 
  group_by(Year,Region, Scenario) %>%
  filter(Region != "Central Asia") %>%
  summarise(fit = mean(fit)) %>%
  data.frame()

# Change rate for rice yield at the observed-level CO2
Rate_observed_TimeTrend <- ((DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 2015 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"] - 
                           DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"]) /
                          DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="Observed-level", "fit"]) * 100

# Change rate for rice yield at the 1990-level CO2
Rate_1990level_TimeTrend <- ((DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 2015 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"] - 
                            DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"]) /
                           DF_Predict_TimeTrend_CO2_rate[DF_Predict_TimeTrend_CO2_rate$Year == 1961 & DF_Predict_TimeTrend_CO2_rate$Scenario=="1990-level", "fit"]) * 100

# Share of the observed rice year attributed to CO2 (Numbers used in the main findings)
(1-(Rate_1990level_TimeTrend/Rate_observed_TimeTrend)) * 100

# Upper bound
max(Rate_observed_TimeTrend)-max(Rate_1990level_TimeTrend)

# Lower bound
min(Rate_observed_TimeTrend)-min(Rate_1990level_TimeTrend)

# 4.3 Forecasting (Out-of-sample period) ------------------------------------------------------------------------------------------
# Adjust the change rate sign for the max length of dry spell because higher dryspell means drier, 
# which is opposite to SPEI, where higher value means wetter.
DF_RCP_weather_raw$Dryspell <- (-1) * DF_RCP_weather_raw$Dryspell

# 4.3.1 Country-level ------------------------------------------------------------------------------------------
# Step 1. Create out-of-sample dataset including each country
# Step 2. Forecast rice yields for each country in 2030 and 2050 under different RCP scenarios
# Step 3. Create a table / Plot a dumbbell chart

# Create an empty projection dataframe
DF_Forecast_proj_country <- setNames(data.frame(matrix(ncol = ncol(DF_Forecast_TimeTrend), 
                                                       nrow = length(2016:2100)*length(unique(DF_Forecast_TimeTrend$Country)))), 
                                                       colnames(DF_Forecast_TimeTrend))

# Year
DF_Forecast_proj_country$Year <- rep(2016:2100, times = length(unique(DF_Forecast_TimeTrend$Country)))

# Trend
DF_Forecast_proj_country$Trend <- DF_Forecast_proj_country$Year - 1960
DF_Forecast_proj_country$Sq_Trend <- DF_Forecast_proj_country$Trend^2

# Country
DF_Forecast_proj_country$Country <- rep(unique(DF_Forecast_TimeTrend$Country), each = length(2016:2100))
DF_Forecast_proj_country$FACE_China <- 0

# Irrigation rate (Assume future irrigation rate remains at the 1996 level for Syria and 2015 level for the rest of countries)
Mean_IrrRate <- aggregate(IrrRate ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_IrrRate$Country))) {
  if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == "Syria","IrrRate"] <- Mean_IrrRate[Mean_IrrRate$Year==1996 & Mean_IrrRate$Country == "Syria","IrrRate"]
  }
  else{
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == unique(Mean_IrrRate$Country)[i],"IrrRate"] <- Mean_IrrRate[Mean_IrrRate$Year==2015 & Mean_IrrRate$Country == unique(Mean_IrrRate$Country)[i],"IrrRate"] 
  }
}

# Overview the assumed irrigation rate in 2050 in each country
DF_Forecast_proj_country %>%
  filter(Year == 2050) %>%
  select(Country, Year, IrrRate) %>%
  data.frame()

# Fertilization application (Assume future fertilization application remains at the 1996 level for Syria and 2015 level for the rest of countries)
Mean_FertApp <- aggregate(FertApp ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_FertApp$Country))) {
  if(unique(Mean_FertApp$Country)[i] == "Syria"){
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == "Syria","FertApp"] <- Mean_FertApp[Mean_FertApp$Year==1996 & Mean_FertApp$Country == "Syria","FertApp"]
  }
  else{
    DF_Forecast_proj_country[DF_Forecast_proj_country$Country == unique(Mean_FertApp$Country)[i],"FertApp"] <- Mean_FertApp[Mean_FertApp$Year==2015 & Mean_FertApp$Country == unique(Mean_FertApp$Country)[i],"FertApp"] 
  }
}

# Overview the assumed fertilization application in 2050 in each country
DF_Forecast_proj_country %>%
  filter(Year == 2050) %>%
  select(Country, Year, FertApp) %>%
  data.frame()

# Country FE function
Fn_Country_dummy <- function(country_list){
  ifelse(DF_Forecast_proj_country$Country == country_list, 1, 0)
}
DF_Forecast_proj_country[,sprintf("factor(Country)%s",Lt_country_Direct)] <- sapply(Lt_country_Direct, Fn_Country_dummy)

# Separate scenarios
DF_Forecast_proj_country_RCP4.5 <- DF_Forecast_proj_country_RCP6 <- DF_Forecast_proj_country_RCP8.5 <- DF_Forecast_proj_country

# Prec
Mean_Prec <- aggregate(Prec ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_Prec$Country))) {
  if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Prec"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Prec"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","Prec"] <- Mean_Prec[Mean_Prec$Year==1996 & Mean_Prec$Country == "Syria","Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Prec"])
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2015 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Prec"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2015 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Prec"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_Prec$Country)[i],"Prec"] <- Mean_Prec[Mean_Prec$Year==2015 & Mean_Prec$Country == unique(Mean_Prec$Country)[i],"Prec"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Prec"])
  }
}

# Tmax
Mean_Tmax <- aggregate(Tmax ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_Tmax$Country))) {
  if(unique(Mean_Prec$Country)[i] == "Saudi Arabia"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Saudi Arabia","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1980 & Mean_Tmax$Country == "Saudi Arabia","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])
  }
  else if(unique(Mean_IrrRate$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","Tmax"] <- Mean_Tmax[Mean_Tmax$Year==1996 & Mean_Tmax$Country == "Syria","Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2015 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Tmax"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2015 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Tmax"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_Tmax$Country)[i],"Tmax"] <- Mean_Tmax[Mean_Tmax$Year==2015 & Mean_Tmax$Country == unique(Mean_Tmax$Country)[i],"Tmax"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Tmax"])
  }
}

# SPEI
# For the SPEI projection, go to KNMI website and choose "GCM: CMIP5 extremes (one ensemble member)" at the topic "Select a dataset and variable"
# Since SPEI is a measure of climate variability, you may replace it by a variable such as maximum length of dry spell.
Mean_SPEI <- aggregate(SPEI ~ Year + Country, FUN = mean, data = DF_Analysis_Asia)
for (i in 1:length(unique(Mean_SPEI$Country))) {
  if(unique(Mean_SPEI$Country)[i] == "Syria"){
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Dryspell"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Dryspell"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == "Syria","SPEI"] <- Mean_SPEI[Mean_SPEI$Year==1996 & Mean_SPEI$Country == "Syria","SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Dryspell"])
  }
  else{
    DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2015 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP4.5","Dryspell"])
    DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2015 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP6","Dryspell"])
    DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Country == unique(Mean_SPEI$Country)[i],"SPEI"] <- Mean_SPEI[Mean_SPEI$Year==2015 & Mean_SPEI$Country == unique(Mean_SPEI$Country)[i],"SPEI"]*(1+DF_RCP_weather_raw[DF_RCP_weather_raw$Scenarios=="RCP8.5","Dryspell"])
  }
}

# FACE
DF_Forecast_proj_country_RCP4.5$FACE_dummy <- 0
DF_Forecast_proj_country_RCP6$FACE_dummy <- 0
DF_Forecast_proj_country_RCP8.5$FACE_dummy <- 0

# CO2 projection ------------------------------------------------------------------------------------------
DF_Forecast_proj_country_RCP4.5$CO2 <- (DF_RCP_raw %>% filter(Year >=2016) %>% select(CO2.RCP4.5) %>% unlist())
DF_Forecast_proj_country_RCP6$CO2 <- (DF_RCP_raw %>% filter(Year >=2016) %>% select(CO2.RCP6) %>% unlist())
DF_Forecast_proj_country_RCP8.5$CO2 <- (DF_RCP_raw %>% filter(Year >=2016) %>% select(CO2.RCP8.5) %>% unlist())

# Apply predict(): FGLS3 ------------------------------------------------------------------------------------------ 
DF_Pred_RCP4.5 <- predict(List_SpecCheck[[9]][["FGLS3"]], newdata = DF_Forecast_proj_country_RCP4.5, type = "response", interval = "confidence", level = 0.95)
DF_Pred_RCP6 <- predict(List_SpecCheck[[9]][["FGLS3"]], newdata = DF_Forecast_proj_country_RCP6, type = "response", interval = "confidence", level = 0.95)
DF_Pred_RCP8.5 <- predict(List_SpecCheck[[9]][["FGLS3"]], newdata = DF_Forecast_proj_country_RCP8.5, type = "response", interval = "confidence", level = 0.95)

DF_Pred_RCP4.5 <- data.frame(DF_Pred_RCP4.5)
DF_Pred_RCP6 <- data.frame(DF_Pred_RCP6)
DF_Pred_RCP8.5 <- data.frame(DF_Pred_RCP8.5)

# Convert rice yields to the original unit
DF_Pred_RCP4.5[,1:3] <- exp(DF_Pred_RCP4.5[,1:3])
DF_Pred_RCP6[,1:3] <- exp(DF_Pred_RCP6[,1:3])
DF_Pred_RCP8.5[,1:3] <- exp(DF_Pred_RCP8.5[,1:3])

# Add a year column
DF_Pred_RCP4.5$Year <- 2016:2100
DF_Pred_RCP6$Year <- 2016:2100
DF_Pred_RCP8.5$Year <- 2016:2100

# Add a country column
DF_Pred_RCP4.5$Country <- DF_Forecast_proj_country_RCP4.5$Country
DF_Pred_RCP6$Country <- DF_Forecast_proj_country_RCP6$Country
DF_Pred_RCP8.5$Country <- DF_Forecast_proj_country_RCP8.5$Country

# Add a scenario column
DF_Pred_RCP4.5$Scenario <- "RCP4.5"
DF_Pred_RCP6$Scenario <- "RCP6"
DF_Pred_RCP8.5$Scenario <- "RCP8.5"

# Merge all forecast dataframe
DF_Predicted_Yields_Country <- rbind.data.frame(DF_Pred_RCP4.5, DF_Pred_RCP6, DF_Pred_RCP8.5)

# Summarize the forecasting result: Regional-level ------------------------------------------------------------------------------------------
# Aggregate the prediction result from country-level to regional-level
List_Region <- DF_Analysis_Asia %>% select(Region, Country)
List_Region <- List_Region[duplicated.data.frame(List_Region)==FALSE,]
DF_ProYields_Region <- DF_ProYields_Country
DF_ProYields_Region <- merge(List_Region, DF_ProYields_Region, by = "Country")

# Import rice production data
DF_Rice_production2015 <- DF_Rice_production %>% filter(Year == 2015 & Country %in% unique(DF_ProYields_Country$Country))
DF_Rice_production2015 <- merge(List_Region, DF_Rice_production2015, by = "Country")
DF_Rice_production_Region <- DF_Rice_production2015 %>%
  group_by(Region) %>%
  summarize(Region_Tot = sum(Production)) %>%
  data.frame()

DF_ProYields_Region <- merge(DF_ProYields_Region, DF_Rice_production2015, by = c("Region","Country"))
DF_ProYields_Region <- merge(DF_ProYields_Region, DF_Rice_production_Region, by = "Region")
DF_ProYields_Region$MktShare <- round(DF_ProYields_Region$Production/DF_ProYields_Region$Region_Tot, 4)

# Save the result as a csv file
write.csv(DF_ProYields_Region, "Projected_regions_yield2015_MktShare.csv")

DF_ProYields_Region <- DF_ProYields_Region %>%
  group_by(Scenario, Region) %>%
  summarize(Yield2015 = weighted.mean(Yields, MktShare), fit2030 = weighted.mean(fit2030, MktShare), 
            fit2050 = weighted.mean(fit2050, MktShare), fit2100 = weighted.mean(fit2100, MktShare)) %>%
  data.frame()

# Calculate yield differences in 2030 and 2050 for each country
DF_ProYields_Region$Diff2015_30 <- sprintf("%d%%",round(100*((DF_ProYields_Region$fit2030 - DF_ProYields_Region$Yield2015)/DF_ProYields_Region$Yield2015),0))
DF_ProYields_Region$Diff2015_50 <- sprintf("%d%%",round(100*((DF_ProYields_Region$fit2050 - DF_ProYields_Region$Yield2015)/DF_ProYields_Region$Yield2015),0))
DF_ProYields_Region$Diff2015_00 <- sprintf("%d%%",round(100*((DF_ProYields_Region$fit2100 - DF_ProYields_Region$Yield2015)/DF_ProYields_Region$Yield2015),0))

# Save the result as a csv file
write.csv(DF_ProYields_Region, "Projected_regions_yield2015.csv")

# Summarize the forecasting result: Country-level ------------------------------------------------------------------------------------------
# Extract the predicted result in 2030 and 2050 for each country under three different warming scenarios
DF_ProYields_Country <- DF_Predicted_Yields_Country %>%
  filter(Year %in% c(2030, 2050, 2100)) %>%
  filter(!Country %in% c("Azerbaijan","Kazakhstan","Kyrgyzstan","Syria", "Hong Kong",
                         "Saudi Arabia","Tajikistan","Turkmenistan","Uzbekistan")) %>% 
  select(fit, Year, Country, Scenario)


# Store predicted yield in 2030 and 2050
DF_ProYields_Country <- data.frame(
  Country = rep(unique(DF_ProYields_Country$Country), times=length(unique(DF_ProYields_Country$Scenario))),
  Scenario = rep(unique(DF_ProYields_Country$Scenario), each=length(unique(DF_ProYields_Country$Country))),
  fit2030 = DF_ProYields_Country[DF_ProYields_Country$Year==2030,"fit"],
  fit2050 = DF_ProYields_Country[DF_ProYields_Country$Year==2050,"fit"],
  fit2100 = DF_ProYields_Country[DF_ProYields_Country$Year==2100,"fit"]
)

# Extract yield data in 2015 for each country
DF_Benchmark_yield <- DF_Analysis_Asia %>% 
  filter(Year == 2015) %>%
  select(Country, Yields)

# Merge the benchmark data to the projected data set
DF_ProYields_Country <- merge(DF_ProYields_Country, DF_Benchmark_yield, by = "Country")

# Reorder the data set
DF_ProYields_Country <- DF_ProYields_Country[order(DF_ProYields_Country$Scenario, DF_ProYields_Country$Country),]

# Calculate yield differences in 2030 and 2050 for each country
DF_ProYields_Country$Diff2015_30 <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2030 - DF_ProYields_Country$Yields)/DF_ProYields_Country$Yields),0))
DF_ProYields_Country$Diff2015_50 <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2050 - DF_ProYields_Country$Yields)/DF_ProYields_Country$Yields),0))
DF_ProYields_Country$Diff2015_00 <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2100 - DF_ProYields_Country$Yields)/DF_ProYields_Country$Yields),0))
DF_ProYields_Country$Diff <- sprintf("%d%%",round(100*((DF_ProYields_Country$fit2050 - DF_ProYields_Country$fit2030)/DF_ProYields_Country$fit2030),0))

# Set the right ordering of countries
DF_ProYields_Country$Country <- factor(DF_ProYields_Country$Country, levels = rev(unique(DF_ProYields_Country$Country)))

# Save the result as a csv file
write.csv(DF_ProYields_Country, "Projected_countries_yield2015.csv")

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

# 5. Forecasting (out-of-sample period, changing temperature) ------------------------------------------------------------------------------------------
# There are two common ways of visually representing the substantive results of a regression model: 
# (1) fitted values plots, which display the fitted conditional mean outcome across levels of a covariate, and 
# (2) marginal effects plots, which display the estimated marginal effect of a variable across levels of a covariate. 

# 5.1 Country-level
# Step 1. Duplicate out-of-sample dataset including each country from 4.3
# Step 2. Modify projection values for temperature and CO2
# Step 3. Calculate weighted average Asian rice yield
# Step 4. Visualize time trends

# Figure 2.a Impact of maximum temperature on rice yields ------------------------------------------------------------------------------------------
# Duplicate forecast use input data set for RCP8.5
DF_Temp_varying <- DF_Forecast_proj_country_RCP8.5
  
# Extract the min and max temperature in 2015 for temperature projection range
DF_Temp_varying$Tmax <- rep(seq(min(DF_Analysis_Asia[DF_Analysis_Asia$Year==2015,"Tmax"]),
                                max(DF_Analysis_Asia[DF_Analysis_Asia$Year==2015,"Tmax"]),length.out =length(2016:2100)),
                            times = length(unique(DF_Temp_varying$Country)))

# Estimate mean rice yields conditional on Tmax
DF_Pred_Temp_varying <- cplot(List_SpecCheck[[9]][["FGLS3"]], "Tmax", what = "prediction", data = DF_Temp_varying)
DF_Pred_Temp_varying <- data.frame(DF_Pred_Temp_varying)

# Convert rice yields to the original unit
DF_Pred_Temp_varying[,c("yvals","upper","lower")] <- exp(DF_Pred_Temp_varying[,c("yvals","upper","lower")])

# Text dataframe
DF_vline_Temp <- data.frame(xvals = c(mean((DF_Analysis_Asia[DF_Analysis_Asia$Year==2015,"Tmax"])),
                                     mean((DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Year==2100,"Tmax"])),
                                     mean((DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Year==2100,"Tmax"])),
                                     mean((DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Year==2100,"Tmax"]))),
                           Scenario = c("Avg. temperature in 2015", "Avg. temperature in 2100 under RCP4.5", 
                                        "Avg. temperature in 2100 under RCP6", "Avg. temperature in 2100 under RCP8.5"))
DF_vline_Temp$Scenario <- factor(DF_vline_Temp$Scenario, levels = c("Avg. temperature in 2015", "Avg. temperature in 2100 under RCP4.5", 
                                                                    "Avg. temperature in 2100 under RCP6", "Avg. temperature in 2100 under RCP8.5"))

# Visualize the projection result 
DF_Pred_Temp_varying %>% 
  ggplot(aes(x=xvals, y=yvals)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "red", alpha=0.2) +
  geom_line(size = 1.5, alpha=0.6, color = "red") + 
  geom_vline(data = DF_vline_Temp, aes(xintercept = xvals, color = Scenario), size = 1.5, linetype = "dashed") +
  scale_color_manual(values = c("gray40","#0171CE","olivedrab4","#DE4433")) + 
  labs(y = "Rice yield (ton/hectare)", x = "Mean max. temperature (°C)", color = "Scenarios") + 
  scale_y_continuous(limits = c(0,50)) +
  theme_bw() +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "top", legend.title = element_text(size=18, face="bold"), legend.text = element_text(size=20),
        legend.direction = "horizontal", legend.key.width = unit(0.25,"cm"), text = element_text(size=24))

# Figure 2.b Impact of CO2 concentration on rice yields ------------------------------------------------------------------------------------------
# Duplicate forecast use input data set for RCP8.5
DF_CO2_varying <- DF_Forecast_proj_country_RCP8.5

# Extract the min and max CO2 for CO2 projection range
DF_CO2_varying$CO2 <- rep(seq(300, 1000,length.out =length(2016:2100)),
                          times = length(unique(DF_CO2_varying$Country)))

# Estimate mean rice yields conditional on CO2
DF_Pred_CO2_varying <- cplot(List_SpecCheck[[9]][["FGLS3"]], "CO2", what = "prediction", data = DF_CO2_varying)
DF_Pred_CO2_varying <- data.frame(DF_Pred_CO2_varying)

# Convert rice yields to the original unit
DF_Pred_CO2_varying[,c("yvals","lower","upper")] <- exp(DF_Pred_CO2_varying[,c("yvals","lower","upper")])

# Text dataframe
DF_vline_CO2 <- data.frame(xvals = c(mean(DF_Analysis_Asia[DF_Analysis_Asia$Year==2015,"CO2"]),
                                      mean(DF_Forecast_proj_country_RCP4.5[DF_Forecast_proj_country_RCP4.5$Year==2100,"CO2"]),
                                      mean(DF_Forecast_proj_country_RCP6[DF_Forecast_proj_country_RCP6$Year==2100,"CO2"]),
                                      mean(DF_Forecast_proj_country_RCP8.5[DF_Forecast_proj_country_RCP8.5$Year==2100,"CO2"])),
                            Scenario = c("CO2 concentration in 2015", "CO2 concentration in 2100 under RCP4.5", 
                                         "CO2 concentration in 2100 under RCP6", "CO2 concentration in 2100 under RCP8.5"))
DF_vline_CO2$Scenario <- factor(DF_vline_CO2$Scenario, levels = c("CO2 concentration in 2015", "CO2 concentration in 2100 under RCP4.5", 
                                                                  "CO2 concentration in 2100 under RCP6", "CO2 concentration in 2100 under RCP8.5"))

# Visualize the forecasting result 
DF_Pred_CO2_varying %>% 
  
  ggplot(aes(x=xvals, y=yvals)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
  geom_line(size = 1.5, alpha=0.6) + 
  geom_vline(data = DF_vline_CO2 , aes(xintercept = xvals, color = Scenario), linetype = "dashed", size = 1.5) +
  scale_color_manual(values = c("gray40","#0171CE","olivedrab4","#DE4433")) + 
  labs(y = "Rice yield (ton/hectare)", x = "CO2 (ppm)") + 
  scale_x_continuous(breaks = seq(300,1000, by=100)) +
  scale_y_continuous(limits = c(0,50)) +
  theme_bw() +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "top", legend.title = element_text(size=18, face="bold"), legend.text = element_text(size=20),
        legend.direction = "horizontal", legend.key.width = unit(1,"cm"), text = element_text(size=24))

# Figure 4. Projected Asian rice yields under different climate scenarios ------------------------------------------------------------------------------------------ 
# Duplicate forecasting training data set
DF_Forecast_Yields <- DF_Predicted_Yields_Country

# Calculate aggregate weighted average yields based on 2015 rice production level
DF_Rice_Shares <- DF_Rice_production %>% filter(Year == 2015 & Country %in% unique(DF_Forecast_Yields$Country))
DF_Rice_Shares[DF_Rice_Shares$Country == "Syria", "Production"] <- 0
DF_Rice_Shares <- DF_Rice_Shares %>%
  mutate(Asia_Tot = sum(Production), MktShare = round(Production/Asia_Tot,4)) %>%
  dplyr::select(Country, MktShare) %>%
  data.frame()

# Merge forecast yield training data set and rice market share
DF_Forecast_Yields <- merge(DF_Forecast_Yields, DF_Rice_Shares, by = "Country")

# Set the order for the model type
DF_Forecast_Yields$Scenario <- factor(DF_Forecast_Yields$Scenario, 
                                       levels = rev(c("RCP4.5", "RCP6", "RCP8.5")))

# Calculate forecasting weighted average
DF_Forecast_Yields <- DF_Forecast_Yields %>% 
  group_by(Year, Scenario) %>%
  summarize(fit = weighted.mean(fit, MktShare), 
            lwr = weighted.mean(lwr, MktShare), 
            upr = weighted.mean(upr, MktShare))

# Merge historical data set and rice market share
DF_Hist_Yields <- merge(DF_Analysis_Asia_NOFACE %>% dplyr::select(Country,Year,Yields), DF_Rice_Shares, by = "Country")

# Calculate historical weighted average
DF_Hist_Yields <- DF_Hist_Yields %>%
  group_by(Year) %>%
  summarize(sd = sd(Yields), fit = weighted.mean(Yields, MktShare)) %>%
  mutate(lwr = fit-1.96*sd, upr = fit+1.96*sd) %>%
  dplyr::select(-sd)

# Manually fill the value of 2016 for connecting historical and forecasting lines
DF_Hist_Yields <- rbind.data.frame(DF_Hist_Yields, data.frame(Year = 2016, fit = NA, lwr = NA, upr = NA))
DF_Hist_Yields[DF_Hist_Yields$Year==2016, "fit"] <- DF_Forecast_Yields[DF_Forecast_Yields$Year==2016 & DF_Forecast_Yields$Scenario=="RCP6", "fit"]
DF_Hist_Yields[DF_Hist_Yields$Year==2016, "lwr"] <- DF_Forecast_Yields[DF_Forecast_Yields$Year==2016 & DF_Forecast_Yields$Scenario=="RCP6", "lwr"]
DF_Hist_Yields[DF_Hist_Yields$Year==2016, "upr"] <- DF_Forecast_Yields[DF_Forecast_Yields$Year==2016 & DF_Forecast_Yields$Scenario=="RCP6", "upr"]

# Text data frame 
DF_text <- data.frame(Year = 1985, fit = 7, text = "Historical Asian rice yield")

# Visualize the forecasting result (weighted average)
ggplot() +
  
  # Historical
  geom_line(data = DF_Hist_Yields, aes(x = Year, y = fit), size = 1.5, color = "gray50") + 
  geom_vline(xintercept = 2016, linetype = "dashed", color = "gray40", size = 0.8) +
  geom_text(data = DF_text, aes(x = Year, y = fit, label=text), color = "gray40", size = 8) +
  
  # Forecasting 
  geom_ribbon(data = DF_Forecast_Yields, aes(x = Year, ymin = lwr, ymax = upr, fill = Scenario, color = NULL), alpha=0.1) +
  geom_line(data = DF_Forecast_Yields, aes(x = Year, y = fit, color = Scenario), size = 1.5, alpha=0.8) + 
  
  scale_color_manual(values = c("#DE4433", "olivedrab4","#0171CE")) + 
  scale_fill_manual(values = c("#DE4433", "olivedrab4","#0171CE")) + 
  scale_x_continuous(breaks = seq(1960,2100, by = 20)) +
  scale_y_continuous(breaks = seq(0,80, by = 10), limits = c(0,80)) +
  labs(y = "Rice yield (ton/hectare)", color="Scenario categories", fill="Scenario categories") + 
  theme_bw() +
  theme(legend.position = c(0.1325,0.9155), legend.title = element_text(size=20, face="bold"), legend.text = element_text(size=20),
        legend.background = element_rect(fill="white", size=1, linetype="solid", color ="gray50"), 
        legend.key.width = unit(3,"cm"), text = element_text(size=28))
