
# # Load essential packages for GAM analysis and data visualization -------
#Load all the necessary libraries for data manipulation
#statistical modeling, and visualization 
library(mgcv)            # Generalized Additive Models (GAMs)
library(MASS)            # Statistical functions and datasets
library(stringr)         # String manipulation functions
library(gamm4)           # GAMM models using lme4
library(tidyr)           # Data tidying
library(ggplot2)         # Data visualization
library(ggthemes)        # Additional themes for ggplot2
library(viridis)         # Color palettes
library(cowplot)         # Combining ggplot objects
library(kableExtra)      # Table formatting
library(knitr)           # Dynamic report generation
library(tibble)          # Enhanced data frames
library(dplyr)           # Data manipulation
library(gratia)          # Tools for GAM visualization and diagnostics
library(latex2exp)       # Use LaTeX in plots
library(itsadug)         # Tools for interpreting time series GAMMs
library(ggdist)          # Visualizing distributions and uncertainty


# Load multiple packages at once (for streamlined loading)
pkgs <- c("mgcv", "gamair", "gratia", "ggplot2", "dplyr", "ggdist")
loaded <- vapply(pkgs, library, logical(1L), logical.return = TRUE, character.only = TRUE)

# Load example dataset from gamair package
data(chl, package = "gamair")



# # Data Import and Preparation -------------------------------------------
# Merging datasets to create a final dataframe for analysis
# Set working directory and load data for Tmax periods
setwd("H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Output2/Tmax")
df1 <- read.table("PeriodsAverages.csv", sep = ",", header = TRUE)

# Rename column for consistency
df1new <- df1 %>% rename(concat_col69 = concat_col99)

# Load second dataset containing specimen data
setwd("H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Output2/FIN_dfs/")
df2 <- read.table("newdfALL.csv", sep = ",", header = TRUE)

# Create a concatenated column for merging datasets
df2$concat_col69 <- paste0(df2$creatingFilename.x, df2$DEC_LAT.x, df2$DEC_LONG.x)

# Merge datasets based on common key
left_join <- merge(df2, df1new, by = "concat_col69", all.x = TRUE)

# Export merged data for further analysis
write_csv(left_join, "H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Output2/FIN_dfs/newdfALL_tmax_Periods.csv")



# # Data Cleaning and Preprocessing ---------------------------------------
# Filtering the dataset to focus on relevant records and preparing variables
# Load the merged dataset
df <- read.table("newdfALL_tmax_Periods.csv", sep = ",", header = TRUE)
nrow(df)  # Check the number of rows

# Filter dataset for BIOME analysis
df <- read.table("newdfALL_tmax_Periods_fewerBIOMES.csv", sep = ",", header = TRUE)
nrow(df)

# Keep only records where male-female comparison is equal
df <- df %>% dplyr::filter(male_female_comparison == "Equal")
nrow(df)

# Load dataset with BIOME index
df <- read.table("newdfALL_tmax_Periods_BiomeYear_index.csv", sep = ",", header = TRUE)

# Clean the Year column (remove extra characters and convert to integer)
df$Year.x <- sub('.', '', df$Year.x)
df$Year.x <- as.integer(df$Year.x)


# Handling Missing Data
# Using the missRanger package for imputation of missing values
library(missRanger)
# Impute missing values for temperature variables
variables_to_impute <- c("tmax_chelsa", "Per1_chel", "Per2_chel", "Per3_chel", "Per4_chel",
                         "Per1_wc", "Per2_wc", "Per3_wc", "Per4_wc")

# Loop through variables to apply imputation
for (var in variables_to_impute) {
  df <- missRanger(df, formula(paste0(var, " ~ ", var)), 
                   pmm.k = 2, num.trees = 100, 
                   case.weights = rowSums(!is.na(df)))
}


# # Data Housekeeping -----------------------------------------------------
# Converting variables to appropriate data types for analysis
# Check and convert data types
str(df$length_cor)      # Response variable
df$BIOME <- as.factor(df$BIOME)          # Convert BIOME to factor
df$Family.x <- as.factor(df$Family.x)    # Convert Family to factor
df$GenusSpecies <- as.factor(df$GenusSpecies)  # Convert GenusSpecies to factor

# Filter data for specific years
df <- df[df$Year.x >= 1900 & df$Year.x <= 2000, ]


# Initial Data Exploration
# Visualizing trends, checking for outliers, and identifying potential patterns
# Cleveland Dotchart for identifying outliers
plot(y = 1:nrow(df), x = df$Year.x, 
     xlab = "Year", ylab = "Observation Order",
     pch = 16, cex = 0.7)

# Subset data from 1900 to 2010 for further analysis
df <- df[df$Year.x >= 1900 & df$Year.x <= 2010, ]

# Replot after subsetting
plot(y = 1:nrow(df), x = df$Year.x, 
     xlab = "Year", ylab = "Observation Order",
     pch = 16, cex = 0.7)

# 1. Exploring Wing Length Trends Over Time
# Scatterplot of wing length over years
par(mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(y = df$length_cor, 
     x = df$Year.x,
     xlab = "Year",
     ylab = "Wing Length")
#This plot helps to visually identify if there are 
#any apparent trends in wing length across different years

# 2. Adding a Smoother to Detect Non-linear Trends
# Apply LOESS smoother to observe trends over time
M1 <- loess(length_cor ~ Year.x, data = df)
MyData1 <- data.frame(Year.x = seq(1900, 2020))
P1 <- predict(M1, newdata = MyData1)

# Add the smoothing line to the scatterplot
lines(x = MyData1$Year.x, 
      y = P1, 
      lwd = 5)
#The LOESS smoother captures non-linear relationships 
#between wing length and time, highlighting subtle changes

# 3. Boxplots of Wing Length Across Years
par(mar = c(5, 5, 2, 2), cex.lab = 1.5)
boxplot(right_wing_span ~ Year.x, 
        data = df,
        xlab = "Year", 
        ylab = "Wing Length",
        varwidth = TRUE)
#The boxplot visualizes the distribution of wing length for each year, 
#with box widths proportional to the number of observations

# 4. Wing Length Variation Across Biomes Over Time
xyplot(right_wing_span ~ BIOME | factor(Year.x),
       col = 1, 
       data = df,
       cex = 0.5,
       xlab = list("BIOME Category", cex = 1.5),
       ylab = list("Wing Length", cex = 1.5),
       strip = function(bg = 'white', ...) strip.default(bg = 'white', ...),
       scales = list(alternating = TRUE,
                     x = list(relation = "same"),
                     y = list(relation = "same")), 
       panel = function(x, y) {
         panel.grid(h = -1, v = 2)
         panel.points(x, y, col = 1)
       })
# This lattice plot examines how wing length varies across different biomes over time



# 5. Species-Level Analysis of Wing Length
# Boxplot of wing length for each species
par(mar = c(5, 5, 2, 2), cex.lab = 1.5)
boxplot(right_wing_span ~ GenusSpecies, 
        data = df,
        xlab = "Species",
        ylab = "Wing Length",
        varwidth = TRUE)
# Species-specific boxplots help determine if wing length differences are 
#influenced by species identity
#This justifies the inclusion of species 
#as a random effect in models


# 6. Visualizing Temporal Distribution of Categorical Variables
# Plotting temporal distribution of Family.x
par(mar = c(5, 5, 2, 2), cex.lab = 1.5)
plot(x = jitter(df$Year.x), 
     y = jitter(as.numeric(df$Family.x)),
     xlab = "Year",
     ylab = "Family (jittered)")

# Add LOESS smoother to check trends in Family over time
M1 <- loess(as.numeric(Family.x) ~ Year.x, data = df)
MyData1 <- data.frame(Year.x = seq(1900, 2020))
P1 <- predict(M1, newdata = MyData1)
lines(x = MyData1$Year.x, 
      y = P1, 
      lwd = 5)
# This checks if changes in family composition over time might confound the analysis


# 7. Scatterplots for Key Variable Relationships
# Explore relationships between wing length and other variables
scatterplot(df$length_cor ~ df$Year.x, data = df, spread = FALSE)
scatterplot(df$right_wing_span ~ df$Family.x, data = df, spread = FALSE)
scatterplot(df$right_wing_span ~ df$tmax_wc, data = df, spread = FALSE)
scatterplot(df$right_wing_span ~ df$tmax_chelsa, data = df, spread = FALSE)
scatterplot(df$tmax_wc ~ df$Year.x, data = df, spread = FALSE)
scatterplot(df$tmax_chelsa ~ df$Year.x, data = df, spread = FALSE)
#Identify potential multicollinearity or interactions 
#between variables like temperature, year, and wing length




# # Model Formulation -----------------------------------------------------

# Defining the modeling framework based on biological hypotheses
# 8. Model Assumptions and Considerations
# Biological Question:
# Is there a long-term temporal trend in wing length, 
# accounting for other covariates?
# Response Variable:
# Wing length (continuous and strictly positive)

# Distribution Choice:
# - Start with Gaussian linear regression
# - If negative fitted values occur, consider Gamma distribution with log link

# Random Effects:
# - Species identity (GenusSpecies) as a random intercept
# Key Considerations:
#   
# Temporal Trends: Focus on long-term changes in wing length
# Species Effect: Individuals from the same species may exhibit similar wing lengths, requiring mixed-effects models
# Modeling Approach: Generalized Additive Models (GAMs) with random intercepts to capture hierarchical structure


# Model 1: Temporal Trends in Wing Length
# Model 1 - Baseline Model
# Filtering data between 1900 and 2010
df <- df[df$Year.x >= "1900" & df$Year.x <= "2010", ]

# Fitting Model 1 using a Generalized Additive Model (GAM)
mfin5 <- bam(length_cor ~ s(Year.x, k = 5, bs = "tp") + 
               BIOME + 
               s(GenusSpecies, bs = "re"),
             method = "REML", 
             data = df)

# Saving the model output
save(mfin5, file = "H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Model_output/mfin5.rda")   
# Response Variable: length_cor (corrected wing length)
# Predictors:
# s(Year.x, k = 5, bs = "tp"): Smooth term for year with thin plate regression splines
# BIOME: Fixed effect representing different biomes.
# s(GenusSpecies, bs = "re"): Random intercept for species to account for species-specific effects
# Method: Restricted Maximum Likelihood (REML) for smoothness estimation


# 2. Model Summary and Visualization
# Summary of the model
summary(mfin5)

# Visualizing the smooth term for Year
plot(mfin5, select = 1, shade = TRUE)
abline(h = 0, lty = 'dashed')

# Advanced visualization
draw(mfin5)
# Identifies the temporal trend in wing length over the years
# Dashed Line: Reference line to evaluate deviations in the smooth function

# 3. Model Diagnostics
# Basic diagnostics
gam.check(mfin5)

# Advanced model appraisal
appraise(mfin5, method = "simulate")

# Check the smoothness of the fitted model
k.check(mfin5)
# Ensures model assumptions (residual distribution, smoothness) are met



# # Model 1 with Nested Random Effects ---------------------------
# Model 1 -Adding nested random effects for species within years
mfin5_nested <- bam(length_cor ~ s(Year.x, k = 5, bs = "tp") + 
                      BIOME + 
                      s(GenusSpecies, bs = "re") + 
                      s(GenusSpecies, by = Year.x, bs = "re"),
                    method = "REML", 
                    data = df)
# Key Changes
# Introduces an interaction between GenusSpecies and Year.x to account 
#for species-specific temporal variability
# Saving the nested model
save(mfin5_nested, file = "H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Model_output/mfin5_nested.rda")
# Model summary and visualization
summary(mfin5_nested)
draw(mfin5_nested)

# Diagnostics
gam.check(mfin5_nested)
appraise(mfin5_nested, method = "simulate")



# # Model 1 - Equal Nested Model ---------------------------------------
# Model 1 - Equal (forewing equal between females/males) nested model for comparison
mfin5_equal_nested <- bam(length_cor ~ s(Year.x, k = 5, bs = "tp") + 
                            BIOME + 
                            s(GenusSpecies, bs = "re") + 
                            s(GenusSpecies, by = Year.x, bs = "re"),
                          method = "REML", 
                          data = df)

# Save the model output
save(mfin5_equal_nested, file = "H:/My Drive/Greece/A_ELIDEK_mine/WP3_DATA ANALYSIS/Model_output/mfin5_equal_nested.rda")
# Model evaluation
summary(mfin5_equal_nested)
draw(mfin5_equal_nested, page = 1)

# Diagnostics
gam.check(mfin5_equal_nested)
appraise(mfin5_equal_nested, method = "simulate")


# # Figure 2: Partial Effect of Year --------------------------------------
# Partial effect plot with confidence intervals
sm |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "red") +
  geom_line(colour = "red", linewidth = 1) +
  labs(y = "Partial Effect", 
       title = expression("Partial Effect of" ~ f(Year[i])), 
       x = expression(Year[i])) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth = 0.5) +
  geom_vline(xintercept = c(1935, 1950, 1970), colour = 'grey', linewidth = 0.01)
  
# Shows how the partial effect of time (Year.x) influences wing length
# Shaded region represents the confidence interval



#Figure 3: First Derivative (Slope) of the Temporal Trend
plot_slopes(mfin5_nested, variables = 'Year.x',
            condition = 'Year.x',
            type = 'link') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme_bw() +
  labs(y = "1st Derivative of the Linear Predictor") +
  theme(axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
# 1st Derivative: Indicates the rate of change in wing length over time
# Interpretation: A derivative above zero implies an increasing trend
#below zero indicates a decreasing trend

  

# # Model 2: Temporal Trends with BIOME-Specific Effects ------------------
# Nested Random Effects Model
# Adding nested random effects to capture within-species variability over time
modelG3_nested <- bam(length_cor ~ s(Year.x, by = BIOME, bs = "tp", k = 5) + 
                        BIOME + 
                        s(GenusSpecies, bs = "re") + 
                        s(GenusSpecies, by = Year.x, bs = "re"),
                      data = df, 
                      method = "REML")

# Model summary and visualization
summary(modelG3_nested)
draw(modelG3_nested)
# account for hierarchical data structure (species nested within years)
# Interpretation: Provides more refined estimates by controlling for species-specific random effects


# Visualization of Interaction Effects
# Visualizing BIOME-specific smooth interactions
vis.gam(modelG3_nested, view = c("Year.x", "BIOME"), plot.type = "contour", color = "topo")
# Visualizes the interaction between Year.x and BIOME 
#using contour plots to highlight non-linear effects


# Post-Hoc Comparisons (Contrasts Between BIOMEs)
# Comparing effects between different BIOMEs
wald_gam(modelG3_nested, comp = list(BIOME = levels(df$BIOME)))
wald_gam(modelG3_nested, comp = list(BIOME = c("1", "4", "11", "12", "13")))

# Differences in smooths between BIOMEs 1 and 4
diff <- get_difference(modelG3_nested, comp = list(BIOME = c("1", "4")), 
                       cond = list(Year.x = seq(1850, 2010, length = 100)))

# Plotting the differences
plot_diff(modelG3_nested, view = "Year.x", comp = list(BIOME = c("1", "4")), 
          n.grid = length(unique(df$Year.x)), rm.ranef = FALSE)
# identify significant differences in trends between BIOMEs



# # Model for EQUAL Wing Lengths ------------------------------------------
modelG3_equal <- bam(length_cor ~ s(Year.x, by = BIOME, bs = "tp", k = 5) + 
                       BIOME + 
                       s(GenusSpecies, bs = "re"),
                     data = df, 
                     method = "REML")

# Diagnostics and Visualization
summary(modelG3_equal)
draw(modelG3_equal)
gam.check(modelG3_equal)
appraise(modelG3_equal)
# Focuses on a subset where wing lengths are equal to check for sexual dimorphism



# # Aggregated BIOMEs model ------------------------------------------
modelG3_pairedbiome <- bam(length_cor ~ s(Year.x, by = BIOME2, bs = "tp", k = 5) + 
                             BIOME2 + 
                             s(GenusSpecies, bs = "re"),
                           data = df, 
                           method = "REML")

# Model summary and diagnostics
summary(modelG3_pairedbiome)
draw(modelG3_pairedbiome)
gam.check(modelG3_pairedbiome)
appraise(modelG3_pairedbiome)
# Purpose: Combines similar BIOMEs into BIOME2 for more generalized trends


# Visualization of Partial Effects by BIOME
# #Figure 5 ---------------------------------------------------------------
# smooth labels as they are known to mgcv 
smooths(modelG3_nested)
# [1] "s(Year.x):BIOME1"  "s(Year.x):BIOME4"  "s(Year.x):BIOME5"  "s(Year.x):BIOME6" 
# [5] "s(Year.x):BIOME8"  "s(Year.x):BIOME9"  "s(Year.x):BIOME10" "s(Year.x):BIOME11"
# [9] "s(Year.x):BIOME12" "s(Year.x):BIOME13" "s(GenusSpecies)"

#define which one you plot
sm5a <- smooth_estimates(modelG3_nested, smooth = "s(Year.x):BIOME1")
#example("hcl.colors")
#hcl.pals("sequential")
#palette("Heat", "Terrain")
#then plot in ggplot2
plot5a <- sm5a |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "green"
  ) +
  geom_line(colour = "green", linewidth = 1) +
  theme_bw() +
  labs(
    y = "Partial effect", title = "1- Tropical & Subtropical Moist Broadleaf Forests",
    #subtitle = expression("Partial effect of" ~ f(Year[i])),
    x = expression(Year[i])
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth =0.5) +
  #geom_vline(xintercept = c(1926, 1970), colour = 'red4', linetype = 'dashed',linewidth = 0.01) +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme (axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 10)  # Adjust the title size here
  )


#Figure 5b
#define which one you plot
sm5b <- smooth_estimates(modelG3_nested, smooth = "s(Year.x):BIOME4")

#then plot in ggplot2
plot5b <- sm5b |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "Forest Green"
  ) +
  geom_line(colour = "Forest Green", linewidth = 1) +
  theme_bw() +
  labs(
    y = "Partial effect", title = "4- Temperate Broadleaf & Mixed Forests",
    #subtitle = expression("Partial effect of" ~ f(Year[i])),
    x = expression(Year[i])
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth =0.5) +
  #geom_vline(xintercept = c(1922, 1950), colour = 'red4', linewidth = 0.01,linetype = 'dashed')+
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme (axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 10)  # Adjust the title size here
  )

#Figure 5c
#define which one you plot
sm5c <- smooth_estimates(modelG3_nested, smooth = "s(Year.x):BIOME11")

#then plot in ggplot2
plot5c <- sm5c |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "#A2B9BC"
  ) +
  geom_line(colour = "#A2B9BC", linewidth = 1) +
  theme_bw() +
  labs(
    y = "Partial effect", title = "11- Tundra",
    #subtitle = expression("Partial effect of" ~ f(Year[i])),
    x = expression(Year[i])
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth =0.5) +
  #geom_vline(xintercept = c(1940), colour = 'red4', linewidth = 0.01,linetype = 'dashed')+
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme (axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 10)  # Adjust the title size here
  )


#Figure 5d
sm5d <- smooth_estimates(modelG3_nested, smooth = "s(Year.x):BIOME12")

#then plot in ggplot2
plot5d <- sm5d |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "red"
  ) +
  geom_line(colour = "red", linewidth = 1) +
  theme_bw() +
  labs(
    y = "Partial effect", title = "12- Mediterranean Forests, Woodlands & Scrub",
    #subtitle = expression("Partial effect of" ~ f(Year[i])),
    x = expression(Year[i])
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth =0.5) +
  #geom_vline(xintercept = c(1973), colour = 'red4', linewidth = 0.01, linetype = 'dashed') +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme (axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 10)  # Adjust the title size here
  )


#Figure 5e
sm4e <- smooth_estimates(modelG3_nested, smooth = "s(Year.x):BIOME13")

#then plot in ggplot2
plot5e <- sm5e |>
  add_confint() |>
  ggplot(aes(y = .estimate, x = Year.x)) +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
              alpha = 0.2, fill = "#F4A6A6"
  ) +
  geom_line(colour = "#F4A6A6", linewidth = 1) +
  theme_bw() +
  labs(
    y = "Partial effect", title = "13- Deserts & Xeric Shrublands",
    #subtitle = expression("Partial effect of" ~ f(Year[i])),
    x = expression(Year[i])
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black', linewidth =0.5) +
  theme(axis.title.x = element_text(size = 10), 
        axis.title.y = element_text(size = 10)) +
  theme (axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         plot.title = element_text(size = 10)  # Adjust the title size here
  )

#install.packages("cowplot")
library(cowplot)
plot_grid(plot4a, plot4b, plot4c, plot4d, plot4e, ncol = 2)

