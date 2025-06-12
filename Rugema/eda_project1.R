# Maternal healthcare disparities

# Data Preparation
library(tidyverse)
library(ggcorrplot)
library(cowplot)
theme_set(theme_light())
maternal <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/maternal.csv")

# Change PriorBirthsNowDeceased to numeric
maternal$PriorBirthsNowDeceased <- as.numeric(maternal$PriorBirthsNowDeceased)

# Handle numerical missing values
colSums(is.na(maternal))
maternal <- maternal |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Check and remove Unknowns
colSums(maternal == "Unknown", na.rm = TRUE)
maternal <- maternal |>
  filter_all(all_vars(. != "Unknown"))

# Exploratory Data Analysis
summary(maternal)

# Question1:
# Does an increased number of prenatal visits correlate with risk 
# factors (tobacco use, diabetes, hypertension)?
maternal_visit <- maternal |>
  pivot_longer(
    cols = c(TobaccoUse, 
             PrePregnancyDiabetes, 
             PrePregnancyHypertension),
    names_to = "risk_type",
    values_to = "risk_value"
  )
maternal_visit |> 
  ggplot(aes(x = risk_value,
             y = AverageNumberPrenatalVisits,
             fill = risk_value)) +
  geom_bar(stat = "summary", fun = mean) +
  facet_wrap(~ risk_type, nrow = 1) + 
  labs(
    title = "Average Prenatal Visits by Risk Factor",
    x = "Risk Group",
    y = "Average Prenatal Visits"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.spacing.x = unit(1, "cm"),
        plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"))

# Question2:
# Are there strong correlations among the numeric variables in the data-set? 
# If so, what are the most important ones, and how might they be related?

# First let's have a correlation heat-map to tell us variables that correlated
corr_data <- maternal |>
  select(where(is.numeric))

# Compute the correlation matrix  
corr_matrix <- cor(corr_data, use = "complete.obs")
ggcorrplot(corr_matrix,
           method = "circle",
           lab = TRUE, 
           type = "lower", 
           title = "Correlation Heatmap",
           colors = c("red", "white", "blue")) +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"))

# Plot 1: Mother's Age vs Pre-Pregnancy BMI
plot1 <- maternal |>
  ggplot(aes(x = AverageMotherAge, 
             y = AveragePrePregnancyBMI)) +
  geom_point(alpha = 0.5, color = "royalblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "Mother's Age vs Pre-Pregnancy BMI",
    x = "Average Mother Age",
    y = "Average Pre-Pregnancy BMI"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"))

# Plot 2: BMI vs Prenatal Visits
plot2 <- maternal |>
  ggplot(aes(x = AveragePrePregnancyBMI, 
             y = AverageNumberPrenatalVisits)) +
  geom_point(alpha = 0.5, color = "seagreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(
    title = "BMI vs Prenatal Visits",
    x = "Average Pre-Pregnancy BMI",
    y = "Average Number of Prenatal Visits"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 14,
                                  face = "bold"))

plot_grid(plot1, plot2, nrow = 2)

# Clustering analysis
# Do certain states form similar risk profiles?
# Principal Component Analysis
# Binary encoding 
maternal <- maternal |>
  mutate(across(c(TobaccoUse, 
                   PrePregnancyDiabetes, 
                   PrePregnancyHypertension), ~ as.numeric(. == "Yes")))

risk_profile_vars <- maternal |>
  select(-State, -Births, -id)

# Apply PCA
maternal_pca <- prcomp(risk_profile_vars, center = TRUE, scale. = TRUE)

# Create a scree plot
maternal_pca |>
  fviz_eig(addlabels = TRUE) +
  geom_hline(
    yintercept = 100 * (1 / ncol(maternal_pca$x)),
    linetype = "dashed",
    color = "darkred"
  )

# Apply K-Means clustering








