library(readxl)

data <- Nigeria_R3_Data_Simple <- 
  read_excel("C:/Users/sssan/OneDrive/Desktop/VN-BIOSTAT 2025/Nigeria-R3-Data-Simple.xlsx")

# Day 2

#Discuss possible missing data mechanisms with a clinician (Dr. Aliyu). Based on our discussions, fit models for the missing data. Use inverse probability weighted methods to estimate the mean CD4 count and the proportion of people with 0, 1, and 2 risk alleles. Compute 95% confidence intervals for these quantities using the bootstrap.

summary(data$cd4)

#Create an indicator (dummy) variable r1 to represent whether a CD4 measurement is observed (r1 = 1 if observed, 0 if missing)
data$r1 <- ifelse(!is.na(data$cd4), 1, 0)
table(data$r1)

# Fit a model to estimate the probability that CD4 measurement is observed (i.e., r1 = 1).
mod.prob.cd4 <- glm(r1 ~ years.ART, data, family = binomial)

#Compute the predicted probability that cd4 is observed (r1=1). 
pred.prob.cd4 <- predict(mod.prob.cd4, type="response")  

data$ipw.cd4 <- 1/pred.prob.cd4

mean.cd4.ipw <- with(data, sum((r1*cd4*ipw.cd4), na.rm=TRUE)/sum((r1*ipw.cd4), na.rm=TRUE))

print(c("Mean of IPW cd4" = mean.cd4.ipw, 
        "Mean of unweighted cd4" = mean(data$cd4, na.rm = TRUE)))


# Compute 95% Confidence Intervals Using Bootstrap

# Set the number of bootstrap iterations
nboot <- 1000  

# Initialize storage for bootstrapped mean values
mean.cd4.ipw_boot <- numeric(nboot)  

for (i in 1:nboot) {
  # Generate bootstrap sample
  bootsamp <- sample(1:nrow(data), nrow(data), replace = TRUE)  
  boot_data <- data[bootsamp, ]  
  
  # Estimate probability of CD4 being observed (r = 1)
  mod.prob.cd4.b <- glm(r1 ~ years.ART, family = "binomial", data = boot_data)
  pred.prob.cd4.b <- predict(mod.prob.cd4.b, type = "response")
  
  boot_data$ipw.cd4.b <- 1/pred.prob.cd4.b
  
  
  # Estimate Mean of CD4 Using IPW  
  mean.cd4.ipw_boot[i] <- with(boot_data, sum((r1 * cd4 * ipw.cd4.b), na.rm = TRUE) / 
                                 sum((r1 * ipw.cd4.b), na.rm = TRUE))
}

# Check results
summary(mean.cd4.ipw_boot)


# Compute CI Using Normal Approximation (Standard Error)
se_est <- sd(mean.cd4.ipw_boot)  # Bootstrapped standard error

lower_bound_norm <- mean.cd4.ipw - 1.96 * se_est
upper_bound_norm <- mean.cd4.ipw + 1.96 * se_est

print(c(lower_bound_norm, upper_bound_norm))  # Display 95% CI


#Estimate Proportions for Risk Alleles with IPW

print("Unweighted proportions");prop.table(table(data$risk.alleles))

#Create an indicator (dummy) variable r2 to represent whether the variable risk.alleles is observed.
data$r2 <- ifelse(is.na(data$risk.alleles), 0, 1)
table(data$r2)

library(splines)
#Estimate the probability that risk.alleles is observed (r2=1) 
mod.prob.risk.alleles <- glm(r2 ~ ns(bmi,df=4), data = data,family = binomial)

#Compute the predicted probability that risk.alleles is observed (r2=1) 
pred.prob.risk.alleles <- predict(mod.prob.risk.alleles, type="response")  

# Compute Inverse Probability Weights
data$ipw.risk.alleles = 1 / pred.prob.risk.alleles

#Compute Weighted Counts for Each Category
library(dplyr)
weighted_counts <- data %>%
  filter(!is.na(risk.alleles)) %>% # remove NA values
  group_by(risk.alleles) %>% # in order to summarize within each category
  summarize(weighted_sum = sum(ipw.risk.alleles, na.rm = TRUE), .groups = "drop")

# Compute proportions
weighted_counts <- weighted_counts %>%
  mutate(proportion = weighted_sum / sum(weighted_sum))

print(weighted_counts)

set.seed(123)  # Ensure reproducibility
nboots <- 1000

# Initialize storage for bootstrap results
boot_results <- matrix(NA, nrow = nboots, ncol = length(unique(na.omit(data$risk.alleles))))

for (i in 1:nboots) {
  # Bootstrap resampling with replacement
  d.boot <- data %>%
    filter(!is.na(risk.alleles)) %>%
    slice_sample(prop = 1, replace = TRUE)  # Stratified resampling to ensure no category is under-/over-sampled 
  
  # Compute weighted counts within bootstrap sample
  weighted_counts <- d.boot %>%
    group_by(risk.alleles) %>%
    summarize(weighted_sum = sum(ipw.risk.alleles, na.rm = TRUE), .groups = "drop") %>%
    mutate(proportion = weighted_sum / sum(weighted_sum))
  
  # Store proportions in matrix for later confidence interval calculation
  boot_results[i, ] <- weighted_counts$proportion
}

head(boot_results)

# Compute 95% bootstrap confidence intervals
ci_lower <- apply(boot_results, 2, quantile, probs = 0.025, na.rm = TRUE)
ci_upper <- apply(boot_results, 2, quantile, probs = 0.975, na.rm = TRUE)

# Format results into a data frame
boot_ci <- data.frame(
  risk_alleles = c(0, 1, 2),
  lower_ci = ci_lower,
  upper_ci = ci_upper
)


# Print results
print(boot_ci)

