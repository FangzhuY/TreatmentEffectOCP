rm(list=ls())

library(nnet)
library(readxl)
library(foreign)
library(haven)
library(tidyverse)
library(dplyr)
library(pracma)
library(patchwork)
library(ggplot2)

#------------------------------------------------------------------------------#
# Load the processed data
#------------------------------------------------------------------------------#

load("Working_data/data_final.RData")

data <- data %>% filter(age>=20 & age<=42)

data <- data %>%
  mutate(job_emp = case_when(
    jobclass == "employed" ~ paste0("employed_", employer),
    jobclass == "self-employed" ~ "self-employed",
    jobclass == "unemployed" ~ "unemployed",
    jobclass == "agriculture" ~ "agriculture",
    TRUE ~ "other"  # catch any unexpected values (optional but safe)
  ))

#------------------------------------------------------------------------------#
# Urban, Han, education, employed, employer type, age, housing price, province
# Parametric methods, faster method
#------------------------------------------------------------------------------#

library(nnet)
library(dplyr)
library(parallel)
library(tictoc)

# --- STEP 1: Precompute the models outside of loop (SLOW PART, only once!) ---
ideal14 <- data %>% filter(year == 2014)
data18  <- data %>% filter(year == 2018)

result1 <- multinom(factor(ideal) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                    data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)

result2 <- multinom(factor(ideal) ~ factor(N) + age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                    data = data18, Hess = TRUE, maxit = 1000, trace = FALSE)

result3 <- multinom(factor(N) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                    data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)

# --- STEP 2: Define the function (SPEED OPTIMIZED) ---

cond_treat_effect <- function(age_i, house_i, urban_i, han_i, educ_i, job_emp_i, prov_i, result1, result2, result3) {
  
  data2 <- data.frame(age = age_i, hprice = house_i, urban = urban_i, han = han_i, educ1 = educ_i, job_emp = job_emp_i, prov = prov_i)
  prob14 <- predict(result1, data2, type = 'probs')
  prob14 <- as.numeric(c(1 - prob14, prob14))
  
  data3 <- data.frame(N = 1, age = age_i, hprice = house_i, urban = urban_i, han = han_i, educ1 = educ_i, job_emp = job_emp_i, prov = prov_i)
  conditional18_1 <- predict(result2, data3, type = 'probs')
  condProb1 <- as.numeric(c(1 - conditional18_1, conditional18_1))
  
  data4 <- data.frame(N = 2, age = age_i, hprice = house_i, urban = urban_i, han = han_i, educ1 = educ_i, job_emp = job_emp_i, prov = prov_i)
  conditional18_2 <- predict(result2, data4, type = 'probs')
  condProb2 <- as.numeric(c(1 - conditional18_2, conditional18_2))
  
  condProb <- t(rbind(condProb1, condProb2))
  child_counter <- solve(condProb) %*% prob14
  
  child14 <- predict(result3, data2, type = 'probs')
  child14 <- as.numeric(c(1 - child14, child14))
  
  child <- c(1, 2)
  policy <- as.numeric(child %*% child14 - child %*% child_counter)
  
  return(policy)
}

# --- STEP 3: Prepare the data (extract variables for parallel use) ---

data14 <- data %>% filter(year == 2014)
args_list <- split(data14, seq(nrow(data14)))

# --- STEP 4: Run in parallel (USE ALL CORES - 1) ---

num_cores <- detectCores() - 1  # leave 1 core free for OS
tic("Parallel cond_effect calculation")
cond_effect_new <- unlist(mclapply(args_list, function(row) {
  cond_treat_effect(row$age, row$hprice, row$urban, row$han, row$educ1, row$job_emp, row$prov, 
                    result1, result2, result3)
}, mc.cores = num_cores))
toc()

# group all the results into a vector
data14$effect <- cond_effect_new

# Grouped means
m_all                  <- mean(data14$effect, na.rm = TRUE)
m_urban1               <- mean(data14$effect[data14$urban == 1], na.rm = TRUE)
m_urban0               <- mean(data14$effect[data14$urban == 0], na.rm = TRUE)
m_han1                 <- mean(data14$effect[data14$han == 1], na.rm = TRUE)
m_han0                 <- mean(data14$effect[data14$han == 0], na.rm = TRUE)
m_educ_lths            <- mean(data14$effect[data14$educ1 == "Less than High School"], na.rm = TRUE)
m_educ_hs              <- mean(data14$effect[data14$educ1 == "High School Graduate"], na.rm = TRUE)
m_educ_college         <- mean(data14$effect[data14$educ1 == "College and Higher"], na.rm = TRUE)
m_job_unemployed       <- mean(data14$effect[data14$job_emp == "unemployed"], na.rm = TRUE)
m_job_self             <- mean(data14$effect[data14$job_emp == "self-employed"], na.rm = TRUE)
m_job_agri             <- mean(data14$effect[data14$job_emp == "agriculture"], na.rm = TRUE)
m_job_employed_private <- mean(data14$effect[data14$job_emp == "employed_private"], na.rm = TRUE)
m_job_employed_gov     <- mean(data14$effect[data14$job_emp == "employed_government"], na.rm = TRUE)

m_prov <- tapply(data14$effect, data14$prov, mean, na.rm = TRUE)

point_estimates <- c(m_all, m_urban1, m_urban0, m_han1, m_han0,
                     m_educ_lths, m_educ_hs, m_educ_college,
                     m_job_unemployed, m_job_self, m_job_agri,
                     m_job_employed_private, m_job_employed_gov, 
                     m_prov)

names(point_estimates) <- c("All", "Urban=1", "Urban=0", "Han=1", "Han=0",
                            "Less than HS", "HS Grad", "College+",
                            "Unemployed", "Self-employed", "Agriculture",
                            "Employed_Private", "Employed_Gov",
                            paste0("Prov_", sort(unique(data14$prov))))

# calculate determinant for M
cal_det <- function(age_i, house_i, urban_i, han_i, educ_i, job_emp_i, prov_i, result1, result2, result3) {

  data3 <- data.frame(N = 1, age = age_i, hprice = house_i, urban = urban_i, han = han_i, educ1 = educ_i, job_emp = job_emp_i, prov = prov_i)
  conditional18_1 <- predict(result2, data3, type = 'probs')
  condProb1 <- as.numeric(c(1 - conditional18_1, conditional18_1))
  
  data4 <- data.frame(N = 2, age = age_i, hprice = house_i, urban = urban_i, han = han_i, educ1 = educ_i, job_emp = job_emp_i, prov = prov_i)
  conditional18_2 <- predict(result2, data4, type = 'probs')
  condProb2 <- as.numeric(c(1 - conditional18_2, conditional18_2))
  
  condProb <- t(rbind(condProb1, condProb2))
  det_result <- det(condProb)

  return(det_result)
}

#------------------------------------------------------------------------------#
# calculate standard errors
#------------------------------------------------------------------------------#

library(dplyr)
library(nnet)
library(pbmcapply)

# ----------- Bootstrap Starts -----------

set.seed(19940506)

B <- 500
num_cores <- parallel::detectCores() - 1
pid_list <- unique(data$pid)


#------------------------------------------------------------------------------#
# Calculate the standard errors of the determinants of M
#------------------------------------------------------------------------------#

data_unique <- data[!duplicated(data[, c("prov", "age", "urban", "han", "educ1", "job_emp","hprice")]), ]

bootstrap_det <- function(age_i, house_i, urban_i, han_i, educ_i, job_emp_i, prov_i){
  det_vec <- rep(NA,B)
  for (i in 1:B){
    
    print(i)
    sampled_pids <- sample(pid_list, size = length(pid_list), replace = TRUE)
    boot_data <- data %>% filter(pid %in% sampled_pids)
    
    # Fit multinom models
    ideal14 <- boot_data %>% filter(year == 2014)
    data18  <- boot_data %>% filter(year == 2018)
    
    result1 <- multinom(factor(ideal) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    result2 <- multinom(factor(ideal) ~ factor(N) + age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = data18, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    result3 <- multinom(factor(N) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    det_vec[i] <- cal_det(age_i, house_i, urban_i, han_i, educ_i, job_emp_i, prov_i, result1, result2, result3)
  }

  return(det_vec)
}

  det_vec1 <- bootstrap_det(data_unique[1,"age"], data_unique[1,"hprice"], data_unique[1,"urban"], data_unique[1,"han"],
                           data_unique[1,"educ1"], data_unique[1,"job_emp"], data_unique[1,"prov"])
  
  # Extract values for title
  age_val <- data_unique[1, "age"]
  educ1_val <- data_unique[1, "educ1"]
  prov_val <- data_unique[1, "prov"]
  urban_val <- data_unique[1, "urban"]
  han_val <- data_unique[1, "han"]
  job_emp_val <- data_unique[1, "job_emp"]
  hprice_val <- data_unique[1, "hprice"]
  
  # Construct title string
  title_str <- paste0(
    # "Bootstrap Distribution of Determinant of M\n",
    "age = ", age_val, ", educ1 = ", educ1_val, 
    ", prov = ", prov_val, ", urban = ", urban_val, 
    ", han = ", han_val, ", job_emp = ", job_emp_val,
    ", hprice = ", round(hprice_val, 2)
  )
  
  # Convert to data frame for ggplot
  df_det <- data.frame(determinant = det_vec1)
  
  ggplot(df_det, aes(x = determinant)) +
    geom_histogram(fill = NA, color = "black", bins = 30) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
    labs(
      title = title_str,
      x = "Determinant",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  #######
  # Preallocate a list to store results
  det_all <- list()
  
  # Loop over first 9 rows of data_unique
  for (i in 1:9) {
    row <- data_unique[i, ]
    
    det_vec <- bootstrap_det(row$age, row$hprice, row$urban, row$han,
                             row$educ1, row$job_emp, row$prov)
    
    # Create a label for this row
    label_i <- paste0("age=", row$age, ", educ1=", row$educ1, ", prov=", row$prov,
                      "house price=", row$hprice, ", urban=row$urban",
                      "ethnicity=", row$han, "job status=", row$job_emp,
                      "province=", row$prov)
    
    # Store with label
    det_all[[i]] <- data.frame(determinant = det_vec, case = label_i)
  }
  
  # Combine into one data frame
  df_all <- bind_rows(det_all)
  
  p <- ggplot(df_all, aes(x = determinant)) +
    geom_histogram(fill = NA, color = "black", bins = 30) +
    geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 0.8) +
    facet_wrap(~ case, scales = "free", ncol = 3) +
    labs(
      # title = "Bootstrap Distributions of Determinant of M",
      x = "Determinant",
      y = "Frequency"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text = element_blank()
    )
  
  ggsave("Output/determinant_hist.pdf", plot = p,
         device = "pdf", width = 10, height = 8, units = "in", dpi = 300)
  
#------------------------------------------------------------------------------#
# Calculate the standard errors for the estimates
#------------------------------------------------------------------------------#
bootstrap_results <- pbmclapply(1:B, function(b) {
  
  tryCatch({
    
    # Sample pids
    sampled_pids <- sample(pid_list, size = length(pid_list), replace = TRUE)
    boot_data <- data %>% filter(pid %in% sampled_pids)
    
    # Fit multinom models
    ideal14 <- boot_data %>% filter(year == 2014)
    data18  <- boot_data %>% filter(year == 2018)
    
    result1 <- multinom(factor(ideal) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    result2 <- multinom(factor(ideal) ~ factor(N) + age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = data18, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    result3 <- multinom(factor(N) ~ age + hprice + urban + han + factor(educ1) + factor(job_emp) + factor(prov), 
                        data = ideal14, Hess = TRUE, maxit = 1000, trace = FALSE)
    
    # Calculate cond_treat_effect for year 2014
    data14 <- boot_data %>% filter(year == 2014)
    data14$effect <- NA
    
    for (i in 1:nrow(data14)) {
      data14$effect[i] <- cond_treat_effect(
        age_i = data14$age[i],
        house_i = data14$hprice[i],
        urban_i = data14$urban[i],
        han_i = data14$han[i],
        educ_i = data14$educ1[i],
        job_emp_i = data14$job_emp[i],
        prov_i = data14$prov[i],
        result1, result2, result3
      )
    }
    
    # Calculate subgroup means
    m_all <- mean(data14$effect, na.rm = TRUE)
    m_urban1 <- mean(data14$effect[data14$urban == 1], na.rm = TRUE)
    m_urban0 <- mean(data14$effect[data14$urban == 0], na.rm = TRUE)
    m_han1 <- mean(data14$effect[data14$han == 1], na.rm = TRUE)
    m_han0 <- mean(data14$effect[data14$han == 0], na.rm = TRUE)
    
    m_educ_lths <- mean(data14$effect[data14$educ1 == "Less than High School"], na.rm = TRUE)
    m_educ_hs <- mean(data14$effect[data14$educ1 == "High School Graduate"], na.rm = TRUE)
    m_educ_college <- mean(data14$effect[data14$educ1 == "College and Higher"], na.rm = TRUE)
    
    m_job_unemployed <- mean(data14$effect[data14$job_emp == "unemployed"], na.rm = TRUE)
    m_job_self <- mean(data14$effect[data14$job_emp == "self-employed"], na.rm = TRUE)
    m_job_agri <- mean(data14$effect[data14$job_emp == "agriculture"], na.rm = TRUE)
    m_job_employed_private <- mean(data14$effect[data14$job_emp == "employed_private"], na.rm = TRUE)
    m_job_employed_gov <- mean(data14$effect[data14$job_emp == "employed_government"], na.rm = TRUE)

    m_prov <- tapply(data14$effect, data14$prov, mean, na.rm = TRUE)
    
    # Combine and return
    c(m_all, m_urban1, m_urban0, m_han1, m_han0,
      m_educ_lths, m_educ_hs, m_educ_college,
      m_job_unemployed, m_job_self, m_job_agri, m_job_employed_private, m_job_employed_gov,
      m_prov)
    
  }, error = function(e) {
    
    cat("Error in bootstrap iteration", b, ":", conditionMessage(e), "\n")
    
    # Return NA vector
    rep(NA, 1 + length(unique(data$prov)))
  })
  
}, mc.cores = num_cores)

# ----------- Combine bootstrap results -----------

bootstrap_mat <- do.call(rbind, bootstrap_results)

threshold <- 10

# Keep only rows where all values are within [-10, 10]
clean_bootstrap_mat <- bootstrap_mat[apply(abs(bootstrap_mat) < threshold, 1, all), ]

# Calculate standard errors
bootstrap_se <- apply(clean_bootstrap_mat, 2, sd, na.rm = TRUE)

# Assign names to results
prov_names <- sort(unique(data$prov))

names(bootstrap_se) <- c("All", "Urban=1", "Urban=0", "Han=1", "Han=0",
                         "Less than HS", "HS Grad", "College+",
                         "Unemployed", "Self-employed", "Agriculture",
                         "Employed_Private", "Employed_Gov",
                         paste0("Prov_", prov_names))

# Show SE
print(bootstrap_se)

# group estimates and SEs into a table
summary_table <- data.frame(
  Group = names(point_estimates),
  Estimate = point_estimates,
  SE = bootstrap_se[names(point_estimates)]
)

write.csv(summary_table, "Output/estimates_with_se.csv", row.names = FALSE)

