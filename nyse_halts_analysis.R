# Project:     NYSE Trading Halts Analysis - Count Data Modeling
# Author:      Stefano Colazzilli
# Description: Comparative analysis of Poisson vs. Negative Binomial regression
#              to model market microstructure shocks.
# Note:        Uses synthetic data to preserve proprietary dataset confidentiality.



# 1. Setup 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(MASS, ggplot2, gridExtra)

set.seed(2025) # Ensure reproducibility


# 2. Data Simulation 
generate_market_data <- function(n = 1000) {
  # Volatility proxy (VIX) and Volume (standardized)
  volatility <- rnorm(n, mean = 20, sd = 5)
  volume     <- rnorm(n, mean = 100, sd = 15)
  
  # Latent component (Log-link)
  log_mu <- -2.5 + 0.08 * volatility + 0.01 * volume
  mu <- exp(log_mu)
  
  # Generate Response (Y) using Negative Binomial (Theta = 1.5 -> Overdispersion)
  halts <- rnegbin(n, mu = mu, theta = 1.5)
  
  return(data.frame(TradingHalts = halts, Volatility = volatility, Volume = volume))
}


# 3. Model Estimation 
run_analysis <- function() {
  cat("\n[1] Generating synthetic data...\n")
  df_market <- generate_market_data(n = 1000)
  
  # Dispersion Check
  mean_y <- mean(df_market$TradingHalts)
  var_y  <- var(df_market$TradingHalts)
  ratio  <- var_y / mean_y
  cat(sprintf("    > Dispersion Check: Mean=%.2f | Var=%.2f | Ratio=%.2f\n", mean_y, var_y, ratio))
  
  cat("\n[2] Fitting GLM models...\n")
  # Model 1: Poisson (Baseline)
  model_pois <- glm(TradingHalts ~ Volatility + Volume, 
                    family = poisson(link = "log"), 
                    data = df_market)
  
  # Model 2: Negative Binomial (Candidate)
  model_nb <- glm.nb(TradingHalts ~ Volatility + Volume, 
                     data = df_market)
  
  # Model Selection (AIC)
  aic_pois <- AIC(model_pois)
  aic_nb   <- AIC(model_nb)
  
  cat("\n[3] Model Comparison (AIC):\n")
  cat(sprintf("    > Poisson: %.2f\n    > NegBin:  %.2f\n", aic_pois, aic_nb))
  
  if(aic_nb < aic_pois) {
    cat(sprintf("    > RESULT: Negative Binomial preferred (Delta AIC: %.2f)\n", aic_pois - aic_nb))
    best_model <- model_nb
  } else {
    cat("    > RESULT: Poisson preferred\n")
    best_model <- model_pois
  }
  
  return(list(model = best_model, data = df_market))
}


# 4. Visualization
visualize_results <- function(model, data) {
  cat("\n[4] Generating Plots\n")
  
  # PLOT 1: Marginal Effect (Ceteris Paribus)
  sim_x <- seq(min(data$Volatility), max(data$Volatility), length.out = 200)
  mean_vol <- mean(data$Volume)
  
  pred_grid <- data.frame(Volatility = sim_x, Volume = mean_vol)
  preds <- predict(model, newdata = pred_grid, type = "link", se.fit = TRUE)
  
  # Inverse Link Transform
  pred_grid$Predicted <- exp(preds$fit)
  pred_grid$LowerCI   <- exp(preds$fit - 1.96 * preds$se.fit)
  pred_grid$UpperCI   <- exp(preds$fit + 1.96 * preds$se.fit)
  
  p1 <- ggplot() +
    geom_point(data = data, aes(x = Volatility, y = TradingHalts), 
               alpha = 0.4, color = "grey", size = 1.5) + 
    geom_ribbon(data = pred_grid, aes(x = Volatility, ymin = LowerCI, ymax = UpperCI),
                fill = "orange", alpha = 0.2) + 
    geom_line(data = pred_grid, aes(x = Volatility, y = Predicted), 
              color = "red", linewidth = 1.2) +
    labs(title = "A. Marginal Effect: Volatility vs Halts",
         subtitle = "Volume fixed at Mean (95% CI)",
         y = "Predicted Halts", x = "Volatility Index") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 11))
  
  # PLOT 2: Calibration
  data$FullPred <- predict(model, type = "response")
  
  p2 <- ggplot(data, aes(x = FullPred, y = TradingHalts)) +
    geom_point(alpha = 0.4, color = "skyblue") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = "B. Model Calibration",
         subtitle = "Observed vs Predicted Counts",
         x = "Predicted", y = "Observed") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 11))
  
  grid.arrange(p1, p2, ncol = 2)
}


# 5. Stress Testing 
perform_stress_test <- function(model) {
  stress_scen <- data.frame(Volatility = 35, Volume = 150)
  exp_halts <- predict(model, newdata = stress_scen, type = "response")
  
  cat("\n[5] Stress Test (Vol=35, Vol=150):\n")
  cat(sprintf("    > Expected Halts: %.2f\n", exp_halts))
}



# MAIN 
results <- run_analysis()
visualize_results(results$model, results$data)
perform_stress_test(results$model)
cat("\nEnd of Analysis\n")