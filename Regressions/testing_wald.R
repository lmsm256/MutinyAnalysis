#random bs first (from peacescienceR)
coup_data <- create_stateyears() %>% add_democracy()

#get full coup dataset
coup_data$mutiny_fake <- rbinom(nrow(coup_data), size = 1, prob = 0.1)
coup_data$coup_fake <- rbinom(nrow(coup_data), size = 1, prob = 0.03)


#if have nas in either, run this so test works
# Clean your data first so both models use identical rows
#clean_data <- subset(coup_data, !is.na(mutiny) & !is.na(coup))


#"naive model" without mutiny 
reduced_model <- feglm(coup_fake ~ polity2 + euds, 
                     data = coup_data, 
                     family = "binomial",
                     cluster = ~ccode)
reduced_model

#actual model with mutiny
full_model <- feglm(coup_fake ~ polity2 + euds + mutiny_fake, #replace with actual variables
                  data = coup_data, 
                  family = "binomial",
                  cluster = ~ ccode)
full_model

# Wald test to test if a variable adds to predictive power (from fixest package)
robust_test <- wald(full_model, keep = "mutiny_fake") #replace with munity, does test for us
robust_test #if p < 0.05, significantly adds to model

#AIC and BIC for models (want full model to have lower both of these for mutiny to have effect)
#Extract AIC and BIC for both models

#If AIC lower, generally improves model
aic_reduced <- AIC(reduced_model)
aic_full    <- AIC(full_model)

if ((aic_full - aic_reduced) < 0){
  print("Mutiny generally improves our model's predictive power")
} else {
  print("Mutiny DOES NOT improve the model under AIC")
}

#BIC much more strict on added variables, so even better if this is lower
bic_reduced <- BIC(reduced_model)
bic_full    <- BIC(full_model)

if ((bic_full - bic_reduced) < 0) {
  print("Mutiny heavily improves our model's predictive power")
} else {
  print("Mutiny DOES NOT improve the model under BIC")
}
