#random bs first (from peacescienceR)
coup_data <- create_stateyears() %>% add_democracy()

#get full coup dataset
coup_data$mutiny_fake <- rbinom(nrow(coup_data), size = 1, prob = 0.1)
coup_data$coup_fake <- rbinom(nrow(coup_data), size = 1, prob = 0.03)


#if have nas in either, run this so test works
# Clean your data first so both models use identical rows
# cleaned_data <- coup_data[complete.cases(coup_data[, c("coup_fake", "polity2", "euds", "mutiny_fake")]), ] #again replace with actual variables


#"naive model" without mutiny 
reduced_model <- feglm(coup_fake ~ polity2 + euds, #replace with actual variables
                     data = coup_data, #change to cleaned data if NAs not dealt with
                     family = "binomial",
                     cluster = ~ccode)
reduced_model

#actual model with mutiny
full_model <- feglm(coup_fake ~ polity2 + euds + mutiny_fake, #replace with actual variables
                  data = coup_data, #change to cleaned data if NAs not dealt with
                  family = "binomial",
                  cluster = ~ ccode)
full_model

# Wald test to test if a variable is significant when controlling for others
robust_test <- wald(full_model, keep = "mutiny_fake") #replace with munity, does test for us
robust_test #if p < 0.05, significantly adds to model

#AIC and BIC for models (want full model to have lower both of these for mutiny to have effect)
#Extract AIC and BIC for both models

#If AIC lower, generally better fitted model (from fitstat(since we use feglm))
aic_reduced <- fitstat(reduced_model, "aic")$aic
aic_full    <- fitstat(full_model, "aic")$aic

if ((aic_full - aic_reduced) < 0){
  print("Model with mutiny is preferred under AIC.")
} else {
  print("Model with mutiny is not preferred under AIC.")
}

#BIC much more strict on added variables, so even better if this is lower
bic_reduced <- fitstat(reduced_model, "bic")$bic
bic_full    <- fitstat(full_model, "bic")$bic

if ((bic_full - bic_reduced) < 0) {
  print("Model with mutiny is preferred under BIC.")
} else {
  print("Model with mutiny is not preferred under BIC.")
}
