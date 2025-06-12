# predict_link.R
# Matt Kmiecik
# 2025-06-11
# v1
# Purpose: always predicts on the link 

predict_link <- function(mod, newdat){
  
  # generates predictions in link
  pred <- 
    predict(
      mod, 
      type = "link",
      newdata = newdat, 
      se.fit = TRUE
    )
  
  # getting critical value for 95% CI
  alpha <- 0.05  # for 95% CI
  df_resid <- mod$df.residual  # residual degrees of freedom
  crit_val <- qt(1 - alpha/2, df = df_resid)
  
  inv_link <- mod$family$linkinv # inverse link function
  lwr <- pred$fit - crit_val * pred$se.fit # lower 95% CI
  upr <- pred$fit + crit_val * pred$se.fit # upper 95% CI
  
  # back-transforms
  pred_resp <- inv_link(pred$fit)
  lwr_resp <- inv_link(lwr)
  upr_resp <- inv_link(upr)
  
  # combines link-inversed variables to newdata 
  newdat <- cbind(newdat, pred_resp, lwr_resp, upr_resp) 
  return(newdat) # returns to user
  
}