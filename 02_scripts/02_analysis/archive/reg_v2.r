run_regression <- function(data, index_cols, aid_col, frag_cols, controls, fixed_effects = TRUE) {
  models <- list()
  model_type <- ifelse(fixed_effects, "within", "pooled")
  effect_param <- ifelse(fixed_effects, "twoways", NULL)
  
  # Print parameters
  cat("Model parameters:\n")
  cat("- Model type:", model_type, "\n")
  cat("- Effect parameter:", ifelse(is.null(effect_param), "NULL", effect_param), "\n")
  cat("- Index columns:", paste(index_cols, collapse = ", "), "\n")
  cat("- Aid column:", aid_col, "\n")
  cat("- Fragment columns:", paste(frag_cols, collapse = ", "), "\n")
  cat("- Control variables:", ifelse(length(controls) > 0, paste(controls, collapse = ", "), "None"), "\n\n")
  
  # Ensure data is sorted by time before applying lag function
  data <- data[order(data[[index_cols[2]]]), ]
  
  for (frag_col in frag_cols) {
    if (length(controls) > 0) {
      control_str <- paste(controls, collapse = " + ")
      formula <- as.formula(paste("log(lag(mean_nl,1)+.01) ~ log(", aid_col, ") +", frag_col,"*", control_str))
    } else {
      formula <- as.formula(paste("lag(mean_nl,1) ~ log(", aid_col, ") +", frag_col))
    }
    
    # Print formula
    cat("Formula for", frag_col, ":\n")
    cat(deparse(formula), "\n\n")
    
    models[[frag_col]] <- plm(
      formula,
      data = data,
      index = index_cols,
      effect = effect_param,
      model = model_type
    )
  }
  
  return(models)
}


fe_models_admin1 <- run_regression(panel_aid_admin1, c("GID_1", "paymentyear"), "total_aid_admin1", frag_vars_admin1, controls_fix_admin1, TRUE)
fe_models_admin2 <- run_regression(panel_aid_admin2, c("GID_2", "paymentyear"), "total_aid_admin2", frag_vars_admin2, controls_fix_admin2, TRUE)

# Fixed Effects Models
stargazer(fe_models_admin1, type = "text", out = "fe_models_admin1.tex")
stargazer(fe_models_admin2, type = "text", out = "fe_models_admin2.tex")
