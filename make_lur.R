make_lur = function(dat1,
               response,
               exclude = {
               },
               special = c('Latitude', 'Longitude'),
               dep_col) {
  # exclude var. you think not related to your y
  # special includes varibles you cannot assign a prior directional relationship to y
  # dep_col:  column index of the start of your dependent var.
  params = names(dat1)
  params1 = params # just to get the listed orders of these vars
  
  params = params[-c(dep_col:ncol(dat1))]
  
  params = setdiff(params, exclude)
  
  
  
  
  
  # Remove parameters with fewer than 10% row length.
  # Must start from the last parameter to avoid skipping items.
  for (i in length(params):1) {
    if (length(unique(dat1[[params[i]]])) < 0.1 * nrow(dat1)) {
      params = params[-i]
    }
  }
  
  
  nparam = length(params)
  stopifnot(nparam > 0)
  
  # Start the regression process.
  model = paste(response, " ~ ", sep = "")
  # As regression proceeds, parameters are moved from one set to the other.
  ID = {
  } # for returning listed order
  unused = params
  used = {
  }
  # adj R^2 of the currently-selescted model.
  adjr2 = 0
  # The history of adj R^2 improvements for debugging.
  adjr2diff = {
  }
  # Accepted number of predictors in the model
  ap = 1
  
  converged = FALSE
  while (!converged) {
    # Make a list of enhanced models and their respective (adjusted) R^2.
    nunused = length(unused)
    adjr2s = rep(0, times = nunused)
    coeff = rep(0, times = nunused)
    
    models = paste(model, " + ", params[match(unused, params)], sep = "")
    for (i in 1:nunused) {
      t = lm(formula(models[i]), dat1)
      adjr2s[i] = summary(t)$adj.r.squared
      coeff[i] <- summary(t)$coefficients[ap + 1, 1]
    }
    
    # In the first iteration, find the model with the largest adj R^2 and the correct coefficients.
    nused = length(used)
    
    coefficients = FALSE
    jump = FALSE
    while (!coefficients & nused == 0) {
      adjr2s = adjr2s[!is.na(adjr2s)]
      if (length(adjr2s) == 0) {
        converged = TRUE
        break
      }
      maxv = max(adjr2s)
      if (maxv < 0) {
        converged = TRUE
        break
      }
      
      # When multiple parameters have the same adj R^2, use the first one.
      maxi = which(adjr2s == maxv)[1]
      stopifnot(maxi > 0)
      
      if (unused[maxi] %in% special) {
        coefficients = TRUE
        jump = TRUE
        model = models[maxi]
        adjr2diff = c(adjr2diff, maxv - adjr2)
        adjr2 = maxv
        ID <- c(ID, which(params1 == unused[maxi]))
        used = c(used, unused[maxi])
        unused = unused[-maxi]
        ap = ap + 1
        
        
      } else if (coeff[maxi] < 0) {
        used = c(used, unused[maxi])
        unused = unused[-maxi]
        adjr2s = adjr2s[-maxi]
        models <- models[-maxi]
        coeff <- coeff[-maxi]
      } else if (coeff[maxi] > 0) {
        coefficients = TRUE
        jump = TRUE
        # Include the chosen parameter
        model = models[maxi]
        
        adjr2diff = c(adjr2diff, maxv - adjr2)
        adjr2 = maxv
        ID <- c(ID, which(params1 == unused[maxi]))
        used = c(used, unused[maxi])
        unused = unused[-maxi]
        ap = ap + 1
        
      }
    }
    
    if (jump == TRUE) {
      next
    }
    
    if (maxv < 0) {
      break
    }
    
    
    coefficients = FALSE
    ncoeff <-
      length(which(summary(lm(
        formula(model), dat1
      ))$coefficients[-1, 1] > 0))
    while (!coefficients) {
      adjr2s = adjr2s[!is.na(adjr2s)]
      if (length(adjr2s) == 0) {
        converged = TRUE
        break
      }
      maxv = max(adjr2s)
      if (maxv < 0) {
        converged = TRUE
        break
      }
      # In the unlikely event that multiple parameters have the same maximum
      # (adjusted) R^2 value, always use the first parameter.
      maxi = which(adjr2s == maxv)[1]
      
      stopifnot(maxi > 0)
      if (unused[maxi] %in% special) {
        model1 = models[maxi]
        ncoefft <-
          length(which(summary(lm(
            formula(model1), dat1
          ))$coefficients[-1, 1] > 0))
        if ((ncoefft - ncoeff) %in% c(0, 1)) {
          coefficients = TRUE
        } else {
          used = c(used, unused[maxi])
          unused = unused[-maxi]
          adjr2s = adjr2s[-maxi]
          models <- models[-maxi]
          coeff <- coeff[-maxi]
        }
        
        
      } else if (coeff[maxi] < 0) {
        used = c(used, unused[maxi])
        unused = unused[-maxi]
        adjr2s = adjr2s[-maxi]
        models <- models[-maxi]
        coeff <- coeff[-maxi]
        
      } else if (coeff[maxi] > 0) {
        model1 = models[maxi]
        ncoefft <-
          length(which(summary(lm(
            formula(model1), dat1
          ))$coefficients[-1, 1] > 0))
        if (ncoefft - ncoeff == 1) {
          coefficients = TRUE
        } else {
          used = c(used, unused[maxi])
          unused = unused[-maxi]
          adjr2s = adjr2s[-maxi]
          models <- models[-maxi]
          coeff <- coeff[-maxi]
        }
        
      }
    }
    
    
    if (maxv < 0) {
      break
    }
    
    # Include the term, if its adjusted R^2 is more than the former adj r2 by
    # at least 1%.
    if (maxv - adjr2 < 0.01) {
      # No more beneficial basic term is found.
      converged = TRUE
    } else {
      model = models[maxi]
      adjr2diff = c(adjr2diff, adjr2s[maxi] - adjr2)
      adjr2 = adjr2s[maxi]
      ID <- c(ID, which(params1 == unused[maxi]))
      used = c(used, unused[maxi])
      unused = unused[-maxi]
      ap = ap + 1
    }
    
  }
  # return(model)
  
  return(list(formula = model, summary = summary(lm(
    formula(model), dat1
  )), excel_column_ID = ID))
  
  
}
