#' @title Average treatment effect of the treated estimation
#' @description The function calculates the influence of a cluster of variables on a target variable using an influence matrix on the server side.lows for the option to assign the result to a new object or a specific data source.
#' @param data A string name of the dataframe containing the data on the server side.
#' @param yname Name of the outcome variable in the data frame on the server side.
#' @param tname Name of the time variable in the data frame on the server side.
#' @param idname Name of the id variable in the data frame on the server side.
#' @param gname Name of the treatment indicator variable in the data frame on the server side.
#' @param t_periods Vector of time periods.
#' @param g_periods Vector of treatment periods.
#' @param xformla Formula of covariates.
#' @param control_group either "nevertreated" or "notyettreated"
#' @param base_period either "varying" or "universal".
#' @param anticipation integer of pre-treatment anticipation.
#' @param alpha Confidence level of CIs.
#' @param bstrap If TRUE, multiplier bootstrap is used to compute standard errors.
#' @param biters Number of bootstrap draws. Only relevenat if bstrap is TRUE.
#' @param cband if TRUE, simultaneous confidence bands are returned.
#' @param clustervars A string name of the vector of variables that make up the cluster for standard errors. Only has an influence if bstrap is true.
#' @param est_method can either be "dr", "reg" or "ipw".
#' @param maxit MAximal number of iterations for ds.glm.
#' @param datasources A specific Datashield data source to which the result should be assigned.
#' @param clear_console If TRUE, the console is cleared at certain stages.
#' @export

ds.did <- function(data = NULL, yname = NULL, tname = NULL, idname = NULL, gname = NULL,
                   t_periods = NULL, g_periods = NULL,
                   xformla = NULL,
                   control_group = "notyettreated",
                   base_period = "varying",
                   anticipation = 0, alpha = 0.05,
                   bstrap = FALSE, biters = 1000, cband = TRUE, clustervars = NULL,
                   est_method = "dr", maxit = 10000,
                   datasources = NULL, clear_console = FALSE) {


  #-------------------------------Checks of inputs------------------------------
  #------------------------------Check that inputs are given (NULL check)-------

  #- Check for data sources
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if (!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {
    methods::is(d, "DSConnection")
  }))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  # check that ever input is given to the function
  necessary_inputs <- c("data", "yname", "tname", "idname", "gname", "t_periods", "g_periods")
  for (i in necessary_inputs) {
    if (is.null(i)) {
      stop(paste0("Please provide the name of the input object ", i), call. = FALSE)
    }
  }

  # check that data is defined on the servers
  # isDefined(datasources, data)

  # TODO
  # check that all relevant names are within each servers columns

  #---------------Sort data-----------------------------------------------------
  # sort data frame by first id and second time

  vec_sort <- paste0("c('", data, "$", tname, "','", data, "$", idname, "')")

  dsBaseClient::ds.dataFrameSort(
    df.name = data, sort.key.name = eval(parse(text=vec_sort)), newobj = "sorted_data_missing",
    datasources = datasources
  )

  #---------------process information about covariates--------------------------
  if (is.null(xformla)) {
    xformla_paste <- ""
  } else {
    xformla_paste <- xformla
  }

  # y_delta is the name we give to the difference of Y observations in t and g - anticipation - 1
  # The name is assigned within the loop below at each period respectively
  formula_linear_model <- stats::as.formula(paste("delta_y", "~ constant +", xformla_paste, "-1"))

  # G_t is the name of the dummy variable that indicates if an individuals receives
  # treatment in period g. The name is assigned within the loop below at each
  # period respectively
  formula_logistic_model <- stats::as.formula(paste("G_dummy", "~ constant +", xformla_paste, "-1"))


  #---------------remove missing values-----------------------------------------
  #ds.completeCases(
  #  x1 = "sorted_data_missing", newobj = "sorted_data",
  #  datasources = datasources
  #)
  covariates_without_constant <- labels(stats::terms(formula_linear_model))[labels(stats::terms(formula_linear_model))!="constant"]
  for (i in 1:length(datasources)){
    dsBaseClient::ds.dataFrameSubset(
      df.name = "sorted_data_missing",
      V1.name =  paste0("sorted_data_missing$", gname),
      V2.name =  paste0("sorted_data_missing$", gname),
      Boolean.operator = "==",
      newobj = "sorted_data",
      keep.NAs = FALSE,
      keep.cols = match(stats::na.omit(c(yname, idname, tname, gname, covariates_without_constant)) , dsBaseClient::ds.colnames("sorted_data_missing", datasources[i])[[1]]) ,
      datasources = datasources[i]
    )
  }




  #------------remove first period as treatment period if varying base period---
  if (base_period == "varying") {
    t_periods <- t_periods[t_periods != 1]
  }


  #---------------Start did algorithm-------------------------------------------
  index_iteration <- 0
  inf <-c()
  out <- c() # creates vector that is used for output all ATTs, SEs and CIs
  for (g in g_periods) {
    for (t in t_periods) {
      if (clear_console) {
        cat("\014")
      }

      index_iteration <- index_iteration + 1

      # varying base periods or universal ones; for explanation see original paper https://github.com/bcallaway11/did/blob/master/R/att_gt.R
      if (base_period == "varying") {
        if (g <= t) {
          control_period <- g - anticipation - 1
        } else {
          control_period <- t - 1
        }
      } else if (base_period == "universal") {
        control_period <- g - anticipation - 1
      } else {
        stop("base_period must either be universal or varying")
      }

      g_string <- paste(g)
      t_string <- paste(t)

      if ((control_group == "nevertreated")) {
        ds.subsetDf("sorted_data", gname, g,
          include_zero = TRUE, newobj = "df_g",
          datasources = datasources
        ) # dummy function because df subsetting does not work with DSLite; MUST be replaced
      } else if (control_group == "notyettreated") {
        ds.generateNotYetTreated("sorted_data", gname, t, g,
          newobj = "df_g",
          datasources = datasources
        ) # not yet treatd values for g are already set to zero
      }

      #-----------------
      #TODO the check must be above the subsetting
      # get datasources that have enough information - if number is too low, code will fail
      valid_treated <- ds.enoughIndividuals("df_g",
                                            colname = tname,
                                            value = g_string,
                                            datasources = datasources
      )

      valid_untreated <- ds.enoughIndividuals("df_g",
                                              colname = tname,
                                              value = control_period,
                                              datasources = datasources
      )

      valid_datasources_g_t <- ((as.vector(do.call(rbind, valid_treated)) + as.vector(do.call(rbind, valid_untreated))) > 0) * (1:length(valid_treated))

      # skip if no valid datasources
      if (sum(valid_datasources_g_t) == 0) {
        warning(paste0("Not enough observations in any of the datasources for treatment period g=", g, " and period t=", t, ". The analysis was only conducted for valid periods."))
        next
      }

      datasources_subsetted <- datasources[valid_datasources_g_t[valid_datasources_g_t != 0]]
      #----------------


      ds.subsetDf("df_g", tname, t, # create observations only from the current period t
        include_zero = FALSE, newobj = "df_g_t_current",
        datasources = datasources_subsetted
      ) # dummy function because df subsetting does not work with DSLite; MUST be replaced



      # create vector with ids that are used within this iteration
      cols_current <- dsBaseClient::ds.colnames("df_g_t_current", datasources = datasources_subsetted)[[1]] # get all columns
      indices_id <- match(idname, cols_current) # get position of id column

      dsBaseClient::ds.dataFrameSubset( # create actual vector
        df.name = "df_g_t_current",
        V1.name = paste0("df_g_t_current$", yname), # just dummy variables such that all rows are true
        V2.name = paste0("df_g_t_current$", yname),
        Boolean.operator = "==",
        keep.cols = indices_id,
        newobj = "ids_g_t",
        datasources = datasources_subsetted,
      )

      if (clear_console) {
        cat("\014")
      }

      length_ids <- dsBaseClient::ds.length("ids_g_t", datasources = datasources_subsetted)


      ds.subsetDf("df_g", tname, control_period, # create observations from the lag period that is used as period before the treatment
        include_zero = FALSE, newobj = "df_g_t_lag",
        datasources = datasources_subsetted
      ) # dummy function because df subsetting does not work with DSLite; MUST be replaced


      # create y from the current period t and its lags
      call_y_current <- paste("df_g_t_current$", yname, sep = "")
      dsBaseClient::ds.make(call_y_current, newobj = "y_t", datasources = datasources_subsetted)



      # create y from the lag period
      call <- paste("df_g_t_lag$", yname, sep = "")
      dsBaseClient::ds.make(call, newobj = "y_lag", datasources = datasources_subsetted)

      # create vector of outcome differences
      dsBaseClient::ds.make("y_t - y_lag", "delta_y", datasources = datasources_subsetted)


      # create data frame with covariates, X variables, and G
      dsBaseClient::ds.dataFrame(stringsAsFactors = FALSE,
        x = c("delta_y", paste0("df_g_t_lag$", gname)),
        newobj = "df_delta_y_g", datasources = datasources_subsetted
      )



      # save X vector from lag period as matrix of controls; generate object since this subsequently used to compute
      if (!is.null(xformla)) {
        # Get the names of the x-variables in vector
        columns_x <- labels(stats::terms(stats::as.formula(paste("delta_y ~", xformla))))

        cols <- dsBaseClient::ds.colnames("df_g_t_lag", datasources = datasources_subsetted)[[1]]
        indices <- match(columns_x, cols)


        if ((base_period == "universal") & (t < g)) { # use covariates from earlier period as covariates, which is in this case the current
          dsBaseClient::ds.dataFrameSubset(
            df.name = "df_g_t_current",
            V1.name = paste0("df_g_t_current$", yname), # just dummy avriables such that all rows are true
            V2.name = paste0("df_g_t_current$", yname),
            Boolean.operator = "==",
            keep.cols = indices,
            newobj = "covariates",
            datasources = datasources_subsetted,
          )
        } else {
          dsBaseClient::ds.dataFrameSubset(
            df.name = "df_g_t_lag",
            V1.name = paste0("df_g_t_lag$", yname),
            V2.name = paste0("df_g_t_lag$", yname),
            Boolean.operator = "==",
            keep.cols = indices,
            newobj = "covariates",
            datasources = datasources_subsetted,
          )
        }

        if (clear_console) {
          cat("\014")
        }

        ds.addColumnOnes("covariates",
          columns = columns_x,
          newobj = "covariates_one", datasources = datasources_subsetted
        )

        dsBaseClient::ds.cbind(
          x = c("df_delta_y_g", "covariates_one"), newobj = "df_analysis",
          datasources = datasources_subsetted
        )
      } else { # case without covariates
        columns_df_delta <- dsBaseClient::ds.colnames("df_delta_y_g", datasources = datasources_subsetted)[[1]]

        ds.addColumnOnes("df_delta_y_g",
          columns = columns_df_delta,
          newobj = "df_analysis", datasources = datasources_subsetted
        )
      }

      #--------------------------

      datasources_untreated <- datasources_subsetted[as.vector(do.call(rbind, ds.enoughIndividuals("df_g",
                                                                        colname = gname,
                                                                        value = "0",
                                                                        datasources = datasources_subsetted)))]


      # subset created to only non-treated data frame (currently df[,gname] == 0) for linear regression
      dsBaseClient::ds.dataFrameSubset(
        df.name = "df_analysis", V1.name = paste0("df_analysis$", gname), V2.name = "0",
        Boolean.operator = "==", newobj = "df_analysis_non_treated",
        datasources = datasources_untreated
      )

      if (!is.null(xformla)) { # compute expectation
        # run linear regression -> E(delta_y|X, G = 0)
        linear_regression_object <- #try({
          dsBaseClient::ds.glm(formula_linear_model,
          data = "df_analysis_non_treated",
          family = "gaussian",
          maxit = maxit,
          datasources=datasources_untreated)

          betas_lin_reg <- linear_regression_object$coefficients[, c(1)]
          #})

        #if(class(linear_regression_object) == "try-error") {
        #  k <<- k + 1
        #  X_T_X <- Reduce("+", ds.computeMatrixCrossproduct("covariates_one",
        #                                                    datasources_untreated ))
        #  covs <- c()
        #  mean_y_temp <- ds.mean("df_analysis$delta_y", type="combined",
        #                         datasources = datasources_untreated )$Global.Mean[1]
        #  for (i in labels(terms(formula_linear_model))){
        #    cov_temp <- ds.cov("df_analysis$delta_y", paste0("df_analysis$", i),
        #                       type="combine",
        #                       datasources = datasources_untreated)
        #    n_temp <- cov_temp$`Number of complete cases used`[2,1]

        #    mean_x <- ds.mean(paste0("df_analysis$", i), type="combined",
        #                      datasources = datasources_untreated )$Global.Mean[1]

        #    covs <- c(covs, cov_temp$`Variance-Covariance Matrix`[2,1]*(n_temp-1) + n_temp * mean_x * mean_y_temp )
        #  }

        #  betas_lin_reg <- solve(X_T_X) %*% as.matrix(covs)

        #}

        ds.multiplyMatrixMatrix("covariates_one", betas_lin_reg,
                                newobj = "delta_y_fitted",
                                datasources = datasources_subsetted
        )

      } else { # if no covariates, expectation is just the mean

        mean_delta_y <- dsBaseClient::ds.mean("df_analysis_non_treated$delta_y",
          type = "combined",
          datasources = datasources_untreated
        )$Global.Mean[1]
        dsBaseClient::ds.rep(
          x1 = eval(paste(mean_delta_y)),
          times = "1",
          length.out = "delta_y", # very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "delta_y_fitted",
          datasources = datasources_subsetted
        )



        #dsBaseClient::ds.asMatrix("delta_y_fitted", newobj = "delta_y_fitted", datasources = datasources_subsetted)
      }

      dsBaseClient::ds.asNumeric(x.name = "delta_y_fitted", newobj = "delta_y_fitted", datasources = datasources_untreated)

      # run logit regression on P(G=g |X, G_g + C = 1) -> only treated in g or never treated (observations in df_analysis)
      ds.recode(paste0("df_analysis$", gname),
                replace_with = 1,
                replace = g,
                newobj = "G_dummy",
                datasources = datasources_subsetted)
      #dsBaseClient::ds.recodeValues(
      #  var.name = paste0("df_analysis$", gname),
      #  values2replace.vector = c(g),
      #  new.values.vector = c(1), newobj = "G_dummy",
      #  datasources = datasources_subsetted
      #) # create variable that is one for treated in g; rest 0

      dsBaseClient::ds.cbind(
        x = c("df_analysis", "G_dummy"), newobj = "df_analysis_logit",
        datasources = datasources_subsetted
      ) # create enw data frame with dummy

      if (!is.null(xformla)) {
        logit_regression_object <- dsBaseClient::ds.glm(formula_logistic_model,
          data = "df_analysis_logit",
          family = "binomial", maxit = maxit,
          viewVarCov = TRUE,
          datasources = datasources_subsetted
        ) # run logistic regression to compute P(G=g |X, G_g + C = 1)



        ds.genProp(formula_logistic_model, logit_regression_object$coefficients[, c(1)],
          "df_analysis_logit", "propensity_scores",
          datasources = datasources_subsetted, invlog = TRUE, constant_in_matrix = TRUE
        )
      } else {#no covariates
        mean_logit <- dsBaseClient::ds.mean("df_analysis_logit$G_dummy",
          type = "combined",
          datasources = datasources_subsetted
        )$Global.Mean[1]
        dsBaseClient::ds.rep(
          x1 = eval(paste(mean_logit)),
          times = "1",
          length.out = "df_analysis_logit$G_dummy", # very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "propensity_scores",
          datasources = datasources_subsetted
        )

        #dsBaseClient::ds.asMatrix("propensity_scores", newobj = "propensity_scores", datasources = datasources_subsetted)
      }

      if (clear_console) {
        cat("\014")
      }

      ds.computeOdds("propensity_scores", "G_dummy", newobj = "odds", datasources = datasources_subsetted)
      mean_G <- dsBaseClient::ds.mean("G_dummy", type = "combined", datasources = datasources_subsetted)$Global.Mean[1]
      mean_odds <- dsBaseClient::ds.mean("odds", type = "combined", datasources = datasources_subsetted)$Global.Mean[1] # odds if in control group; else zero
      dsBaseClient::ds.make("1 - df_analysis_logit$G_dummy", "weights_ols", datasources = datasources_subsetted)
      dsBaseClient::ds.make("weights_ols * as.vector(propensity_scores)", "pscore_tr", datasources = datasources_subsetted)

      #-------------Differentiate with respect to estimators--------------------
      if (est_method == "reg") {
        dsBaseClient::ds.make("df_analysis_logit$G_dummy * (delta_y)", "att_treat",
          datasources = datasources_subsetted
        )

        dsBaseClient::ds.make("df_analysis_logit$G_dummy * (delta_y_fitted)", "att_cont",
          datasources = datasources_subsetted
        )
      } else if (est_method == "dr") {

        # treatment and control group for dr estimator
        dsBaseClient::ds.make("df_analysis_logit$G_dummy * (delta_y - delta_y_fitted)", "att_treat",
          datasources = datasources_subsetted
        )

        dsBaseClient::ds.make("odds * (delta_y - delta_y_fitted)", "att_cont",
          datasources = datasources_subsetted
        )
      } else if (est_method == "ipw") {
        # treatment and control group for ipw estimator
        dsBaseClient::ds.make("df_analysis_logit$G_dummy * (delta_y)", "att_treat",
          datasources = datasources_subsetted
        )

        dsBaseClient::ds.make("odds * (delta_y)", "att_cont",
          datasources = datasources_subsetted
        )
      } else {
        stop("Estimation method must either be reg, dr or ipw.")
      }

      if (clear_console) {
        cat("\014")
      }

      #-------------------------------------------------------------------------
      n <- dsBaseClient::ds.dim("df_analysis", datasources = datasources_subsetted)$`dimensions of df_analysis in combined studies`[1]


      # plug-in relevant estimator instead of "dr_att_treat/cont"
      mean_att_treat <- dsBaseClient::ds.mean("att_treat",
        type = "combined",
        datasources = datasources_subsetted
      )$Global.Mean[1]

      mean_att_cont <- dsBaseClient::ds.mean("att_cont",
        type = "combined",
        datasources = datasources_subsetted
      )$Global.Mean[1]

      eta_treat <- mean_att_treat / mean_G

      if (est_method %in% c("ipw", "dr")){
        eta_cont <- mean_att_cont / mean_odds
      } else {
        eta_cont <- mean_att_cont / mean_G
      }

      dr_att <- eta_treat - eta_cont # compute estimate
      #-------------------------------------------------------------------------


      # compute standard errors --------------------------------------------------
      if (is.null(xformla)) {
        dsBaseClient::ds.rep(
          x1 = "1",
          times = "1",
          length.out = "weights_ols", # very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "covariates_one",
          datasources = datasources_subsetted
        )

        #dsBaseClient::ds.asMatrix("covariates_one",
        #  newobj = "covariates_one",
        #  datasources = datasources_subsetted
        #)
      }

      # Asymptotic representation of OLS' betas
      dsBaseClient::ds.make("weights_ols * covariates_one", newobj = "wols_x", datasources = datasources_subsetted)
      dsBaseClient::ds.make("weights_ols * (delta_y - delta_y_fitted) * covariates_one",
        newobj = "wols_eX",
        datasources = datasources_subsetted
      )

      # TODO
      # check for compute Matrix crossproduct -> at least 5x5? and no identity?
      crossproduct_AX <- Reduce("+", ds.computeMatrixCrossproduct("wols_x",
        datasources = datasources_subsetted
      ))
      XpX_inv <- qr.solve(crossproduct_AX / n) #use qr.solve
      XpX_vector <- as.vector(XpX_inv)
      nrows <- nrow(XpX_inv)

      # TODO
      # check for compute Matrix crossproduct -> at least 5x5? and no identity?
      ds.multiplyMatrixMatrix("wols_eX", XpX_vector,
        nrow2 = nrows,
        ncol2 = nrows, newobj = "asy_lin_rep_wols", datasources = datasources_subsetted
      )

      # Asymptotic linear representation of logit's beta's
      dsBaseClient::ds.make("(df_analysis_logit$G_dummy - propensity_scores) * covariates_one",
        newobj = "score_ps", datasources = datasources_subsetted
      )

      if (!is.null(xformla)) {
        hessian_ps <- as.matrix(logit_regression_object$VarCovMatrix) * n
        hessian_ps_vector <- as.vector(hessian_ps)

        # TODO
        # check for compute Matrix crossproduct -> at least 5x5? and no identity?  see above
        ds.multiplyMatrixMatrix("score_ps", hessian_ps_vector,
          nrow2 = nrow(hessian_ps),
          ncol2 = ncol(hessian_ps), newobj = "asy_lin_rep_ps",
          datasources = datasources_subsetted
        )
      } else {
        hessian_ps <- 1 / (mean_G * (1 - mean_G)) # n drops our since formula for variance is: 1 / ( n * mean_G * (1-mean_G) ); which we would multiply by n
        hessian_ps_vector <- as.vector(hessian_ps)
        # TODO
        # check for compute Matrix crossproduct -> at least 5x5? and no identity?  see above
        ds.multiplyMatrixMatrix("score_ps", hessian_ps_vector,
          nrow2 = 1,
          ncol2 = 1, newobj = "asy_lin_rep_ps",
          datasources = datasources_subsetted
        )
      }

      if (clear_console) {
        cat("\014")
      }

      # Now, the influence function of the "treat" component
      # Leading term of the influence function: no estimation effect

      # only numbers are possible
      ds.sendToServer(eta_treat, newobj = "eta_treat_server",
                      datasources = datasources_subsetted)
      dsBaseClient::ds.make("(att_treat - df_analysis_logit$G_dummy * eta_treat_server)",
        newobj = "inf_treat_1",
        datasources = datasources_subsetted
      )

      # Estimation effect from beta hat
      # Derivative matrix (k x 1 vector)
      dsBaseClient::ds.make("df_analysis_logit$G_dummy * covariates_one",
        newobj = "M1_helper",
        datasources = datasources_subsetted
      )

      # ds.rowColCalc(x = "M1_helper", operation = "colMeans", newobj = "M1",
      #              datasources = datasources_subsetted) #only works if enough columns -> due to isVAlid line 40. but should work for already one


      # Now get the influence function related to the estimation effect related to beta's
      # ds.insertNumberToVector("M1", 0, 0, newobj = "M1_zeros",
      #                        datasources = datasources_subsetted)
      means_M1 <- c()
      if (!is.null(xformla)) {
        cols_covariates <- dsBaseClient::ds.colnames("M1_helper", datasources = datasources_subsetted)[[1]]
        for (i in 1:length(cols_covariates)) {
          variable <- paste0("M1_helper$", cols_covariates[i])
          means_M1[i] <- dsBaseClient::ds.mean(variable,
            type = "combined",
            datasources = datasources_subsetted
          )$Global.Mean[1]
        }
      } else {
        means_M1[1] <- dsBaseClient::ds.mean("M1_helper",
          type = "combined",
          datasources = datasources_subsetted
        )$Global.Mean[1]
      }

      ds.multiplyMatrixMatrix("asy_lin_rep_wols", means_M1,
        newobj = "inf_treat_2",
        datasources = datasources_subsetted
      )

      dsBaseClient::ds.asNumeric(x.name = "inf_treat_2", newobj = "inf_treat_2",
                   datasources = datasources_subsetted)

      # Influence function for the treated component

      # TODO
      # check function. dsBaseClient::ds.make?

      ds.sendToServer(mean_G, newobj = "mean_G",
                      datasources = datasources_subsetted)
      dsBaseClient::ds.make("inf_treat_1 / mean_G", newobj = "inf_treat_1_G",
              datasources = datasources_subsetted)
      dsBaseClient::ds.make("inf_treat_2 / mean_G", newobj = "inf_treat_2_G",
              datasources = datasources_subsetted)

      #inf_treat_object <- ds.computeInfTreatDifference("inf_treat_1", "inf_treat_2",
      #  datasources = datasources_subsetted
      #) # divide by mean of G later
      #inf_treat <- lapply(inf_treat_object, function(l) l[[1]])
      #noise_sd_treat <- lapply(inf_treat_object, function(l) l[[2]])
      #-----------------------------------------------------------------------------
      # Now, get the influence function of control component
      # Leading term of the influence function: no estimation effect

      ds.sendToServer(eta_cont, newobj = "eta_cont_server",
                      datasources = datasources_subsetted)

      if (est_method %in% c("dr", "ipw")){
        dsBaseClient::ds.make("(att_cont - odds * eta_cont_server)",
          newobj = "inf_cont_1",
          datasources = datasources_subsetted
        )
      } else {
        dsBaseClient::ds.make("(att_cont - df_analysis_logit$G_dummy * eta_cont_server)",
                              newobj = "inf_cont_1",
                              datasources = datasources_subsetted
        )
      }
      # inf.cont.1 <- (dr.att.cont - w.cont * eta.cont)


      # Estimation effect from gamma hat (pscore)
      # Derivative matrix (k x 1 vector)
      if (est_method == "dr") {
        dsBaseClient::ds.make("odds * (delta_y -  delta_y_fitted - eta_cont_server) * covariates_one",
          newobj = "M2_helper",
          datasources = datasources_subsetted
        )
      } else if (est_method == "ipw") {
        dsBaseClient::ds.make("odds * (delta_y - eta_cont_server) * covariates_one",
          newobj = "M2_helper",
          datasources = datasources_subsetted
        )
      } else {
        dsBaseClient::ds.make("odds  * covariates_one",
          newobj = "M2_helper",
          datasources = datasources_subsetted
        )
      }

      # rowcolcalc does not work yet
      means_M2 <- c()
      if (!is.null(xformla)) {
        cols_covariates <- dsBaseClient::ds.colnames("M2_helper", datasources = datasources_subsetted)[[1]]
        for (i in 1:length(cols_covariates)) {
          variable <- paste0("M2_helper$", cols_covariates[i])
          means_M2[i] <- dsBaseClient::ds.mean(variable,
            type = "combined",
            datasources = datasources_subsetted
          )$Global.Mean[1]
        }
      } else {
        means_M2[1] <- dsBaseClient::ds.mean("M2_helper",
          type = "combined",
          datasources = datasources_subsetted
        )$Global.Mean[1]
      }


      if (clear_console) {
        cat("\014")
      }

      # Now the influence function related to estimation effect of pscores

      if (est_method == "reg") {
        ds.multiplyMatrixMatrix("asy_lin_rep_wols", means_M1,
          newobj = "inf_cont_2",
          datasources = datasources_subsetted
        )
      } else {
        ds.multiplyMatrixMatrix("asy_lin_rep_ps", means_M2,
          newobj = "inf_cont_2",
          datasources = datasources_subsetted
        )
      }

      # Estimation Effect from beta hat (weighted OLS)
      dsBaseClient::ds.make("odds * covariates_one",
        newobj = "M3_helper",
        datasources = datasources_subsetted
      )

      means_M3 <- c()
      if (!is.null(xformla)) {
        cols_covariates <- dsBaseClient::ds.colnames("M3_helper", datasources = datasources_subsetted)[[1]]
        for (i in 1:length(cols_covariates)) {
          variable <- paste0("M3_helper$", cols_covariates[i])
          means_M3[i] <- dsBaseClient::ds.mean(variable,
            type = "combined",
            datasources = datasources_subsetted
          )$Global.Mean[1]
        }
      } else {
        means_M3[1] <- dsBaseClient::ds.mean("M3_helper",
          type = "combined",
          datasources = datasources_subsetted
        )$Global.Mean[1]
      }

      ds.multiplyMatrixMatrix("asy_lin_rep_wols", means_M3,
        newobj = "inf_cont_3",
        datasources = datasources_subsetted
      )

      dsBaseClient::ds.asNumeric(x.name = "inf_cont_3", newobj = "inf_cont_3",
                   datasources = datasources_subsetted)



      # Influence function for the control component
      # compute in1 + inf2 -> compute difference with function, compute mean, compute inf.control
      dsBaseClient::ds.asNumeric(x.name = "inf_cont_1", newobj = "inf_cont_1",
                                 datasources = datasources_subsetted)
      dsBaseClient::ds.asNumeric(x.name = "inf_cont_2", newobj = "inf_cont_2",
                                 datasources = datasources_subsetted)

      dsBaseClient::ds.make("inf_cont_1 + inf_cont_2",
        newobj = "inf_cont_helper",
        datasources = datasources_subsetted
      )


      odds_mean <- dsBaseClient::ds.mean("odds", type = "combined",
                           datasources = datasources_subsetted)$Global.Mean[1]

      ds.sendToServer(odds_mean, newobj = "odds_mean",
                      datasources = datasources_subsetted)
      dsBaseClient::ds.make("inf_cont_helper / odds_mean", newobj = "inf_cont_helper_p",
              datasources = datasources_subsetted)

      dsBaseClient::ds.make("inf_cont_3 / odds_mean", newobj = "inf_cont_3_p",
              datasources = datasources_subsetted)

      if (clear_console) {
        cat("\014")
      }

      # get the influence function of the DR estimator (put all pieces together)
      if (est_method == "dr") {

        dsBaseClient::ds.make("inf_treat_1_G - inf_treat_2_G - inf_cont_helper_p + inf_cont_3_p",
                newobj="dr_att_inf_func", datasources = datasources_subsetted)

        } else if (est_method == "ipw") {

        dsBaseClient::ds.make(paste0("inf_treat_1 / ", mean_G),
                newobj = "inf_treat", datasources = datasources_subsetted)

          dsBaseClient::ds.make(paste0("inf_cont_helper / ", odds_mean),
                newobj = "inf_control", datasources = datasources_subsetted)

        dsBaseClient::ds.make("inf_treat_1_G - inf_cont_helper_p ",
                newobj="dr_att_inf_func", datasources = datasources_subsetted)

        } else if (est_method == "reg") {

          dsBaseClient::ds.make(paste0("inf_cont_helper / ", mean_G),
                                newobj = "inf_control", datasources = datasources_subsetted)

          dsBaseClient::ds.make("inf_treat_1_G - inf_control ",
                                newobj="dr_att_inf_func", datasources = datasources_subsetted)
        }

      #-----------------------------------------------------------------------------
      # create empty matrix with all ids for influence
      n_columns <- length(t_periods) * length(g_periods)

      if (index_iteration == 1) {
        ds.createEmptyIdMatrix("sorted_data", idname, paste(n_columns),
          newobj = "influence_matrix", datasources = datasources_subsetted
        )

        ds.createEmptyIdMatrix("sorted_data", idname, paste(n_columns),
                               newobj = "influence_matrix_not_divided",
                               datasources = datasources_subsetted
        )

        ds.createEmptyIdMatrix("sorted_data", idname, paste(n_columns),
                               newobj = "test_matrix", datasources = datasources_subsetted
        )
      }

      # send dr_att_inf_func and ids to servers loop over connections and ids_length and append to matrix
      #last_index <- 0 # initialize index of individuals in dr_att_inf_func (they are ordered)

      #for (i in 1:length(datasources_subsetted)) {
       # first_index <- last_index + 1 # choose next index
      #  last_index <- last_index + length_ids[[i]]
      #  influence_source <- dr_att_inf_func[(first_index:last_index)] / n # divide by n (which is n1) in the paper

        # TODO recheck function
      ds.sendToServer(n, newobj = "n", datasources = datasources_subsetted)
      dsBaseClient::ds.make("dr_att_inf_func / n",
                                             newobj="dr_att_inf_func_n",
                                             datasources = datasources_subsetted)

      ds.AppendInfluence(df = "influence_matrix", influences = "dr_att_inf_func_n",
                         id_period_vector = "ids_g_t",
          column = index_iteration,
          newobj = "influence_matrix",
          datasources = datasources_subsetted#[[i]]
      )

      ds.AppendInfluence(df = "influence_matrix_not_divided",
                         influences = "dr_att_inf_func", id_period_vector= "ids_g_t",
                         column = index_iteration,
                         newobj = "influence_matrix_not_divided",
                         datasources = datasources_subsetted#[[i]]
      )
      #}


      # Estimate of asymptotic standard error
      #correction <- 0
      #if (correct_asymp_std_errors) {
      #  correction <- sum( (unlist(noise_sd_control_difference)^2 / mean_G^2 + unlist(noise_sd_treat)^2 / mean_odds^2) * unlist(sample_sizes) ) / n^2
      #}

      dsBaseClient::ds.make("dr_att_inf_func * dr_att_inf_func", newobj="psi_inner_product",
              datasources = datasources_subsetted)

      #mean(t(psi) * psi) = psi^T * psi / n -> need to divide gloabl mean by sqrt n
      se_dr_att <- (dsBaseClient::ds.mean("psi_inner_product", type = "combined",
                                        datasources=datasources_subsetted)$Global.Mean[1])^0.5 / sqrt(n)

      #se_dr_att <- (((t(dr_att_inf_func) %*% dr_att_inf_func / n)^0.5 / sqrt(n))^2 - correction)^0.5
      #dr_att_inf_func <- unlist(ds.buildHelper("dr_att_inf_func", datasources = datasources_subsetted))
      #inf <- cbind(inf, dr_att_inf_func)
      z_value <- stats::qnorm(1 - alpha / 2)

      length_ci <- z_value * se_dr_att
      upper_bound <- dr_att + length_ci
      lower_bound <- dr_att - length_ci

      # result <- DSI::datashield.assign(datasources, "dp", cally)
      asymptotic <- c(
        "Group" = g, "Time" = t,
        "ATT" = dr_att, "SE" = se_dr_att, "[95% Pointwise" = lower_bound,
        "Conf. Band]" = upper_bound
      )

      # if (g <= t){ #include only post-treatment periods
      out <- rbind(out, asymptotic)
      # }
    } # end t
  } # end g

  se <- out[, "SE"]

  dimensions_influence <- dsBaseClient::ds.dim("influence_matrix", datasources = datasources_subsetted)
  n_global <- dimensions_influence[[length(dimensions_influence)]][1]
  n_std_error <- n_global # is overwritten if clustered standard errors; otherwise we cluster at the individual level

  ds.sendToServer(n_global, newobj = "n_global",
                  datasources = datasources_subsetted)

  ds.multiplyMatrixScalar("influence_matrix", "n_global",
                          newobj="influence_matrix_adjusted",
                          datasources = datasources_subsetted)

  name_influence_use <- "influence_matrix_adjusted"

  #needed for output and bootstrap
  dp <- did::DIDparams(yname=yname,
                  tname=tname,
                  idname=idname,
                  gname=gname,
                  xformla=xformla,
                  data=NULL,
                  control_group=control_group,
                  anticipation=anticipation,
                  weightsname=NULL,
                  alp=alpha,
                  bstrap=bstrap,
                  biters=biters,
                  clustervars=clustervars,
                  cband=cband,
                  print_details=FALSE,
                  pl=FALSE,
                  cores=1,
                  est_method=est_method,
                  base_period=base_period,
                  panel=TRUE,
                  true_repeated_cross_sections=FALSE,
                  n=n_global,
                  nG=length(g_periods),
                  nT=length(t_periods),
                  tlist=t_periods,
                  glist=g_periods,
                  call=match.call())


  # bootstrap
  if (bstrap) {
    if (!is.null(clustervars)) { # clusters are only possible within a server
      #---------this needs to be changed to allow for global clusters--------- -> dsBaseClient::ds.meanByClass only allows for 3 clusters
      # TODO check function
      ds.clusterInfluenceFunction("sorted_data", "influence_matrix_adjusted",
        clustervars,
        idname,
        newobj = "influence_matrix_adjusted_cluster",
        datasources = datasources_subsetted
      )


      name_influence_use <- "influence_matrix_adjusted_cluster"
      dimensions_influence_clust <- dsBaseClient::ds.dim("influence_matrix_adjusted_cluster",
        datasources = datasources_subsetted
      )
      n_std_error <- dimensions_influence_clust[[length(dimensions_influence_clust)]][1]

      #-----------------------------------------------------------------------
    }

    #cannot use original mboot functionsince this function requires the data to be in dp
    bootstrap_samples <- ds.multiplierBootstrap(name_influence_use, biters,
      datasources = datasources_subsetted
    ) # everything still needs to be divided by number of cluster


    bootstrap_sample_combined <- Reduce("+", bootstrap_samples) / n_std_error
    bres <- sqrt(n_std_error) * bootstrap_sample_combined


    # from main package
    # for uniform confidence band
    # compute new critical value
    # see paper for details
    b_sigma <- apply(
      bres, 2,
      function(b) {
        (stats::quantile(b, .75, type = 1, na.rm = T) -
          stats::quantile(b, .25, type = 1, na.rm = T)) / (stats::qnorm(.75) - stats::qnorm(.25))
      }
    )
    se <- as.numeric(b_sigma) / sqrt(n_std_error)

    b_sigma[b_sigma <= sqrt(.Machine$double.eps) * 10] <- NA

    if (cband) {
      # sup-t confidence band
      bT <- apply(bres, 1, function(b) max(abs(b / b_sigma), na.rm = TRUE))
      z_value <- stats::quantile(bT, 1 - alpha, type = 1, na.rm = T)
      if (z_value >= 7) {
        warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
      }
    }

    lower_bounds <- out[, "ATT"] - z_value * se
    upper_bounds <- out[, "ATT"] + z_value * se

    # overwrite asymptotic values
    out[, "SE"] <- se
    out[, "[95% Pointwise"] <- lower_bounds
    out[, "Conf. Band]"] <- upper_bounds
  }

  #-------------------Wald-Test for parallel trend assumption-------------------
  # TODO check function; as above
  #TODO reduce using + and divide by n_global

  V <- Reduce("+", ds.computeMatrixCrossproduct("influence_matrix_not_divided",
                                    datasources = datasources_subsetted)) / n_global

  pre <-out[, "Group"] > out[, "Time"]
  pre_att <-as.matrix(out[pre, "ATT"])
  pre_V <-as.matrix(V[pre, pre])

  #-----------start from original package --------------------------------------
  # check if there are actually any pre-treatment periods
  if (length(pre_V) == 0) {
    message("No pre-treatment periods to test")
    W <- NULL
    W_pval <- NULL
  } else if (sum(is.na(pre_V))) {
    warning("Not returning pre-test Wald statistic due to NA pre-treatment values")
    W <- NULL
    W_pval <- NULL
  } else if (rcond(pre_V) <= .Machine$double.eps) {
    # singluar covariance matrix for pre-treatment periods
    warning("Not returning pre-test Wald statistic due to singular covariance matrix")
    W <- NULL
    W_pval <- NULL
  } else {
    # everything is working...
    W <-n_global * t(pre_att) %*% solve(pre_V) %*% pre_att
    q <- length(c(pre_att)) # number of restrictions
    W_pval <- round(1 - stats::pchisq(W, q), 5)
  }
  #---------End from original package-------------------------------------------


  #return(out)
  #change output such that it matches original did package
  tt <- rep(t_periods, length(g_periods))
  group <- rep(g_periods,each=length(t_periods))
  att <- out[, "ATT"]




  return(did::MP(group=group, t=tt, att=att, V_analytical=V, se=se, c=z_value,
            inffunc=NULL, n=n_global, W=W, Wpval=W_pval, alp = alpha, DIDparams=dp))
}
