#' Compute did estimate
#' client side function ...
#'
#' @param df table name
#' @param pos distance measure
#' @param form formula
#'
#' @return
#' @export

ds.did <- function(data, yname, tname, idname, gname, t_periods, g_periods,
                   xformla = NULL,
                   panel = TRUE, allow_unbalanced_panel = FALSE,
                   control_group = "notyettreated",
                   base_period = "varying",
                   anticipation = 0, alpha = 0.05,
                   bstrap = FALSE, biters=1000, cband = TRUE, clustervars = NULL,
                   est_method = "dr", maxit = 10000,
                   datasources = NULL) {
  #TODOs

  #list of t and g created automatically
  #create input checks (are all inputs of ds.did valid) -> for example: create warning if t=1 is in "varying" period
  #create input checks for all ds functions
  #write proper docstrings

  #panel input currently not implemented
  #allow_unbalanced_panel input currently not implemented


  #-------------------------------Checks of inputs------------------------------

  #- Check for data sources
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }


  #TODO
  #check that all relevant names are within each servers columns


  #TODO
  #check that other inputs are valid




  # sort data frame by first id and second time
  vec_sort <- paste0("c(", data, "$", idname, ",", data, "$", tname, ")")

  ds.dataFrameSort(df.name = data, sort.key.name = vec_sort, newobj = "sorted_data",
                   datasources = datasources)

  ds.completeCases(x1 = "sorted_data", newobj = "sorted_data",
                   datasources = datasources)

  if (is.null(xformla)){
   xformla_paste <- ""
  } else{
    xformla_paste <- xformla
  }

  # y_delta is the name we give to the difference of Y observations in t and g - anticipation - 1
  # The name is assigned within the loop below at each period respectively
  formula_linear_model <- as.formula(paste("delta_y", "~ constant +", xformla_paste, "-1"))

  # G_t is the name of the dummy variable that indicates if an individuals receives
  # treatment in period g. The name is assigned within the loop below at each
  # period respectively
  formula_logistic_model <- as.formula(paste("G_dummy", "~ constant +", xformla_paste, "-1"))


  if (base_period == "varying") {
    #check if 1 is in t; if yes, delete it
  }

  #get unique values of g and t periods? Guess that's not possible? -> inputs

  index_iteration <- 0

  out <- c() # creates vector that is used for output all ATTs, SEs and CIs
  for (g in g_periods){
    for (t in t_periods){





      index_iteration <- index_iteration + 1

      #varying base periods or universal ones; for explanaition see original paper https://github.com/bcallaway11/did/blob/master/R/att_gt.R
      if (base_period == "varying"){
        if (g <=t){
          control_period <- g - anticipation - 1
        } else{
          control_period <- t - 1
        }
      } else if (base_period == "universal"){
        control_period <- g - anticipation - 1
      } else{
        stop("base_period must either be universal or varying")
      }

      g_string <- paste(g)
      t_string <- paste(t)

      # V1_name_g <- paste(data,"$", gname, sep="")
      # V2_name_t <- paste(data,"$", tname, sep="")
      # keep only the individuals that are treated in g = 11 or never treated (must currently be g=0)
      # ds.dataFrameSubset(df.name = data, V1.name = V1_name_g, V2.name = g_string,
      #                   Boolean.operator = "==", newobj = "df_g",
      #                   datasources= connections)

      # ds.dataFrameSubset(df.name = "df_g", V1.name = V1_name_t, V2.name = t_string,
      #                   Boolean.operator = "==", newobj = "df_g",
      #                   datasources= connections)

      if ((control_group == "nevertreated") ){
        #TODO recheck function
        ds.subsetDf("sorted_data", gname, g,
          include_zero = TRUE, newobj = "df_g",
          datasources = datasources
        ) # dummy function because df subsetting does not work with DSLite; MUST be replaced
      } else if (control_group == "notyettreated"){
        #TODO recheck function
        ds.generateNotYetTreated("sorted_data", gname, t, g, newobj = "df_g",
                                 datasources=datasources) #not yet treatd values for g are already set to zero

      }



      #TODO
      #check if all studies have sufficient many individuals in -> otherwise drop this study for the
      #period/treatment time -> at least 5?
      ds.subsetDf("df_g", tname, t,# create observations only from the current period t
        include_zero = FALSE, newobj = "df_g_t_current",
        datasources = datasources
      ) # dummy function because df subsetting does not work with DSLite; MUST be replaced


      #create vector with ids that are used within this iteration
      cols_current <- ds.colnames("df_g_t_current", datasources = datasources)[[1]] #get all columns
      indices_id <- match(idname, cols_current) #get position of id column

      ds.dataFrameSubset( #create actual vector
        df.name = "df_g_t_current",
        V1.name = paste0("df_g_t_current$", yname), #just dummy variables such that all rows are true
        V2.name = paste0("df_g_t_current$", yname),
        Boolean.operator = "==",
        keep.cols = indices_id,
        newobj = "ids_g_t",
        datasources = datasources,
      )


      length_ids <- ds.length("ids_g_t",  datasources = datasources)


      #TODO
      #check if all studies have sufficient many individuals in -> otherwise drop this study for the
      #period/treatment time -> at least 5? -> see above
      ds.subsetDf("df_g", tname, control_period, # create observations from the lag period that is used as period before the treatment
        include_zero = FALSE, newobj = "df_g_t_lag",
        datasources = datasources
      ) # dummy function because df subsetting does not work with DSLite; MUST be replaced


      # create y from the current period t and its lags
      call_y_current <- paste("df_g_t_current$", yname, sep = "")
      ds.make(call_y_current, newobj = "y_t", datasources = datasources)


      # create y from the lag period
      call <- paste("df_g_t_lag$", yname, sep = "")
      ds.make(call, newobj = "y_lag", datasources = datasources)

      # create vector of outcome differences
      ds.make("y_t - y_lag", "delta_y", datasources = datasources)



      # create data frame with covariates, X variables, and G
      ds.dataFrame(
        x = c("delta_y", paste0("df_g_t_lag$", gname)),
        newobj = "df_delta_y_g", datasources = datasources
      )



      # save X vector from lag period as matrix of controls; generate object since this subsequently used to compute
      if (!is.null(xformla)){
        # Get the names of the x-variables in vector
        columns_x <- labels(terms(as.formula(paste("delta_y ~", xformla))))

        cols <- ds.colnames("df_g_t_lag", datasources = datasources)[[1]]
        indices <- match(columns_x, cols)


        if ((base_period == "universal") & (t < g)){ #use covariates from earlier period as covariates, which is in this case the current
          ds.dataFrameSubset(
            df.name = "df_g_t_current",
            V1.name = paste0("df_g_t_current$", yname), #just dummy avriables such that all rows are true
            V2.name = paste0("df_g_t_current$", yname),
            Boolean.operator = "==",
            keep.cols = indices,
            newobj = "covariates",
            datasources = datasources,
          )


        } else{
          ds.dataFrameSubset(
            df.name = "df_g_t_lag",
            V1.name = paste0("df_g_t_lag$", yname),
            V2.name = paste0("df_g_t_lag$", yname),
            Boolean.operator = "==",
            keep.cols = indices,
            newobj = "covariates",
            datasources = datasources,
          )
        }

        #TODO or write disclosiveness checks
        #change by cbind? and as.matrix?
        ds.addColumnOnes("covariates", columns =  columns_x,
                         newobj = "covariates_one", datasources = datasources)

        ds.cbind(
          x = c("df_delta_y_g", "covariates_one"), newobj = "df_analysis",
          datasources = datasources
        )
      }else{#case without covariates
        columns_df_delta <- ds.colnames("df_delta_y_g")[[1]]
        #TODO or write disclosiveness checks
        #change by cbind? and as.matrix?
        ds.addColumnOnes("df_delta_y_g", columns = columns_df_delta,
                         newobj = "df_analysis", datasources = datasources)
      }



      # subset created to only non-treated data frame (currently df[,gname] == 0) for linear regression
      ds.dataFrameSubset(
        df.name = "df_analysis", V1.name = paste0("df_analysis$", gname), V2.name = "0",
        Boolean.operator = "==", newobj = "df_analysis_non_treated",
        datasources = datasources
      )



      if (!is.null(xformla)){#compute expectation
        #TODO
        #include checks for overparameterization similar to DID package


        # run linear regression -> E(delta_y|X, G = 0)
        linear_regression_object <- ds.glm(formula_linear_model,
          data = "df_analysis_non_treated",
          family = "gaussian",
          maxit = maxit
        )



        betas_lin_reg <- linear_regression_object$coefficients[, c(1)]


        #TODO
        #include checks -> especially that none of both can be the identity matrix
        #maybe at least 5x5 matrix?
        ds.multiplyMatrixVector(betas_lin_reg, "covariates_one",
          newobj = "delta_y_fitted",
          datasources = datasources
        )
      } else{#if no covariates, expectation is just the mean

        mean_delta_y <- ds.mean("df_analysis_non_treated$delta_y",
                                type = "combined",
                                datasources = datasources)$Global.Mean[1]
        ds.rep(
          x1 = eval(paste(mean_delta_y)),
          times = "1",
          length.out = "delta_y", #very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "delta_y_fitted",
          datasources = datasources
        )

        ds.asMatrix("delta_y_fitted", newobj = "delta_y_fitted", datasources = datasources)

      }

      # run logit regression on P(G=g |X, G_g + C = 1) -> only treated in g or never treated (observations in df_analysis)
      ds.recodeValues(
        var.name = paste0("df_analysis$", gname),
        values2replace.vector = c(g),
        new.values.vector = c(1), newobj = "G_dummy",
        datasources = datasources
      ) # create variable that is one for treated in g; rest 0

      ds.cbind(
        x = c("df_analysis", "G_dummy"), newobj = "df_analysis_logit",
        datasources = datasources
      ) # create enw data frame with dummy

      if (!is.null(xformla)){
        #TODO include checks from DID package
        logit_regression_object <- ds.glm(formula_logistic_model,
          data = "df_analysis_logit",
          family = "binomial", maxit = maxit,
          viewVarCov = TRUE
        ) # run logistic regression to compute P(G=g |X, G_g + C = 1)

        #TODO
        #include checks for gen prop function
        ds.genProp(logit_regression_object$formula, logit_regression_object$coefficients[, c(1)],
          "df_analysis_logit", "propensity_scores",
          datasources = datasources, invlog = TRUE, constant_in_matrix =TRUE
        )

      } else{
        mean_logit <- ds.mean("df_analysis_logit$G_dummy",
                                 type = "combined",
                                 datasources = datasources)$Global.Mean[1]
        ds.rep(
          x1 = eval(paste(mean_logit)),
          times = "1",
          length.out = "df_analysis_logit$G_dummy", #very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "propensity_scores",
          datasources = datasources
        )

        ds.asMatrix("propensity_scores", newobj = "propensity_scores", datasources = datasources)


      }

      #TODO
      #include checks for odds computations (check if worked properly)
      ds.computeOdds("propensity_scores", "G_dummy", newobj = "odds", datasources = datasources)


      mean_G <- ds.mean("G_dummy", type = "combined", datasources = datasources)$Global.Mean[1]

      mean_odds <- ds.mean("odds", type = "combined",datasources = datasources)$Global.Mean[1] # odds if in control group; else zero

      ds.make("1 - df_analysis_logit$G_dummy", "weights_ols", datasources = datasources)
      ds.make("weights_ols * propensity_scores", "pscore_tr", datasources = datasources)


      #-------------Differentiate with respect to estimators--------------------
      if (est_method == "reg"){
        ds.make("df_analysis_logit$G_dummy * (delta_y)", "att_treat",
                datasources = datasources
        )

        ds.make("odds * (delta_y_fitted)", "att_cont",
                datasources = datasources
        )

      } else if (est_method == "dr") {

          #treatment and control group for dr estimator
          ds.make("df_analysis_logit$G_dummy * (delta_y - delta_y_fitted)", "att_treat",
            datasources = datasources
          )

          ds.make("odds * (delta_y - delta_y_fitted)", "att_cont",
            datasources = datasources
          )
      } else if (est_method == "ipw"){
          #treatment and control group for ipw estimator
          ds.make("df_analysis_logit$G_dummy * (delta_y)", "att_treat",
                  datasources = datasources
          )

          ds.make("odds * (delta_y)", "att_cont",
                  datasources = datasources
          )
      } else{
        stop("Estimation method must either be reg, dr or ipw.")
      }

      #-------------------------------------------------------------------------

      # weights.ols = weights_ols
      # int.cov = covariates_one
      # deltaY = delta_y
      # out.delta = delta_y_fitted
      # ps.fit = propensity_scores
      # D <- df_analysis_logit$G_dummy
      # pscore.tr = pscore_tr
      # w.treat = df_analysis_logit$G_dummy
      # w.cont = odds
      # dr.att = dr_att

      n <- ds.dim("df_analysis", datasources = datasources)$`dimensions of df_analysis in combined studies`[1]


      #plug-in relevant estimator instead of "dr_att_treat/cont"
      mean_att_treat <- ds.mean("att_treat",
        type = "combined",
        datasources = datasources
      )$Global.Mean[1]

      mean_att_cont <- ds.mean("att_cont",
        type = "combined",
        datasources = datasources
      )$Global.Mean[1]

      eta_treat <- mean_att_treat / mean_G
      eta_cont <- mean_att_cont / mean_odds
      dr_att <- eta_treat - eta_cont # compute estimate


      # compute standard errors --------------------------------------------------
      if (is.null(xformla)){
        ds.rep(
          x1 = "1",
          times = "1",
          length.out = "weights_ols", #very arbitrary what object is used here
          each = "1",
          source.x1 = "serverside",
          source.times = "serverside",
          source.length.out = "serverside",
          source.each = "serverside",
          newobj = "covariates_one",
          datasources = datasources
        )

        ds.asMatrix("covariates_one", newobj="covariates_one",
                    datasources = datasources)
      }

      # Asymptotic representation of OLS' betas
      ds.make("weights_ols * covariates_one", newobj = "wols_x", datasources = datasources)
      ds.make("weights_ols * (delta_y - delta_y_fitted) * covariates_one",
        newobj = "wols_eX",
        datasources = datasources
      )

      #TODO
      #check for compute Matrix crossproduct -> at least 5x5? and no identity?
      crossproduct_AX <- Reduce("+", ds.computeMatrixCrossproduct("wols_x"))
      XpX_inv <- solve(crossproduct_AX / n)
      XpX_vector <- as.vector(XpX_inv)
      nrows <- nrow(XpX_inv)

      #TODO
      #check for compute Matrix crossproduct -> at least 5x5? and no identity?
      ds.multiplyMatrixMatrix("wols_eX", XpX_vector,
        nrow2 = nrows,
        ncol2 = nrows, newobj = "asy_lin_rep_wols"
      )

      # Asymptotic linear representation of logit's beta's
      ds.make("(df_analysis_logit$G_dummy - propensity_scores) * covariates_one",
        newobj = "score_ps", datasources = datasources
      )

      if (!is.null(xformla)){
        hessian_ps <- as.matrix(logit_regression_object$VarCovMatrix) * n
        hessian_ps_vector <- as.vector(hessian_ps)

        #TODO
        #check for compute Matrix crossproduct -> at least 5x5? and no identity?  see above
        ds.multiplyMatrixMatrix("score_ps", hessian_ps_vector,
          nrow2 = nrow(hessian_ps),
          ncol2 = ncol(hessian_ps), newobj = "asy_lin_rep_ps",
          datasources = datasources
        )
      } else{
        hessian_ps <- 1 / ( mean_G * (1-mean_G) )   #n drops our since formula for variance is: 1 / ( n * mean_G * (1-mean_G) ); which we would multiply by n
        hessian_ps_vector <- as.vector(hessian_ps)
        #TODO
        #check for compute Matrix crossproduct -> at least 5x5? and no identity?  see above
        ds.multiplyMatrixMatrix("score_ps", hessian_ps_vector,
                                nrow2 = 1,
                                ncol2 = 1, newobj = "asy_lin_rep_ps",
                                datasources = datasources
        )

      }

      # Now, the influence function of the "treat" component
      # Leading term of the influence function: no estimation effect
      #TODO
      #this will definitely not work, I think.
      ds.sendToServer(eta_treat, newobj = "eta_treat_server", datasources = datasources)
      ds.make("(att_treat - df_analysis_logit$G_dummy * eta_treat_server)",
        newobj = "inf_treat_1",
        datasources = datasources
      )

      # Estimation effect from beta hat
      # Derivative matrix (k x 1 vector)
      ds.make("df_analysis_logit$G_dummy * covariates_one",
        newobj = "M1_helper",
        datasources = datasources
      )

      # ds.rowColCalc(x = "M1_helper", operation = "colMeans", newobj = "M1",
      #              datasources = datasources) #only works if enough columns -> due to isVAlid line 40. but should work for already one


      # Now get the influence function related to the estimation effect related to beta's
      # ds.insertNumberToVector("M1", 0, 0, newobj = "M1_zeros",
      #                        datasources = datasources)
      means_M1 <- c()
      if (!is.null(xformla)){
        cols_covariates <- ds.colnames("M1_helper")[[1]]
        for (i in 1:length(cols_covariates)) {
          variable <- paste0("M1_helper$`", cols_covariates[i], "`")
          means_M1[i] <- ds.mean(variable,
            type = "combined",
            datasources = datasources
          )$Global.Mean[1]
        }
      } else{
        means_M1[1] <-  ds.mean("M1_helper",
                                type = "combined",
                                datasources = datasources
        )$Global.Mean[1]
      }

      #TODO
      #this will definitely not work, I think.
      ds.sendToServer(means_M1, newobj = "M1", datasources = datasources)

      ds.make("asy_lin_rep_wols %*% M1",
        newobj = "inf_treat_2",
        datasources = datasources
      )

      # Influence function for the treated component

      #TODO
      #check function. ds.make?
      inf_treat <- ds.computeInfTreatDifference("inf_treat_1", "inf_treat_2",
        datasources = datasources
      ) # divide by mean of G later
      #-----------------------------------------------------------------------------
      # Now, get the influence function of control component
      # Leading term of the influence function: no estimation effect
      #TODO
      #this will definitely not work, I think.
      ds.sendToServer(eta_cont, newobj = "eta_cont_server", datasources = datasources)

      ds.make("(att_cont - odds * eta_cont_server)",
        newobj = "inf_cont_1",
        datasources = datasources
      )
      # inf.cont.1 <- (dr.att.cont - w.cont * eta.cont)


      # Estimation effect from gamma hat (pscore)
      # Derivative matrix (k x 1 vector)
      if (est_method == "dr"){
        ds.make("odds * (delta_y -  delta_y_fitted - eta_cont_server) * covariates_one",
          newobj = "M2_helper",
          datasources = datasources
        )

      } else if(est_method == "ipw"){
        ds.make("odds * (delta_y - eta_cont_server) * covariates_one",
                newobj = "M2_helper",
                datasources = datasources
        )

      } else{
        ds.make("odds  * covariates_one",
                newobj = "M2_helper",
                datasources = datasources
        )
      }

      # rowcolcalc does not work yet
      means_M2 <- c()
      if (!is.null(xformla)){
          cols_covariates <- ds.colnames("M2_helper")[[1]]
          for (i in 1:length(cols_covariates)) {
            variable <- paste0("M2_helper$`", cols_covariates[i], "`")
            means_M2[i] <- ds.mean(variable,
              type = "combined",
              datasources = datasources
            )$Global.Mean[1]
          }
      } else{
        means_M2[1] <-  ds.mean("M2_helper",
                                type = "combined",
                                datasources = datasources
        )$Global.Mean[1]
      }

      #TODO
      #this will definitely not work, I think.
      ds.sendToServer(means_M2, newobj = "M2", datasources = datasources)

      # Now the influence function related to estimation effect of pscores

      if (est_method == "reg") {
        ds.make("asy_lin_rep_wols %*% M2",
          newobj = "inf_cont_2",
          datasources = datasources
        )
      } else{
        ds.make("asy_lin_rep_ps %*% M2",
                newobj = "inf_cont_2",
                datasources = datasources
        )
      }



      # Estimation Effect from beta hat (weighted OLS)
      ds.make("odds * covariates_one",
        newobj = "M3_helper",
        datasources = datasources
      )

      means_M3 <- c()
      if (!is.null(xformla)){
        cols_covariates <- ds.colnames("M3_helper")[[1]]
        for (i in 1:length(cols_covariates)) {
          variable <- paste0("M3_helper$`", cols_covariates[i], "`")
          means_M3[i] <- ds.mean(variable,
            type = "combined",
            datasources = datasources
          )$Global.Mean[1]
        }
      } else{
        means_M3[1] <-  ds.mean("M3_helper",
                                type = "combined",
                                datasources = datasources
        )$Global.Mean[1]

      }

      #TODO
      #this will definitely not work, I think.
      ds.sendToServer(means_M3, newobj = "M3", datasources = datasources)

      # Now the influence function related to estimation effect of regressions
      ds.make("asy_lin_rep_wols %*% M3",
        newobj = "inf_cont_3",
        datasources = datasources
      )

      # Influence function for the control component
      # compute in1 + inf2 -> compute difference with function, compute mean, compute inf.control
      ds.make("inf_cont_1 + inf_cont_2",
        newobj = "inf_cont_helper",
        datasources = datasources
      )

      #TODO
      #ds.make?
      inf_control_difference <- ds.computeInfTreatDifference("inf_cont_helper", "inf_cont_3",
        datasources = datasources
      )
      odds_mean <- ds.mean("odds", type = "combined")$Global.Mean[1]

      inf_control <- unlist(inf_control_difference) / odds_mean

      # get the influence function of the DR estimator (put all pieces together)
      if (est_method == "dr"){
        dr_att_inf_func <- unlist(inf_treat) / mean_G - inf_control # divide by mean of G here because we cannot do it when inf_treat is created
      } else if(est_method %in%  c("ipw","reg")) {
        ds.make(paste0("inf_treat_1 / ", mean_G) , newobj = "inf_treat", datasources = datasources)
        ds.make(paste0("inf_cont_helper / ", odds_mean), newobj = "inf_control", datasources = datasources)


        dr_att_inf_func <- unlist(ds.computeInfTreatDifference("inf_treat", "inf_control",
                                                        datasources = datasources
                                                        ))
      }
      #-----------------------------------------------------------------------------
      #create empty matrix with all ids for influence
      n_columns <- length(t_periods) * length(g_periods)

      if (index_iteration == 1){
        ds.createEmptyIdMatrix("sorted_data", idname, n_columns,
                              newobj = "influence_matrix", datasources=datasources)
      }



      #send dr_att_inf_func and ids to servers loop over connections and ids_length and append to matrix
      last_index <- 0 #initialize index of individuals in dr_att_inf_func (they are ordered)

      for (i in 1:length(datasources)){
        first_index <- last_index + 1 #choose next index
        last_index <- last_index + length_ids[[i]]
        influence_source <- dr_att_inf_func[(first_index:last_index)] / n  #divide by (which is n1) in the paper

        #TODO recheck function
        ds.AppendInfluence("influence_matrix", influence_source, "ids_g_t",
                           index_iteration,
                           newobj = "influence_matrix",
                           datasources=datasources[[i]])

      }


      # Estimate of asymptotic standard error
      se_dr_att <- (t(dr_att_inf_func) %*% dr_att_inf_func / n)^0.5 / sqrt(n) #-> divide by 40 (total numbers of individuals)

      z_value <- qnorm(1-alpha/2)

      length_ci <- z_value * se_dr_att
      upper_bound <- dr_att + length_ci
      lower_bound <- dr_att - length_ci

      # result <- DSI::datashield.assign(datasources, "dp", cally)
      asymptotic <- c("Group" = g, "Time" = t,
        "ATT" = dr_att, "SE" = se_dr_att, "[95% Pointwise" = lower_bound,
        "Conf. Band]" = upper_bound
      )

      #if (g <= t){ #include only post-treatment periods
      out <- rbind(out, asymptotic)
      #}
    }#end t
  }#end g

  dimensions_influence <- ds.dim("influence_matrix", datasources=datasources)
  n_global <- dimensions_influence[[length(dimensions_influence)]][1]
  n_std_error <- n_global #is overwritten if clustered standard errors; otherwise we cluster at the individual level

  ds.make( paste0("influence_matrix *", n_global), newobj = "influence_matrix_adjusted",
           datasources = datasources) #adjust by multiplying with total amount (done in original package with n)

  name_influence_use <- "influence_matrix_adjusted"

  #bootstrap
  if (bstrap){

      if (!is.null(clustervars)){ #clusters are only possible within a server
        #---------this needs to be changed to allow for global clusters--------- -> ds.meanByClass only allows for 3 clusters
        #TODO check function
        ds.clusterInfluenceFunction("sorted_data", "influence_matrix_adjusted",
                                    clustervars,
                                    idname,
                                    newobj = "influence_matrix_adjusted_cluster",
                                    datasources = datasources)


        name_influence_use <- "influence_matrix_adjusted_cluster"
        dimensions_influence_clust <- ds.dim("influence_matrix_adjusted_cluster", datasources=datasources)
        n_std_error <- dimensions_influence_clust[[length(dimensions_influence_clust)]][1]

        #-----------------------------------------------------------------------
      }

      bootstrap_samples <- ds.multiplierBootstrap(name_influence_use, biters,
                                                   datasources=datasources) #everything still needs to be divided by number of cluster


      bootstrap_sample_combined <- Reduce('+', bootstrap_samples) / n_std_error

      bres <- sqrt(n_std_error) * bootstrap_sample_combined


      #from main package
      # for uniform confidence band
      # compute new critical value
      # see paper for details
      b_sigma <- apply(bres, 2,
                      function(b) (quantile(b, .75, type=1, na.rm = T) -
                                     quantile(b, .25, type=1, na.rm = T))/(qnorm(.75) - qnorm(.25)))
      se <- as.numeric(b_sigma) / sqrt(n_std_error)

      b_sigma[b_sigma <= sqrt(.Machine$double.eps)*10] <- NA

      if (cband) {
      # sup-t confidence band
        bT <- apply(bres, 1, function(b) max( abs(b/b_sigma), na.rm = TRUE))
        z_value <<- quantile(bT, 1-alpha, type=1, na.rm = T)
        if(z_value >= 7){
          warning("Simultaneous critical value is arguably `too large' to be realible. This usually happens when number of observations per group is small and/or there is no much variation in outcomes.")
        }
      }

      lower_bounds <- out[,"ATT"] - z_value * se
      upper_bounds <- out[,"ATT"] + z_value * se

      #overwrite asymptotic values
      out[,"SE"] <- se
      out[,"[95% Pointwise"] <- lower_bounds
      out[,"Conf. Band]"] <- upper_bounds

  }

  #-------------------Wald-Test for parallel trend assumption-------------------
  #TODO check function; as above
  V <- Reduce("+", ds.computeMatrixCrossproduct("influence_matrix_adjusted")) / n_global
  V1 <- V
  pre <- out[, "Group"] > out[, "Time"]
  pre_att <- as.matrix(out[pre, "ATT"])
  pre_V <- as.matrix(V[pre,pre])

  #-----------start from original package --------------------------------------
  # check if there are actually any pre-treatment periods
  if (length(pre_V) == 0) {
    message("No pre-treatment periods to test")
    W  <- NULL
    W_pval <- NULL
  } else if(sum(is.na(pre_V))) {
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
    W <- n_global*t(pre_att)%*%solve(pre_V)%*%pre_att
    q <- length(pre) # number of restrictions
    W_pval <- round(1-pchisq(W,q),5)
  }
  #---------End from original package-------------------------------------------


  return(out)
}
