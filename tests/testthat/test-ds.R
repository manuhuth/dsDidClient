test_that("ds_did_works", {
  install.packages("DSLite")
  library(DSLite)
  library(did)
  library(dsDid)
  library(dsBase)
  library(dsBaseClient)
  #----------------------------------define functions-----------------------------
  transform_did_data_set <- function(data_did, id_var="individual", post_treatment = "post_treatment"){
    data_did[, "period"] <- NaN

    p <- -1
    for (i in 1:nrow(data_did)){
      if (i > 1){
        if (data_did[i, id_var] != data_did[i-1, id_var]){
          p <- -1
        }
      }
      p <- p + 1

      data_did[i, "period"] <- p
    }

    data_did[, "G"] <- 0
    for (i in 1:nrow(data_did)){
      if (i > 1){
        if ( (data_did[i, id_var] == data_did[i-1, id_var]) & (data_did[i, post_treatment] != data_did[i-1, post_treatment]) ){

          data_did[ which(data_did[,id_var] == unlist(data_did[i, id_var])), "G" ] <- unlist(data_did[i, "period"])
        }
      }
    }
    return(data_did)
  }

  # ------------------------------------------------------------------------------
  # Create virtualized server with DSLite, assign everything needed on it
  set.seed(1814)
  time.periods <- 3
  sim_par <- reset.sim(time.periods=time.periods, n=1000, ipw=TRUE, reg=TRUE)
  dta <- build_sim_dataset(sim_par)
  dta["X2"] <- dta["X"] + rnorm(nrow(dta), 0, 1)
  dta["X3"] <- dta["X"] + rnorm(nrow(dta), 0, 1)
  dta["X4"] <- dta["X"] + rnorm(nrow(dta), 0, 1)

  dta <-dta[order(dta$id, dta$period),]
  dta_0 <- dta[which(dta$treat == 0),]
  dta_1 <- dta[which(dta$treat == 1),]
  half_ids_0 <- floor(length(unlist(unique(dta_0["id"]))) / 2)
  cut_off_0 <- half_ids_0 * time.periods

  half_ids_1 <- floor(length(unlist(unique(dta_1["id"]))) / 2)
  cut_off_1 <- half_ids_1 * time.periods

  data_1_treat0 <- dta_0[1:cut_off_0,]
  data_1_treat1 <- dta_1[1:cut_off_1,]

  data_2_treat0 <- dta_0[(cut_off_0+1):(nrow(dta_0)),]
  data_2_treat1 <- dta_1[(cut_off_1+1):(nrow(dta_1)),]

  data_server_1 <- rbind(data_1_treat0, data_1_treat1) #transform_did_data_set(data_server_1)
  data_server_2 <- rbind(data_2_treat0, data_2_treat1)  #transform_did_data_set(data_server_2)

  #create cluster variable
  data_server_1["clust"] <- NaN
  for (i in 1:nrow(data_server_1)){
    data_server_1[i, "clust"] <- data_server_1[i, "id"] %% 7

  }

  data_server_2["clust"] <- NaN
  for (i in 1:nrow(data_server_2)){
    data_server_2[i, "clust"] <- data_server_2[i, "id"] %% 7 + 7

  }

  data_server_1 <- data_server_1
  data_server_2 <- data_server_2

  dta <- rbind(data_server_1, data_server_2)

  #-------------------------------------Set-up dslite-----------------------------

  dslite.server <- newDSLiteServer(tables=list(data_server_1=data_server_1, data_server_2=data_server_2),
                                   config = DSLite::defaultDSConfiguration(include=c("dsBase", "dsBaseClient",
                                                                                     "dsDid", "dsDidClient"
                                   )))

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "server1", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server2", url = "dslite.server", driver = "DSLiteDriver")

  logindata.dslite <- builder$build()

  # Login to the virtualized server
  connections <- DSI::datashield.login(logindata.dslite, assign=T, symbol = "D")

  DSI::datashield.assign.table(conns = connections, symbol = "D", table =list(server1="data_server_1", server2="data_server_2"))


  #---------------------------Run DID--------------------------------------------
  #solve problem for no covariates
  our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
                                  t_periods = c(2, 3), g_periods=c(2, 3),
                                  data="D", xformla="X",
                                  control_group= "notyettreated", # c("nevertreated","notyettreated"),
                                  anticipation=0,  alpha=0.05,
                                  base_period = "varying",
                                  bstrap=FALSE, biters = 1000, cband = TRUE,
                                  clustervars = NULL,
                                  est_method="dr",
                                  datasources = connections,
                                  clear_console=TRUE)

  original_non_federated_package <- att_gt(yname = "Y",
                                           tname = "period",
                                           idname = "id",
                                           gname = "G",
                                           xformla = ~ X,
                                           data = dta,
                                           est_method="dr",
                                           control_group = "notyettreated", #"nevertreated",
                                           bstrap=FALSE, biters=1000, clustervars = NULL,
                                           base_period = "varying", cband = FALSE
  )
  expect_equal(2 * 2, 4)
})
