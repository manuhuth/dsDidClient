create_test_data <- function(seed, time.periods = 3) {
  set.seed(seed)
  sim_par <- did::reset.sim(time.periods = time.periods, n = 7000, ipw = TRUE, reg = TRUE)
  dta <- did::build_sim_dataset(sim_par)
  dta["X2"] <- dta["X"] + rnorm(nrow(dta), 0, 1)
  dta["X3"] <- dta["X"] + rnorm(nrow(dta), 0, 1)
  dta["X4"] <- dta["X"] + rnorm(nrow(dta), 0, 1)
  dta["Y"] <- 3 * dta["X"] - 1.5 * dta["X2"]
  dta <- dta[order(dta$id, dta$period), ]

  dta_0 <- dta[which(dta$treat == 0), ]
  dta_1 <- dta[which(dta$treat == 1), ]

  half_ids_0 <- floor(length(unlist(unique(dta_0["id"]))) / 2)
  cut_off_0 <- half_ids_0 * time.periods

  half_ids_1 <- floor(length(unlist(unique(dta_1["id"]))) / 2)
  cut_off_1 <- half_ids_1 * time.periods

  data_1_treat0 <- dta_0[1:cut_off_0, ]
  data_1_treat1 <- dta_1[1:cut_off_1, ]

  data_2_treat0 <- dta_0[(cut_off_0 + 1):(nrow(dta_0)), ]
  data_2_treat1 <- dta_1[(cut_off_1 + 1):(nrow(dta_1)), ]

  data_server_1 <- rbind(data_1_treat0, data_1_treat1)
  data_server_2 <- rbind(data_2_treat0, data_2_treat1)

  # create cluster variable
  data_server_1["clust"] <- NaN
  for (i in 1:nrow(data_server_1)) {
    data_server_1[i, "clust"] <- data_server_1[i, "id"] %% 7
  }

  data_server_2["clust"] <- NaN
  for (i in 1:nrow(data_server_2)) {
    data_server_2[i, "clust"] <- data_server_2[i, "id"] %% 7 + 7
  }

  dslite.server <<- DSLite::newDSLiteServer(
    tables = list(
      data_server_1 = data_server_1,
      data_server_2 = data_server_2
    ),
    config = DSLite::defaultDSConfiguration(include = c(
      "dsBase",
      "dsDid"
    ))
  )

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "server1", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server2", url = "dslite.server", driver = "DSLiteDriver")

  logindata.dslite <- builder$build()

  # Login to the virtualized server
  connections <<- DSI::datashield.login(logindata.dslite, assign = T, symbol = "D")

  DSI::datashield.assign.table(
    conns = connections, symbol = "D",
    table = list(server1 = "data_server_1", server2 = "data_server_2")
  )

  return(list("datasource" = connections, "data" = rbind(data_server_1, data_server_2)))
}
