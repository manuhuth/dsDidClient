test_that("ds_test_basic_set_up_check", {

   data_object <- create_test_data(seed=12345)
   connections <- connections
   data <- data_object$datasources

   test_data <- data_object$data

   mean_X <- dsBaseClient::ds.mean(x="D$X", type = 'combine', datasources = connections)
   mean_X <- as.numeric(mean_X$Global.Mean[1])

   expect_equal(mean_X, mean(test_data$X))
})

test_that("ds_run_doubly_robust", {

   data_object <- create_test_data(seed=1235)
   connections <- connections
   data <- data_object$datasources

   our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
                                  t_periods = c(2, 3), g_periods=c(2, 3),
                                 data="D", #xformla="X",
                                   control_group= "notyettreated",
                                   anticipation=0,  alpha=0.05,
                                   base_period = "varying",
                                   bstrap=FALSE, biters = 1000, cband = TRUE,
                                   clustervars = NULL,
                                   est_method="dr",
                                   datasources = connections,
                                   clear_console=TRUE)



   expect_no_error(our_federated_package)

})

test_that("ds_run_ipw", {

  data_object <- create_test_data(seed=1235)
  connections <- connections
  data <- data_object$datasources

  our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
                                  t_periods = c(2, 3), g_periods=c(2, 3),
                                  data="D", #xformla="X",
                                  control_group= "nevertreated",
                                  anticipation=0,  alpha=0.05,
                                  base_period = "varying",
                                  bstrap=FALSE, biters = 1000, cband = TRUE,
                                  clustervars = NULL,
                                  est_method="ipw",
                                  datasources = connections,
                                  clear_console=TRUE)



  expect_no_error(our_federated_package)

})

test_that("ds_run_doubly_robust_nevertreated", {

  data_object <- create_test_data(seed=1235)
  connections <- connections
  data <- data_object$datasources

  our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
                                  t_periods = c(2, 3), g_periods=c(2, 3),
                                  data="D", #xformla="X",
                                  control_group= "nevertreated",
                                  anticipation=0,  alpha=0.05,
                                  base_period = "varying",
                                  bstrap=FALSE, biters = 1000, cband = TRUE,
                                  clustervars = NULL,
                                  est_method="reg",
                                  datasources = connections,
                                  clear_console=TRUE)



  expect_no_error(our_federated_package)

})

test_that("ds_run_doubly_robust_covs", {

  data_object <- create_test_data(seed=1235)
  connections <- connections
  data <- data_object$datasources

  #expect warning is necessary since ds.glm throws warning at every step
  #(calls glm for every step)
  #of the gradient descent; but actually the algoprithms converge after the 3rd step
  #thiss needs to be fixed in the base package
  suppressWarnings(our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
                                  t_periods = c(2, 3), g_periods=c(2, 3),
                                  data="D", xformla="X2",
                                  control_group= "notyettreated",
                                  anticipation=0,  alpha=0.05,
                                  base_period = "universal",
                                  bstrap=TRUE, biters = 30, cband = TRUE,
                                  clustervars = NULL,
                                  est_method="dr", maxit =1000,
                                  datasources = connections,
                                  clear_console=TRUE))


  testthat::expect_no_error(our_federated_package)

})
