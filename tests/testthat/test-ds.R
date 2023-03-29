test_that("ds_did_works", {
  expect_equal(2 * 3, 6)
})


test_that("ds_test_basic_set_up_check", {

   data_object <- create_test_data(seed=12345)
   connections <- connections
   data <- data_object$datasources

   test_data <- data_object$data

   mean_X <- dsBaseClient::ds.mean(x="D$X", type = 'combine', datasources = connections)
   mean_X <- as.numeric(mean_X$Global.Mean[1])
   dsBaseClient::ds.asMatrix("D$X", datasources = connections)
   expect_equal(mean_X, mean(test_data$X))
})

#test_that("ds_run_doubly_robust", {

#   data_object <- create_test_data(seed=12345)
#   connections <- connections
#   data <- data_object$datasources

#   our_federated_package <- ds.did(yname="Y", tname="period", idname="id", gname="G",
 #                                  t_periods = c(2, 3), g_periods=c(2, 3),
  #                                 data="D", xformla = NULL,#xformla="X",
   #                                control_group= "notyettreated",
    #                               anticipation=0,  alpha=0.05,
     #                              base_period = "varying",
      #                             bstrap=FALSE, biters = 1000, cband = TRUE,
       #                            clustervars = NULL,
        #                           est_method="dr",
         #                          datasources = connections,
          #                         clear_console=TRUE)

#   expect_no_error(our_federated_package)

 #})
