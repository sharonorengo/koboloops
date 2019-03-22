context("testthat.R")
library(testthat)
library(koboloops)

devtools::test()

parent_data1 <- data.frame( uuid=1:10, age_parent=runif(10,5,30), other=runif(10))
loop_data1 <- data.frame (parent_uuid=sample(1:8,20,replace = T), initial=sample(letters,20,replace = T), index=1:20, age_child=sample(1:18, 20, replace = T))

#no uuid column
parent_data2 <- data.frame(index=1:10, age_parent=runif(10,5,30), other=runif(10))

#uuid are not the same as for the parent uuids
loop_data2 <- data.frame(parent_uuid=sample(11:20,20,replace = T), initial=sample(c("A","B","C"),20,replace = T),index=1:10)


sum_index <- function(x, variable.to.add){
  result_aggregation <- sum(x[[variable.to.add]])
  return(result_aggregation)
}


test_that("colums that doesn't exist are found", {
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("others")))
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("ages","other")))
  expect_error(add_parent_to_loop(loop_data1, parent_data1 , c("Age_parent","Other")))
})


test_that("uuid is not specified or doesn't exist", {
  expect_error(add_parent_to_loop(loop_data1, parent_data1 ,c("age_parent"), "uuid" , "uuid" ))
  expect_error(add_parent_to_loop(loop_data1, parent_data2 , c("age_parent")))
})


test_that("parent and loop don't have common index", {
  expect_error(add_parent_to_loop(loop_data2,parent_data1,"age_parent"))
})


test_that("test on optional parameters", {
  expect_identical( names(add_parent_to_loop(loop_data1,parent_data1)) , c("parent_uuid","initial","index","age_child","age_parent","other"))
  expect_equal( nrow(add_parent_to_loop(loop_data1,parent_data1)) , 20)
  expect_identical(add_parent_to_loop(loop_data1,parent_data1)[ ,"parent_uuid"], loop_data1[ ,"parent_uuid"])
  expect_identical(add_parent_to_loop(loop_data1,parent_data1)[ ,"initial"], loop_data1[ ,"initial"])
  expect_identical(names( add_parent_to_loop(loop_data1,parent_data1, c("age_parent"), "parent_uuid","uuid")) , names(add_parent_to_loop(loop_data1,parent_data1, c("age_parent")) ))
})


test_that("positive test add_parent_to_loop", {
  expect_equal( nrow(add_parent_to_loop(loop_data1,parent_data1, c("age_parent"), "parent_uuid","uuid")) , 20 )
  expect_identical( names(add_parent_to_loop(loop_data1,parent_data1, c("age_parent"), "parent_uuid","uuid")) , c("parent_uuid","initial","index","age_child","age_parent"))
  expect_identical( add_parent_to_loop(loop_data1,parent_data1)[,"parent_uuid"] , loop_data1[,"parent_uuid"])
  expect_identical( add_parent_to_loop(loop_data1,parent_data1)[,"initial"] , loop_data1[,"initial"])
})




test_that("colums that doesn't exist are found", {
  expect_error(affect_loop_to_parent(loop_data1, parent_data1 , sum_index, c("Index")))
  expect_error(affect_loop_to_parent(loop_data1, parent_data1 , sum_index, c("index","Age")))
  expect_error(affect_loop_to_parent(loop_data1, parent_data1 , sum_index, c("Index","ages")))
})


test_that("uuid is not specified or doesn't exist", {
  expect_error(affect_loop_to_parent(loop_data1, parent_data1 ,sum_index,c("age_child"),uuid.name.loop =  "uuid" , "uuid" ))
  expect_error(affect_loop_to_parent(loop_data1, parent_data2 ,sum_index, c("age_child")))
})


test_that("parent and loop don't have common index", {
  expect_error(affect_loop_to_parent(loop_data2,parent_data1,sum_index, "index"))
})


test_that("positive test affect_loop_to_parent", {
  expect_equal( nrow(affect_loop_to_parent(loop_data1, parent_data1 ,sum_index, c("index"))) , 10 )
  expect_identical( names(affect_loop_to_parent(loop_data1, parent_data1 ,sum_index, c("index"))) , c("uuid","age_parent","other","index"))
  expect_identical( affect_loop_to_parent(loop_data1, parent_data1 ,sum_index, c("index"))[,"uuid"] , parent_data1[,"uuid"])
})


