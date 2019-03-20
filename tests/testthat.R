library(testthat)
library(koboloops)

test_check("koboloops")

a <- data.frame( uuid=1:10, age=runif(10,5,30), d=runif(10))
b <- data.frame (parent_uuid=sample(1:10,20,replace = T), e=sample(c("A","B","C"),20,replace = T))

a1 <- data.frame( uuid=1:10, age=runif(10,5,30), d=runif(10))
b1 <- data.frame (parent_uuid=sample(11:20,20,replace = T), e=sample(c("A","B","C"),20,replace = T))

a2 <- data.frame( index=1:10, age=runif(10,5,30), d=runif(10))


aggregate.function <- function(x, variable.to.add){
  result_aggregation <- sum(x[[variable.to.add]])
  return(result_aggregation)
}


library(testthat)

test_that("colums that doesn't exist are found", {
  expect_error(add_parent_to_loop(b, a , c("aaa")))
  expect_error(add_parent_to_loop(b, a , c("aaa","abc")))
  expect_error(add_parent_to_loop(b, a , c("age","abc")))
})


test_that("uuid is not specified or doesn't exist", {
  expect_error(add_parent_to_loop(b, a ,c("age"), "uuid" , "uuid" ))
  expect_error(add_parent_to_loop(b, a2 , c("age")))
})


test_that("parent and loop don't have common index", {
  expect_error(add_parent_to_loop(b1,a1,"age"))
})


test_that("test on optional parameters", {
  expect_identical( names(add_parent_to_loop(b,a)) , c("parent_uuid","e","age","d") )
  expect_equal( nrow(add_parent_to_loop(b,a)) , 20)
  expect_identical(add_parent_to_loop(b,a)[ ,"parent_uuid"], b[ ,"parent_uuid"])
  expect_identical(add_parent_to_loop(b,a)[ ,"e"], b[ ,"e"])
  expect_identical(names( add_parent_to_loop(b,a, c("age"), "parent_uuid","uuid")) , names(add_parent_to_loop(b,a, c("age")) ))
})


test_that("positive test add_parent_to_loop", {
  expect_equal( nrow(add_parent_to_loop(b,a, c("age"), "parent_uuid","uuid")) , 20 )
  expect_identical( names(add_parent_to_loop(b,a, c("age"), "parent_uuid","uuid")) , c("parent_uuid","e","age"))
  expect_identical( add_parent_to_loop(b,a)[,"parent_uuid"] , b[,"parent_uuid"])
  expect_identical( add_parent_to_loop(b,a)[,"e"] , b[,"e"])
})


a <- data.frame( uuid=1:10, age=runif(10,5,30), d=runif(10))
b <- data.frame (parent_uuid=sample(1:8,20,replace = T), e=sample(letters,20,replace = T), index=1:20, age=sample(1:19, 20, replace = T))

a1 <- data.frame( uuid=1:10, age=runif(10,5,30), d=runif(10))
b1 <- data.frame (parent_uuid=sample(11:20,20,replace = T), index=1:20, e=sample(c("A","B","C"),20,replace = T))

a2 <- data.frame( index=1:10, age=runif(10,5,30), d=runif(10))

test_that("colums that doesn't exist are found", {
  expect_error(affect_loop_to_parent(b, a , aggregate.function, c("aaa")))
  expect_error(affect_loop_to_parent(b, a , aggregate.function, c("e","abc")))
  expect_error(affect_loop_to_parent(b, a , aggregate.function, c("abc","bbb"),aggregate.function))
})


test_that("uuid is not specified or doesn't exist", {
  expect_error(affect_loop_to_parent(b, a ,aggregate.function,c("age"),uuid.name.loop =  "uuid" , "uuid" ))
  expect_error(affect_loop_to_parent(b, a2 ,aggregate.function, c("age")))
})


test_that("parent and loop don't have common index", {
  expect_error(affect_loop_to_parent(b1,a1,aggregate.function, "index"))
})


test_that("positive test affect_loop_to_parent", {
  expect_equal( nrow(affect_loop_to_parent(b, a ,aggregate.function, c("index"))) , 10 )
  expect_identical( names(affect_loop_to_parent(b, a ,aggregate.function, c("index"))) , c("uuid","age","d","index"))
  expect_identical( affect_loop_to_parent(b, a ,aggregate.function, c("index"))[,"uuid"] , a[,"uuid"])
})
