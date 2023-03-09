load(file = system.file("test_data",
                        "data_test.RData",
                        package = "codama"))

test_that("check_specie_catch_ocean() value without inconsistency for a trip", {
  expect_equal(
    sum(!check_specie_catch_ocean(
      data_connection = list(specie_ocean_data, specie_catch_ocean_data), output = "logical"
    )),
    10
  )
})
