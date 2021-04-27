test_that("QAQC checking works", {
  skip_if_no_db("production")

  expect_s3_class(
    check_qaqc(
      cdec.code = c("CSE", "TEA"),
      analyte.name = c("Specific Conductance", "Temperature"),
      from = Sys.Date() - 4,
      to = Sys.Date() - 1,
      program = "marsh",
      database = "production"
    ),
    "tbl_df"
  )
})

test_that("Action checking works", {
  skip_if_no_db("production")

  expect_s3_class(
    check_actions(
      actions = c("Removed", "Installed"),
      from = Sys.Date() - 60,
      to = Sys.Date(),
      group.by = "station",
      program = "marsh",
      database = "production"
    ),
    "tbl_df"
  )

  expect_s3_class(
    check_actions(
      actions = c("Removed", "Installed"),
      from = Sys.Date() - 60,
      to = Sys.Date(),
      group.by = "sonde",
      program = "marsh",
      database = "production"
    ),
    "tbl_df"
  )
})
