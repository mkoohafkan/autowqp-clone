test_that("forms processing works", {
  skip_if_no_db("production")

  test.dir = tempdir()
  file.copy("sample-data/sharepoint-mock", test.dir, recursive = TRUE)

  expect_type(
    auto_forms(
      local.path = test.dir,
      forms.dir = "sharepoint-mock",
      workspace = test.dir,
      pattern = ".xlsx$",
      program = "marsh",
      database = "production",
      token = NULL,
      email.template = "sample-data/templates-mock/forms-template.yaml"
    ),
    "character"
  )
})

test_that("QAQC processing works", {
  skip_if_no_db("production")

  test.dir = tempdir()

  expect_type(
    auto_qaqc(
      workspace = test.dir,
      cdec.code = c("CSE", "TEA"),
      analyte.name = c("Specific Conductance", "Temperature"),
      from = Sys.Date() - 4,
      to = Sys.Date() - 1,
      program = "marsh",
      database = "production",
      email.template = "sample-data/templates-mock/qaqc-template.yaml"
    ),
    "character"
  )
})

test_that("Action processing works", {
  skip_if_no_db("production")

  test.dir = tempdir()

  expect_type(
    auto_actions(
      workspace = test.dir,
      actions = c("Removed", "Installed"),
      from = Sys.Date() - 40,
      to = Sys.Date() - 1,
      program = "marsh",
      database = "production",
      email.template = "sample-data/templates-mock/actions-template.yaml"
    ),
    "character"
  )
})
