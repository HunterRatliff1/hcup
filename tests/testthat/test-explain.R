test_that("explain_ccsr handles NA & NULL", {
  CCSR_PR <- c("URN001", NA, "CAR004")

  expect_true(is.na(explain_ccsr(NA)))
  expect_false(is.na(explain_ccsr("CAR004")))
  expect_equal(explain_ccsr(CCSR_PR)[3], "Percutaneous coronary interventions (PCI)")
  expect_equal(explain_ccsr(CCSR_PR)[2], as.character(NA))


  bad_category <- c("URN001", "BAD CATEGORY", "CAR004")

  expect_warning(explain_ccsr("Bad category"))
  expect_null(explain_ccsr("Bad category"))
  expect_equal(explain_ccsr(bad_category)[2], as.character(NA))

})
