#### check_icd_format ####
test_that("gives decimal warnings and does correction", {
  short_chr <- c("01234", "A000")
  short_fct <- as.factor(short_chr)
  decml_chr <- c("012.34", "A00.0")
  decml_fct <- as.factor(decml_chr)

  check_icd_format(decml_chr) %>%
    expect_equal(short_chr) %>%
    expect_warning(regexp = "decimal")

  check_icd_format(decml_fct) %>%
    expect_equal(short_chr) %>%
    expect_warning("is a factor") %>%
    expect_warning("decimal")

  c("K39.0", NA) %>%
    check_icd_format() %>%
    expect_warning("decimal")
})

test_that("gives factor warnings and does correction", {
  short_chr <- c("01234", "A000")
  short_fct <- as.factor(short_chr)
  decml_chr <- c("012.34", "A00.0")
  decml_fct <- as.factor(decml_chr)
  missn_chr <- c("K390", NA, "xxxx")
  missn_fct <- as.factor(missn_chr)

  check_icd_format(short_fct) %>%
    expect_equal(short_chr) %>%
    expect_warning("is a factor")

  check_icd_format(decml_fct) %>%
    expect_equal(short_chr) %>%
    expect_warning("is a factor") %>%
    expect_warning("decimal")

  check_icd_format(missn_fct) %>%
    expect_equal(missn_chr) %>%
    expect_warning("is a factor")


})

test_that("returns char vector of same length", {

  expect_char_of_n <- function(codes){
    # Should be same length
    expect_length(check_icd_format(codes), length(codes))
    # Should return character
    expect_true(rlang::is_character(check_icd_format(codes)))
  }

  decml_chr <- c("ABC.123", "ABC.123")
  decml_fct <- as.factor(decml_chr)
  short_chr <- c("01234", "A000")
  short_fct <- as.factor(short_chr)
  missn_chr <- c("K390", NA, "xxxx")
  short_num <- c(0013, 4220)

  expect_char_of_n(decml_chr) %>% suppressWarnings()
  expect_char_of_n(decml_fct) %>% suppressWarnings()
  expect_char_of_n(short_chr) %>% suppressWarnings()
  expect_char_of_n(short_fct) %>% suppressWarnings()
  expect_char_of_n(missn_chr) %>% suppressWarnings()
  # expect_char_of_n(short_num)
})

test_that("fails with bad data types", {
  bad_lgl  <- c(T, F, F)
  bad_num  <- c(1.2, 2.3, 5.0)
  bad_int  <- c(1L, 2L, 3L)
  bad_lst  <- list("Dog", "Cat", 8)
  bad_NAl  <- c(NA, NA)
  bad_NAc  <- as.character(bad_NAl)
  bad_NULL <- NULL
  bad_df   <- data.frame(x = 1,
                         y = 1:10,
                         fac = base::sample(LETTERS[1:3], 10, replace = TRUE))

  check_icd_format(bad_lgl) %>% expect_error()
  check_icd_format(bad_num) %>% expect_error()
  check_icd_format(bad_int) %>% expect_error()
  check_icd_format(bad_lst) %>% expect_error()
  check_icd_format(bad_NAl) %>% expect_error()
  # check_icd_format(bad_NAc) # should still work
  check_icd_format(bad_NULL) %>% expect_error()

})
