
test_that("testing make_filename function",{ 
   expect_that(basename(make_filename(2014)),is_identical_to("accident_2014.csv.bz2"))
})