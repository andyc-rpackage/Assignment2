


test_that("testing class output fars_read",{
   fn<-make_filename(2013)
   df<-fars_read(fn)
   expect_that(df,is_a("tbl_df"))
})
