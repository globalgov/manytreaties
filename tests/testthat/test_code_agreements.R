
# Test numbers assigned to procotol/amendment
data5 <- data.frame(title = c("Amendments On The Transport Of Corrosive Substances To Protocol 18 Of The 1868 Revised Convention On The Navigation Of The Rhine",
                              "Amendments 34 Of The Limitation Amounts In The 1992 Convention",
                              "Amendments Of The Limitation Amounts In The 1992 Convention (Annex 4)"),
                    date = c("1899-10-02", "2000-10-18", "2010-10-10"))

test_that("code_agreements() identify correct number of protocol or amendment", {
  expect_equal(code_agreements(data5, data5$title, data5$date),
               c("TRCSNR_1899E18", "LMTTNA_2000E34", "LMTTNA_2010E4"))
})

# Test that some functions return coding information when argument is missing
test_that("certain functions return coding information when argument is missing", {
  expect_type(code_known_agreements(), "character")
})

# Test that the punctuation marks are not in the treatyID
test_that("Punctation marks are not in the treatyID", {
  treatyID <- code_agreements(data5, data5$title, data5$date)
  expect_false(any(grepl("\\(|\\)", treatyID)))
})


# Test that linkages are added correctly for bilaterals treaties
# data9 <- data.frame(title = c("Supplementary Protocol To The Treaty Relating To The Utilization Of The Waters Of The Colorado And Tijuana Rivers And Of The Rio Grande (Rio Bravo) From Fort Quitman Texas To The Gulf Of Mexico",
#                              "Treaty Relating To The Utilization Of The Waters Of The Colorado And Tijuana Rivers And Of The Rio Grande (Rio Bravo) From Fort Quitman Texas To The Gulf Of Mexico"),
#                     date = c("1944-11-14", "1944-11-14"))
# treatyID <- c("MEX-TEX[FQG]_1944P:MEX-TEX[FQG]_1944A", "MEX-TEX[FQG]_1944A")
# cd <- code_agreements(title = data9$title, date = data9$date)
# 
# test_that("linkages are added correctly for bilaterals treaties", {
#   expect_equal(cd, treatyID)
# })
