# # test equalise
# test_korn1 <- data.frame(
#   Produkt = factor(c("Buchweizen", "Dinkel", "Hafer", "Weizen"))
# )
# test_start <- data.frame(
#   Produkt = factor(c("Buchweizen", "Dinkel", "Hafer", "Weizen"))
# )
# test_korn2 <- data.frame(
#   Produkt = factor(c("Buchweizen", "Cashews", "Dinkel", "Hafer", "Weizen"))
# )
# 
# 
# test_that(
#   "all products are known",
#   {
#     len <- length(equalise(data = test_korn1, startingCSV = test_start))
#     expect_equal(len, 0)
#   }
# )
# 
# test_that(
#   "all products are known",
#   {
#     len <- length(equalise(data = test_korn1, startingCSV = test_start))
#     expect_equal(len, 0)
#   }
# )