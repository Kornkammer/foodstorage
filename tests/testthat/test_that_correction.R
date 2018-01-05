# data("kornumsatz_demo", package = "foodstorage")
# data("starting_csv", package = "foodstorage")
# kornumsatz <<- foodstorage::startup.settings(get("kornumsatz_demo"), get("starting_csv"))
# products <- levels(kornumsatz$Produkt)
# 
# for (i in 1:length(products)) {
#   sub.df <- prepare(products[i], test_correction = T)
#   
#   test_that("candidates and candidates_eval have same number of rows", {
#     expect_equal(correction(sub.df, sub.df$VPE[1], test_equal_candidates = T), 
#                  0)
#   })
#   
#   test_that("assume that there is only one row consisting of NA", {
#     candidates <- correction(sub.df, sub.df$VPE[1], test_candidates = T)
#     candidates_eval <- sub.df[which(sub.df$Position %in% candidates$Position)+1, ]
#     
#     # test 1: always the last candidate produces NA
#     if (TRUE %in% is.na(candidates_eval$Position)) {
#       expect_equal(max(sub.df$Position), max(candidates$Position))
#     }
#     
#     candidates_eval <- candidates_eval[which(!is.na(candidates_eval$Position)),]
#     # test 2: maximum one row with NA exsists
#     ndif <- nrow(candidates)-nrow(candidates_eval)
#     expect_lt(ndif, 2)
#   })
#   
# }