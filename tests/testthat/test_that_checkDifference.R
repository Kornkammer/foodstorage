test_that(
  "checkDifference works", {
    ##### create test data
    productsExist <- tibble(
      Produkte_App = c("Buchweizen", "Weizen", "Roggen"),
      Produkt = c("Buchweizen", "Weizen", "Roggen")
    )
    productsDoesntExist <- tibble(
      Produkt = c("Kakao", "Bananen", "Mango"),
      Produkte_App = c("Kakao", "Bananen", "Mango")
    )
    productsAll <- tibble(
      Produkt = c("Buchweizen", "Weizen", "Roggen", "Kakao", "Bananen", "Mango")
    )
    
    expect_equal(
      checkDifference(productsAll, productsExist),
      productsDoesntExist$Produkt
    )
    expect_equal(
      checkDifference(productsExist, productsExist),
      character(0)
    )
    expect_equal(
      checkDifference(productsDoesntExist, productsExist),
      productsDoesntExist$Produkt
    )
    expect_equal(
      checkDifference(productsExist, productsDoesntExist),
      productsExist$Produkt
    )
  }
)