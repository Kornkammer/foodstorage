# # test currentStroage
# Bode <- c("Basilikum", "Basitom", "Basmati Weiß", "Blaumohn", "Buchweizenmehl", "Couscous", "Currychini", "Kichererbsen", "Kräuter der Provence", "Mepfel", "Oregano", "Paprika Edelsüß", "Risottoreis", "Rosmarin", "Rote Beete Meerettich", "Linsen Rot", "Schwarzkümmel", "Sendi", "Thymian", "Zwiebelschmelz")
# 
# test_that(
#   "data.frames have same number of rows before and after separating",
#   {
#     row.bigdf <- nrow(currentStorage(Bode, rawlist = T))
#     row.sep <- currentStorage(Bode, test_separating = T)
#     
#     expect_equal(row.sep, row.bigdf)
#   }
# )