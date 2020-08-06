Brennan.3.1 <- read.delim("Brennan_tab3.1.txt")
set.seed(123)
test_boot <- CalcGTheoryCI(Brennan.3.1, 1000, type = “pi”)
summaryCI(test_boot, 0.80, 4)
