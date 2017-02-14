df <- maml.mapInputPort(1)


# import data
# df <- read.csv("LoansTrainingSetV2.csv", header = TRUE, stringsAsFactors = FALSE)


# rename columns
columnnames <-
  c(
    "LoanID",
    "CustomerID",
    "LoanStatus",
    "CurrentLoanAmount",
    "Term",
    "CreditScore",
    "YearsInCurrentJob",
    "HomeOwnership",
    "AnnualIncome",
    "Purpose",
    "MonthlyDebt",
    "YearsOfCreditHistory",
    "MonthsSinceLastDelinquent",
    "NumberOfOpenAccounts",
    "NumberOfCreditProblems",
    "CurrentCreditBalance",
    "MaximumOpenCredit",
    "Bankruptcies",
    "TaxLiens"
  )
colnames(df) <- columnnames
rm(columnnames)


# remove for web service
# df <- df[order(df$LoanID, df$CurrentLoanAmount),]
# df <- df[!duplicated(df$LoanID),]


# LoanID
# nothing to do...


# CustomerID
# nothing to do...


# LoanStatus
df$LoanStatus <- as.factor(df$LoanStatus)


# CurrentLoanAmount
df$CurrentLoanAmount[df$CurrentLoanAmount == 99999999] <- 0
df$CurrentLoanAmount <- as.numeric(ifelse(is.na(df$CurrentLoanAmount), 0, df$CurrentLoanAmount))


# Term
df$Term <- as.factor(df$Term)


# CreditScore
df$CreditScore <-
  ifelse(
    is.na(df$CreditScore) == FALSE &
      df$CreditScore >= 1000,
    df$CreditScore / 10,
    df$CreditScore
  )
df$CreditScore <- as.integer(ifelse(is.na(df$CreditScore), 0, df$CreditScore))


# YearsInCurrentJob
df$YearsInCurrentJob <- ifelse(df$YearsInCurrentJob == "< 1 year", "-1", df$YearsInCurrentJob)
df$YearsInCurrentJob <- ifelse(df$YearsInCurrentJob == "n/a", "NA", df$YearsInCurrentJob)
df$YearsInCurrentJob <- as.integer(gsub("([0-9]*).*", "\\1", df$YearsInCurrentJob))
df$YearsInCurrentJob <- ifelse(is.na(df$YearsInCurrentJob), 0, df$YearsInCurrentJob)


# HomeOwnership
df$HomeOwnership[df$HomeOwnership == "Home Mortgage"] <- "HomeMortgage"
df$HomeOwnership[df$HomeOwnership == "HaveMortgage"] <- "HomeMortgage"
df$HomeOwnership[df$HomeOwnership == "Own Home"] <- "Own"

df$HomeOwnership <- as.factor(df$HomeOwnership)


# AnnualIncome
df$AnnualIncome <- as.integer(df$AnnualIncome)
df$AnnualIncome <- ifelse(is.na(df$AnnualIncome), 0, df$AnnualIncome)


# Purpose
df$Purpose[df$Purpose == "Buy a Car"] <- "BuyCarHouse"
df$Purpose[df$Purpose == "Buy House"] <- "BuyCarHouse"
df$Purpose[df$Purpose == "Business Loan"] <- "BusinessLoan"
df$Purpose[df$Purpose == "small_business"] <- "BusinessLoan"
df$Purpose[df$Purpose == "Debt Consolidation"] <- "DebtConsolidation"
df$Purpose[df$Purpose == "Educational Expenses"] <- "Misc"
df$Purpose[df$Purpose == "Home Improvements"] <- "Misc"
df$Purpose[df$Purpose == "major_purchase"] <- "Misc"
df$Purpose[df$Purpose == "Medical Bills"] <- "Misc"
df$Purpose[df$Purpose == "moving"] <- "Misc"
df$Purpose[df$Purpose == "Other"] <- "Misc"
df$Purpose[df$Purpose == "other"] <- "Misc"
df$Purpose[df$Purpose == "renewable_energy"] <- "Misc"
df$Purpose[df$Purpose == "Take a Trip"] <- "Misc"
df$Purpose[df$Purpose == "wedding"] <- "Misc"
df$Purpose[df$Purpose == "vacation"] <- "Misc"

df$Purpose <- as.factor(df$Purpose)


# MonthlyDebt
df$MonthlyDebt <- sub("\\$", "", as.character(df$MonthlyDebt))
df$MonthlyDebt <- sub(",", "", as.character(df$MonthlyDebt))
df$MonthlyDebt <- as.numeric(ifelse(is.na(df$MonthlyDebt), 0, df$MonthlyDebt))


# YearsOfCreditHistory
df$YearsOfCreditHistory <- as.numeric(ifelse(is.na(df$YearsOfCreditHistory), 0, df$YearsOfCreditHistory))


# MonthsSinceLastDelinquent
df$MonthsSinceLastDelinquent <- as.integer(ifelse(is.na(df$MonthsSinceLastDelinquent), 0, df$MonthsSinceLastDelinquent))


# NumberOfOpenAccounts
df$NumberOfOpenAccounts <- as.integer(ifelse(is.na(df$NumberOfOpenAccounts), 0, df$NumberOfOpenAccounts))


# NumberOfCreditProblems
df$NumberOfCreditProblems <- as.integer(ifelse(is.na(df$NumberOfCreditProblems), 0, df$NumberOfCreditProblems))


# CurrentCreditBalance
df$CurrentCreditBalance <- as.numeric(ifelse(is.na(df$CurrentCreditBalance), 0, df$CurrentCreditBalance))


# MaximumOpenCredit
df$MaximumOpenCredit <- as.numeric(gsub("[\\$,]", "", df$MaximumOpenCredit))
df$MaximumOpenCredit <- as.numeric(ifelse(is.na(df$MaximumOpenCredit), 0, df$MaximumOpenCredit))


# Bankruptcies
df$Bankruptcies <- as.integer(ifelse(is.na(df$Bankruptcies), 0, df$Bankruptcies))


# TaxLiens
df$TaxLiens <- as.integer(ifelse(is.na(df$TaxLiens), 0, df$TaxLiens))


# add features

# AccountsPerYearofHistory
df$AccountsPerYearofHistory <- df$NumberOfOpenAccounts / df$YearsOfCreditHistory 
df$AccountsPerYearofHistory[!is.finite(df$AccountsPerYearofHistory)] <- 0


# CreditHistoryWeight
df$CreditHistoryWeight <- df$CreditScore * df$YearsOfCreditHistory 
df$CreditHistoryWeight[!is.finite(df$CreditHistoryWeight)] <- 0


# CreditRatio
# df$CreditRatio <- df$CurrentCreditBalance / df$MaximumOpenCredit 
# df$CreditRatio[!is.finite(df$CreditRatio)] <- 0


# CreditToIncomeRatio
# df$CreditToIncomeRatio <- df$CreditScore / df$AnnualIncome 
# df$CreditToIncomeRatio[!is.finite(df$CreditToIncomeRatio)] <- 0


# DebtToIncomeRatio
df$DebtToIncomeRatio <- ((df$MonthlyDebt * 12)) / df$AnnualIncome
df$DebtToIncomeRatio[!is.finite(df$DebtToIncomeRatio)] <- 0


# DisposableIncome
# df$DisposableIncome <- df$AnnualIncome - ((df$MonthlyDebt * 12) - ifelse(df$HomeOwnership == "Own", 0, -(df$AnnualIncomeClean * .15)))
# df$DisposableIncome[!is.finite(df$DisposableIncome)] <- 0


# LoanToMaxAvailableRatio
df$LoanToMaxAvailableRatio <- ifelse(df$MaximumOpenCredit == 0, 100, df$CurrentLoanAmount / df$MaximumOpenCredit)
df$LoanToMaxAvailableRatio[!is.finite(df$LoanToMaxAvailableRatio)] <- 0


# NewCreditRatio
df$NewCreditRatio <- (df$CurrentLoanAmount + df$CurrentCreditBalance) / df$MaximumOpenCredit
df$NewCreditRatio[!is.finite(df$NewCreditRatio)] <- 0


# PctCreditUsed
# df$PctCreditUsed <- ifelse(df$MaximumOpenCredit == 0, max(100, df$MaximumOpenCredit), df$CurrentCreditBalance / df$MaximumOpenCredit)
# df$PctCreditUsed[!is.finite(df$PctCreditUsed)] <- 0


# ProblemIndicator
# df$ProblemIndicator <- ifelse(df$NumberOfCreditProblems + df$Bankruptcies + df$TaxLiens > 0, 1, 0)
# df$ProblemIndicator <- as.factor(df$ProblemIndicator)


# OwnHome
# df$OwnHome <- ifelse(df$HomeOwnership == "Own", 1, 0)
# df$OwnHome <- as.factor(df$OwnHome)


# RecentDelinquent
# df$RecentDelinquent <- ifelse(df$MonthsSinceLastDelinquent > 0 & df$MonthsSinceLastDelinquent <= 6, 1, 0)
# df$RecentDelinquent <- as.factor(df$RecentDelinquent)


# TotalCreditProblems
# df$TotalCreditProblems <- df$TaxLiens + df$Bankruptcies + df$NumberOfCreditProblems

df[is.na(df)] <- 0

maml.mapOutputPort("df")