# Reading in the required dataset

df <- read.csv("MAE_207_Midterm_Exam_W_2022_Problem_2_Data.csv")
head(df)

# Approach: Extract the significant digit from the given data
# and then calculate the probability based on the values

# a. Significant digit can be extracted as following:
# We can express any number in the format of exponential 
# Ex: 1000 = 1e3 and 1 is the significant digit hence, we can extract all the 
# values using the below line 

df$sig_dig <- as.numeric(substr(formatC(df$x, format = "e"),1,1))

# Now we can calculate the values based on the provided formula for benford's law
# P(D1 = d) = log((d+1)/d) where d = significant digit

df$bf_val <- log10((df$sig_dig + 1)/df$sig_dig)


# Now that we have the 2 probability samples we can run a chi-squared test
# on the calculated and given values to see whether data is fabricated or not

# This would be the expected setup for the following problem
# H0: The 2 variables are independent ie. they dont comply to Benford's Law
# H1: The 2 variables are related ie. they follow Benford's Law

test_statistic <- sum((df$x - df$bf_val) ** 2/df$bf_val)
pchisq(q = test_statistic, df = length(df$bf_val) -1 , lower.tail = F)

# According to the above test since the p-value ~ 0 we can reject the 
# null hypothesis and say that the data follows Benford's Law.
