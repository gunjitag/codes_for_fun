########################################################
########################################################
#Project Euler#
########################################################
########################################################

####################################################################################################
#Problem 11: Largest Product in a grid
# n the 20??20 grid below, four numbers along a diagonal line have been marked in red.
# 
# 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
# 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
# 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
# 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
# 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
# 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
# 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
# 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
# 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
# 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
# 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
# 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
# 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
# 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
# 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
# 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
# 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
# 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
# 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
# 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
# 
# The product of these numbers is 26 ?? 63 ?? 78 ?? 14 = 1788696.
# 
# What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20??20 grid?
####################################################################################################

#Clear the environment 
rm(list= ls())

# Input the Matrix
M <- matrix(c(08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08,
              49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00,
              81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65,
              52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91,
              22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80,
              24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50,
              32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70,
              67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21,
              24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72,
              21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95,
              78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92,
              16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57,
              86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58,
              19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40,
              04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66,
              88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69,
              04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36,
              20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16,
              20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54,
              01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48), 
            nrow = 20, ncol = 20, byrow = TRUE)


#Horizontal product
product_horizontal <- c()
for(i in 1:20){
  for(j in 1:17){
    product_horizontal <- c(product_horizontal, M[i,j]*M[i,j+1]*M[i,j+2]*M[i,j+3])
  }
}
max_product_horizontal <- max(product_horizontal)

#Vertical product
product_vertical <- c()
for(j in 1:20){
  for(i in 1:17){
    product_vertical <- c(product_vertical, M[i,j]*M[i+1,j]*M[i+2,j]*M[i+3,j])
  }
}
max_product_vertical <- max(product_vertical)


#Forward Diagnal product
product_diagnal <- c()
for(j in 1:17){
  for(i in 1:17){
    product_diagnal <- c(product_diagnal, M[i,j]*M[i+1,j+1]*M[i+2,j+2]*M[i+3,j+3])
  }
}
max_product_diagnal <- max(product_diagnal)


#Backward Diagnal product
product_diagnal2 <- c()
for(i in 1:17){
  for(j in 4:20){
    product_diagnal2 <- c(product_diagnal2, M[i,j]*M[i+1,j-1]*M[i+2,j-2]*M[i+3,j-3])
  }
}
max_product_diagnal2 <- max(product_diagnal2)

max_product <- max(max_product_diagnal2, max_product_diagnal, max_product_vertical, max_product_horizontal)


####################################################################################################
#Problem 12: Highly Divisible Triangular Number
# The sequence of triangle numbers is generated by adding the natural numbers. 
#So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
#   1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
# Let us list the factors of the first seven triangle numbers:
# 1: 1
# 3: 1,3
# 6: 1,2,3,6
# 10: 1,2,5,10
# 15: 1,3,5,15
# 21: 1,3,7,21
# 28: 1,2,4,7,14,28
# We can see that 28 is the first triangle number to have over five divisors.
# What is the value of the first triangle number to have over five hundred divisors?
####################################################################################################

#Clear the environment 
rm(list= ls())

#Write a function to find number of factors 
num_fac <- function(x){
  #A number has atleast 2 factors: 1 and itself
  output <- 2
  #We only need to check the number of factors till sqrt of x as any number below that has a mirror number above that
  for(i in 2:floor(sqrt(x))) {
   #Add 2 the divisor and quotient to factor list
   output <- if (x %% i == 0) output + 2 else output
  }
  #If x is divisible by sqrt(x) then that should be counted only once; Hard code 1,2,3
  output <- if(x==1) 1 else if(x == 2 | x==3) 2 else if(sqrt(x) == floor(sqrt(x))) output - 1 else output
  output
}

start_time <- Sys.time()

#Start with traingular number
i <- 2
triangular_num <- 1

num_factors <- 1
  
while(num_factors <= 500) {
  triangular_num <- triangular_num + i
  num_factors <- num_fac(triangular_num)
  i <- i + 1
}

num_factors 
triangular_num

end_time <- Sys.time()

end_time - start_time

#Takes a bit of time but does the job

#################################################################################################### 
#Problem 17: Number Letter Count
# If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
# then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
####################################################################################################

#Clear the environment 
rm(list= ls())

#one to nine
x <- nchar("one") + nchar("two") + nchar("three") + nchar("four") + nchar("five") + nchar("six") + nchar("seven") + nchar("eight") + nchar("nine")
#ten to nineteen
y <- nchar("ten") + nchar("eleven") + nchar("twelve") + nchar("thirteen") + nchar("fourteen") + nchar("fifteen") + nchar("sixteen") + nchar("seventeen") + nchar("eighteen") + nchar("nineteen")
#10s
z <- nchar("twenty") + nchar("thirty") + nchar("forty") + nchar("fifty") + nchar("sixty") + nchar("seventy") + nchar("eighty") + nchar("ninety")

# one to 99
p <- 9*x + y + 10*z

#special chars 
q <- nchar("hundred")
r <- nchar("and")
s <- nchar("onethousand")

#Total (formula worked out on paper)
10*p + 900*q + 9*99*r + 100*x + s


####################################################################################################
# Problem 19: Counting Sundays
# You are given the following information, but you may prefer to do some research for yourself.
# 1 Jan 1900 was a Monday.
# Thirty days has September,
# April, June and November.
# All the rest have thirty-one,
# Saving February alone,
# Which has twenty-eight, rain or shine.
# And on leap years, twenty-nine.
# A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
####################################################################################################

#Clear the environment 
rm(list= ls())

#Find out the day for 1 Jan 1901 
dow_1jan1900 <- 1
#365 as 1900 is not divisible by 400
dow_1jan1901 <- dow_1jan1900 + (365%%7)

#Create a vector that stores the day on which the 1st of each month falls 
dow <- c(dow_1jan1901)
#Object storing what was the day of the 1st of the current month
dow_month <- dow
#Month - start with Jan
month <- 1
#Year - start with 1901
year <- 1901

#Go on till year is 2000 iclusive
while(year < 2001) {
  #Months having 31 days
  if(month %in% c(1,3,5,7,8,10,12)) {
    dow_month <- dow_month + (31 %% 7)
    #Start again if the calculation goes above 7
    dow_month <- if(dow_month < 8) dow_month else dow_month - 7
    #Can also do: (dow_month + 31) %% 7 for the above two lines
    #Append to the vector
    dow <- c(dow, dow_month) 
  }
  
  #Months having 30 days
  else if(month %in% c(4,6,9,11)) {
    dow_month <- dow_month + (30 %% 7)
    dow_month <- if(dow_month < 8) dow_month else dow_month - 7
    dow <- c(dow, dow_month) 
  }
  
  #February in leap years has 29 days
  else if(month == 2 & ((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0)){
    dow_month <- dow_month + (29 %% 7)
    dow_month <- if(dow_month < 8) dow_month else dow_month - 7
    dow <- c(dow, dow_month) 
  }
  
  #February in non-leap years has 28 days
  else {
    dow <- c(dow, dow_month) 
  }
  
  #Increase month counter till 12 and then start again
  month <- if(month + 1 < 13) month + 1 else 1
  #Increase year counter every 12 months
  year <- if(month + 1 == 13) year + 1 else year
}

#Number of months beginning on Sunday (7th day of week)
length(dow[dow == 7])


####################################################################################################
# Problem 20: Factorial digit sum
# n! means n ?? (n ??? 1) ?? ... ?? 3 ?? 2 ?? 1
# For example, 10! = 10 ?? 9 ?? ... ?? 3 ?? 2 ?? 1 = 3628800,
# and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# Find the sum of the digits in the number 100!
####################################################################################################

#Clear the environment 
rm(list= ls())

#Load a package that makes things easier
library(gmp)

#Get factorial of 99 - use as.bigz to not lose precision
num <- as.bigz(99)
factorial_num <- factorial(num)

#R cannot perform mathematical operations on such a big number, so I convert it to a string
num_fac_char <- toString(factorial_num)

#Split the string, store in a vector, convert individual component to the numbers
vec_num_components <- as.numeric(unlist(strsplit(num_fac_char, "")))

#Sum of numbers
sum(vec_num_components)



