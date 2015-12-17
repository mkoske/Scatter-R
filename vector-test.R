# Test, if HEOM could be made faster
# Trying to vectorize

data <- read.csv("testdata_3.csv")

a <- data[1, ]
b <- data[2, ]

print(a)
print(b)

which(a == b)       # Indices of those that are same
as.numeric(a == b)  # Numeric vector, where 1 if theyr'e same, 0 if not

as.numeric(is.na(a))# Like above, but 1, if a[i] is NA. This could be used to compare
                    # items and finally to replace NA's with proper values (1)
                    # like ... TODO: figure out an example