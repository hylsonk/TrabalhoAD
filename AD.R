library(optimbase)
library(pracma)

Q = matrix(c(-1,2,1,-2), ncol = 2, nrow = 2)
Qt = transpose(Q)
s = size(Qt, 1)
Qt[2,] = 1
e = zeros(s,1)

e[s]=1

Ans = mldivide(Qt, e, pinv= TRUE)

Ans