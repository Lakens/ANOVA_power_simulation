In a 2x2 design, with factors A and B, each with 2 levels, there are 6 possible comparisons that can be made.
1. A1 vs. A2
2. A1 vs. B1
3. A1 vs. B2
4. A2 vs. B1
5. A2 vs. B2
6. B1 vs. B2

The possible comparisons is the product of the levels of all factors squared minus the product of all factors, divided by two. For a 2x2 design where each factor has two levels, this is:
(((2*2)^2)-(2*2))/2
For a 2x2x4 design, the number of possible comparisons is:
(((2*2*4)^2)-(2*2*4))/2

Each of these comparisons can have their own correlation if the factor is manipulated within subjects (if the factor is manipulated between subjects the correlation is 0).
These correlations determine the covariance matrix.
If you do not want to assume equal correlations for all paired comparisons, you can enter the correlation as a vector that determines the upper diagonal of the matrix.
The order in which the correlations are entered is very important.
The order for a 2x2 design is listed above. The general pattern is the matrix is filled from top to bottom, and left to right.

              a1_b1           a1_b2           a2_b1           a2_b2
a1_b1        1.060900        0.91            0.92            0.93
a1_b2        0.91            1.060900        0.94            0.95
a2_b1        0.92            0.94            1.060900        0.96
a2_b2        0.93            0.95            0.9             1.060900

The diagonal is generated dynamically (based on the stadard devication - EXPLAIN MORE).
We would enter this the upper right triangle as:

design_result <- ANOVA_design(string = "2w*2w",
                              n = 80,
                              mu = c(1.03, 1.21, 0.98, 1.01),
                              sd = 1.03,
                              r <- c(0.91, 0.92, 0.93, 0.94, 0.95, 0.96),
                              p_adjust = "none",
                              labelnames = c("age", "old", "young", "speed", "fast", "slow"))


We can see the correlation matrix by asking for design_result$sigmatrix


require(mvtnorm, quietly = TRUE)
#require(emmeans, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(gridExtra, quietly = TRUE)
require(reshape2, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)

string = "2w*2w"
n = 20
mu = c(1.03, 1.21, 0.98, 1.01)
sd = 1.03
r=0.87
p_adjust = "none"
labelnames = c("age", "a1", "a2", "speed", "s1", "s2")


generate_cor_matrix  <- function(vars = 3, cors = 0, mu = 0, sd = 1) {
  if (length(sd) == 1) {
    sd <- rep(sd, vars)
  } else if (length(sd) != vars) {
    stop("the length of sd must be 1 or vars");
  }
  
  # correlation matrix
  if (class(cors) == "numeric" & length(cors) == 1) {
    if (cors >=-1 & cors <=1) {
      cors = rep(cors, vars*(vars-1)/2)
    } else {
      stop("cors must be between -1 and 1")
    }
  }
  
  if (class(cors) == "matrix") { 
    if (!is.numeric(cors)) {
      stop("cors matrix not numeric")
    } else if (dim(cors)[1] != vars || dim(cors)[2] != vars) {
      stop("cors matrix wrong dimensions")
    } else if (sum(cors == t(cors)) != (nrow(cors)^2)) {
      stop("cors matrix not symmetric")
    } else {
      cor_mat <- cors
    }
  } else if (length(cors) == vars*vars) {
    cor_mat <- matrix(cors, vars)
  } else if (length(cors) == vars*(vars-1)/2) {
    # generate full matrix from vector of upper right triangle
    
    cor_mat <- matrix(nrow=vars, ncol = vars)
    upcounter = 1
    lowcounter = 1
    for (col in 1:vars) {
      for (row in 1:vars) {
        if (row == col) {
          # diagonal
          cor_mat[row, col] = 1
        } else if (row > col) {
          # lower left triangle
          cor_mat[row, col] = cors[lowcounter]
          lowcounter <- lowcounter + 1
        }
      }
    }
    for (row in 1:vars) {
      for (col in 1:vars) {
        if (row < col) {
          # upper right triangle
          cor_mat[row, col] = cors[upcounter]
          upcounter <- upcounter + 1
        }
      }
    }
  }
  
  # check matrix is positive definite
  tol <- 1e-08
  ev <- eigen(cor_mat, only.values = TRUE)$values
  if (sum(ev < tol)) {
    stop("correlation matrix not positive definite")
  }
  
  return(cor_mat)
}  
cor_mat <- generate_cor_matrix(vars = vars, cors = cors)

sd_for_sigma <- sd #added to prevent changing sd which is now passed on
if (length(sd_for_sigma) == 1) {
  sd_for_sigma <- rep(sd_for_sigma, vars)
} else if (length(sd_for_sigma) != vars) {
  stop("the length of sd_for_sigma must be 1 or vars");
}

sigma <- (sd_for_sigma %*% t(sd_for_sigma)) * cor_mat #Our earlier code had a bug, with SD on the diagonal. Not correct! Thanks Lisa.

#check if code below works with more than 1 factor!
row.names(sigma) <- design_list
colnames(sigma) <- design_list

