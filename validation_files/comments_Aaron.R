#Removed the r from: 
#   sigmatrix_2 <- matrix(0, length(mu),length(mu)) #create temp matrix filled with value of correlation, nrow and ncol set to length in mu
# (now a 0). Because r = a venctor in some cases now. And r is not needed at all (it is a dummy matrix)


#I moved the code below to the design. 

#Need to identify which columns from df to pull the factor names from
if (design_result$factors == 1) {
  cond_col <- c(4)
} else if (design_result$factors == 2) {
  cond_col <- c(4, 5)
} else {
  cond_col <- c(4, 5, 6)
}

df$cond <- as.character(interaction(df[, cond_col], sep = "_")) #create a new condition variable combine 2 columns (interaction is a cool function!)
#If it is there, the df$cond is all we need in the power function to name paired comparisons.

# I moved: 
# row.names(sigma) <- design_list
# colnames(sigma) <- design_list


# I replaced design_result$sig with design_result$sigmatrix - it worked (R is amazing like that) but we need to check all names!

#Why is this using a 'while'? Shoulld be just an ' if'  right? 

#The loop below is to avoid issues with creating the matrix associated with having a sd < r
while (sd2 < r) {
  sd2 <- sd2*10
  mu2 <- mu2*10
}

#I added things to the output of ANOVA design. Strangely enough sd and r were not forwarded. And I added cor_mat - to show the correlation matrix is as people wanted it to be (now that they can specify it).


#in POWER
#this can be deleted?
# how many studies should be simulated? 100.000 is very accurate, 10.000 reasonable accurate, 10.000 somewhat accurate
nsims = nsims

#The Shiny App throws some errors. Did not put it on the server yet. Can these be fixed? Sometimes errors that make it run on the PC make the app crash online.

Warning: Error in $: $ operator is invalid for atomic vectors
97: paste
96: renderText [C:\Users\Daniel\surfdrive\R\ANOVA_power_simulation\shiny_app/app.R#928]
                95: func
                82: origRenderFunc
                81: output$DESIGN
                1: shiny::runApp
                Warning: Error in $: $ operator is invalid for atomic vectors
                96: renderTable [C:\Users\Daniel\surfdrive\R\ANOVA_power_simulation\shiny_app/app.R#947]
                                 95: func
                                 82: origRenderFunc
                                 81: output$corMat
                                 1: shiny::runApp
                                 Warning: Error in $: $ operator is invalid for atomic vectors
                                 167: renderPlot [C:\Users\Daniel\surfdrive\R\ANOVA_power_simulation\shiny_app/app.R#953]
                                                  165: func
                                                  125: drawPlot
                                                  111: <reactive:plotObj>
                                                    95: drawReactive
                                                  82: origRenderFunc
                                                  81: output$plot
                                                  1: shiny::runApp
