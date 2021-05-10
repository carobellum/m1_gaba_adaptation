# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script name:  Test_difference_spearman.R
# 
# Author:       Caroline Nettekoven, 2021
# Contact:      nettekoven-enquiries@web.de 
# 
# Description:  Function to test whether two Spearman's coefficients
#               are significantly different. Transforms coefficients 
#               into z-scores using Fisher's z-transformation. Then
#               performs one-tailed z-tests on the z-scores given the 
#               standard error of each z-score.
# 
#               This method is standard for testing the equality between
#               Pearson’s correlation coefficients, but less commonly used
#               for Spearman’s coefficients. However, simulations show
#               that this practice is justified for Spearman’s
#               coefficients and more robust than using Pearson’s
#               correlation coefficients on non-normal data to test
#               for differences in correlations (Myers, 2006).
# 
# Parameters: 
# 
#   results1    Results of first correlation.
#               Object is in the format of the output from the "rcorr"
#               function from the Hmisc package.
# 
#   results2    Results of second correlation.
#               Same format as results1.
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --------------------------------------------------------------------
Test_difference_spearman <- function(results1, results2) {
library(psych)
s1=results1$r[2]
s2=results2$r[2]

# Method 1: Test difference between spearman correlation coefficients
z_s1=fisherz( s1 )
z_s2=fisherz( s2 )
N1=results1$n[2]
N2=results2$n[2]
z_diff_spearman = (z_s1 - z_s2) / (sqrt( (1/(N1 - 3)) + (1/(N2 - 3)) ) )
# pval_spearman <- pnorm(z_diff_spearman)
pval_spearman <- pnorm(-abs(z_diff_spearman))

# Round according to convention: 
if (pval_spearman < 0.001) {
  pval_spearman_round = 0.001 
} else if (pval_spearman < 0.01) {
    pval_spearman_round = 0.01 
} else {
      pval_spearman_round = round(pval_spearman,2) }

# Method 2: Test difference between spearman correlation coefficients converted to Pearsons
p1= 2*( sin( s1*( pi/6 )) )
p2= 2*( sin( s2*( pi/6 )) )
z_p1=fisherz( p1 )
z_p2=fisherz( p2 )
z_diff_pearsons = (z_p1 - z_p2) / (sqrt( (1/(N1 - 3)) + (1/(N2 - 3)) ) )
# pval_pearsons <- pnorm(z_diff_pearsons)
# pval_pearsons <- 2*pnorm(-abs(z_diff_pearsons)) # This would be the two-tailed z-test
pval_pearsons <- pnorm(-abs(z_diff_pearsons)) # This would be the one-tailed z-test

if (pval_pearsons < 0.001) {
  pval_pearsons_round = 0.001 
} else if (pval_pearsons < 0.01) {
  pval_pearsons_round = 0.01 
} else {
  pval_pearsons_round = round(pval_pearsons,2) }


if (pval_spearman < 0.05 || pval_pearsons < 0.05)
{
  cat("Significant difference between correlations.",
     "\n\n", "Difference between spearmans.","\n", "Zscore: ", round(z_diff_spearman,2), "pval: ", pval_spearman_round, "(Exact:", pval_spearman, ")") 
  } else {
  cat("No significant difference between correlations.",
      "\n\n", "Difference between spearmans.","\n", "Zscore: ", round(z_diff_spearman,2), "pval: ", pval_spearman_round, "(Exact:", pval_spearman, ")") 
  }

}