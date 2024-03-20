# Assignment-11
Debugging and defensive programming
# Define the Tukey outlier function (hypothetical implementation)
> tukey.outlier <- function(x) {
+     q <- quantile(x, c(0.25, 0.75))
+     iqr <- q[2] - q[1]
+     upper_bound <- q[2] + 1.5 * iqr
+     outliers <- x > upper_bound
+     return(outliers)
+ }
> 
> # Corrected tukey_multiple function
> tukey_multiple <- function(x) {
+     outliers <- array(TRUE, dim = dim(x))
+     for (j in 1:ncol(x)) {
+         outliers[, j] <- tukey.outlier(x[, j])
+     }
+     outlier.vec <- apply(outliers, 1, all)
+     return(outlier.vec)
+ }
