/**
* Folded normal distribution
*
* @param y Point where the density / cdf is evaluated.
* @param mean Center of the normal distribution.
* @param sd Standard deviation of the normal.
* @return A real number, the density / cdf at the point.
*/

real fnormal_lpdf(real y, real mean, real sd) {
  return(log_sum_exp(normal_lpdf(y | mean, sd), normal_lpdf(y | -mean, sd)));
}

real fnormal_cdf(real y, real mean, real sd) {
  return(normal_cdf(y, mean, sd) + normal_cdf(y, -mean, sd) - sd);
}

real fnormal_ccdf(real y, real mean, real sd) {
  return(2 - normal_cdf(y, mean, sd) - normal_cdf(y, -mean, sd));
}

real fnormal_lcdf(real y, real mean, real sd) {
  return(log(normal_cdf(y, mean, sd) +
               normal_cdf(y, -mean, sd) - sd));
}

real fnormal_lccdf(real y, real mean, real sd) {
  return(log(2 - normal_cdf(y, mean, sd) - normal_cdf(y, -mean, sd)));
}

real fnormal_rng(real mean, real sd) {
  return(fabs(normal_rng(mean, sd)));
}


/**
* Truncated (folded) normal distribution
*
* Log-densities for lower and upper truncated (folded) normals.
*
* @param y Point where the density is evaluated.
* @param mean Center of the normal distribution.
* @param sd Standard deviation of the normal.
* @param lower Lower truncation point.
* @param upper Upper truncation point.
* @return A real number, the density at the point.
*/

real lower_fnormal_lpdf(real y, real mean, real sd, real lower) {
  return(fnormal_lpdf(y | mean, sd) - fnormal_lccdf(lower | mean, sd));
}

real upper_fnormal_lpdf(real y, real mean, real sd, real upper) {
  return(fnormal_lpdf(y | mean, sd) - fnormal_lcdf(upper | mean, sd));
}

real inner_fnormal_lpdf(real y, real mean, real sd, real lower, real upper) {
  return(fnormal_lpdf(y | mean, sd) -
         log(fnormal_cdf(upper, mean, sd) -
             fnormal_cdf(lower, mean, sd)));
}

real double_fnormal_lpdf(real y, real mean, real sd, real lower, real upper) {
  return(fnormal_lpdf(y | mean, sd) -
         log(fnormal_cdf(upper, mean, sd) + fnormal_ccdf(lower, mean, sd)));
}

real lower_normal_lpdf(real y, real mean, real sd, real lower) {
  return(normal_lpdf(y | mean, sd) - normal_lccdf(lower | mean, sd));
}

real upper_normal_lpdf(real y, real mean, real sd, real upper) {
  return(normal_lpdf(y | mean, sd) - normal_lcdf(upper | mean, sd));
}

real inner_normal_lpdf(real y, real mean, real sd, real lower, real upper) {
  return(normal_lpdf(y | mean, sd) -
         log(normal_cdf(upper, mean, sd) -
             normal_cdf(lower, mean, sd)));
}

real double_normal_lpdf(real y, real mean, real sd, real lower, real upper) {
  return(normal_lpdf(y | mean, sd) -
         log(normal_cdf(upper, mean, sd) + normal_cdf(-lower, -mean, sd)));
}
