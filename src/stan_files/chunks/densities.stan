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
