real lower_normal_lpdf(real y, real mean, real sd, real input_lower) {
  return(normal_lpdf(y | mean, sd) - normal_lccdf(input_lower | mean, sd));
}

real upper_normal_lpdf(real y, real mean, real sd, real input_upper) {
  return(normal_lpdf(y | mean, sd) - normal_lcdf(input_upper | mean, sd));
}

real inner_normal_lpdf(real y, real mean, real sd, real input_lower, real input_upper) {
  return(normal_lpdf(y | mean, sd) -
         log(normal_cdf(input_upper | mean, sd) -
             normal_cdf(input_lower | mean, sd)));
}

real double_normal_lpdf(real y, real mean, real sd, real input_lower, real input_upper) {
  return(normal_lpdf(y | mean, sd) -
         log(normal_cdf(input_upper | mean, sd) + normal_cdf(-input_lower | -mean, sd)));
}
