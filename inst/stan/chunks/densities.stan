real lower_normal_lpdf(real y, real mean, real sd, real lower_input) {
  return(normal_lpdf(y | mean, sd) - normal_lccdf(lower_input | mean, sd));
}

real upper_normal_lpdf(real y, real mean, real sd, real upper_input) {
  return(normal_lpdf(y | mean, sd) - normal_lcdf(upper_input | mean, sd));
}

real inner_normal_lpdf(real y, real mean, real sd, real lower_input, real upper_input) {
  return(normal_lpdf(y | mean, sd) -
         log_diff_exp(normal_lcdf(upper_input | mean, sd),
                      normal_lcdf(lower_input | mean, sd)));
}

real double_normal_lpdf(real y, real mean, real sd, real lower_input, real upper_input) {
  return(normal_lpdf(y | mean, sd) -
         log_sum_exp(normal_lcdf(upper_input | mean, sd),
                      normal_lcdf(-lower_input | -mean, sd)));
}
