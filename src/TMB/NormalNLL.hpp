/// @file NormalNLL.hpp

#ifndef NormalNLL_hpp
#define NormalNLL_hpp

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR obj

/// Negative log-likelihood of the normal distribution.
template<class Type>
  Type NormalNLL(objective_function<Type>* obj) {
    DATA_VECTOR(x); // data vector
    PARAMETER(mm); // mean parameter
    PARAMETER(ss); // standard deviation parameter
    return -sum(dnorm(x,mm,ss,true)); // negative log likelihood
  }

#undef TMB_OBJECTIVE_PTR
#define TMB_OBJECTIVE_PTR this

#endif
