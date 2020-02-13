# default parameters for setup
R = 500             # number of repeats
alpha = 0.2         # FWER level

D = 30              # size of hypothesis grid D*D
d = 5               # size of the non-null cluster
mu_1 = 3            # alternative mean value
mu_0 = 0            # null mean value

# parameters for testing methods
methods = c("Sidak", "i-fwer-tent")         # compared methods
pstar = alpha/2                             # parameter for tent/railway masking
p_l = pstar; p_u = 0.5                      # parameters for gap masking
smoothed = TRUE                             # indicator of whether to model score S

