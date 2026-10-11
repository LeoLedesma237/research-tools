# Our goal is to basically try to create simulated data that can closley look like our CPT data
# For this code the goal is to be as efficient as possible

# Load in packages
library(EMC2)
library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

# load in all custom ddm functions
source(here("ddm", "load.R"))

# Load in additional custom ddm functions
source(here("ddm", "individual", "functions.R"))

###
###### Part 1: Setting up default WDM parameter values and prior information
###

# Create a vector with default DDM parameter values
p_vector <- c()
{p_vector["v_match:Sgo"] = 4
p_vector["v_match:Snogo"] = 1.5
p_vector["a"] = log(1) 
p_vector["t0"] = log(.3) 
p_vector["Z"] = qnorm(0.5)} 

# Create a vector with default prior means
prior_pvec <- rep(0, length(p_vector)) # start with 0's
{names(prior_pvec) <- names(p_vector)
prior_pvec[grepl("^v_",names(prior_pvec))] = 2 
prior_pvec[grepl("^a",names(prior_pvec))] = log(1) 
prior_pvec[grepl("^Z",names(prior_pvec))] = qnorm(0.5) 
prior_pvec[grepl("^t0",names(prior_pvec))] = log(.3)}

# Create a vector with default prior variances 
prior_vvec <- rep(1, length(p_vector)) # start with 1's
{names(prior_vvec) <- names(p_vector)
prior_vvec[grepl("^t0",names(prior_pvec))] = .5 
prior_vvec[grepl("^Z",names(prior_pvec))] = .5 
prior_vvec[grepl("^sv",names(prior_pvec))] = .5 }

###
####### Part 2: Investigating WDM parameter effects on simulated data
###

# Create a WDM specification
WDM_formula <- list(v~0 + match:S, Z~1, a~1, t0~1, s~1)
WDM <- create_design(WDM_formula)

# Generate simulated data as a function of changing parameters
a_vec  <- log(seq(.05, 3.5, length.out = 30))
v_vec  <- seq(0, 5, length.out = 30)
t0_vec <- log(seq(.05, .5, length.out = 30))
Z_vec  <- qnorm(seq(.01, .99, length.out = 30))

# Simulate data for the drift rate
a_sim <- simulate_DDM(param_vector = a_vec, param_name = c("a"), mod = WDM)
v_sim <- simulate_DDM(param_vector = v_vec, param_name = c("v_match:Sgo", "v_match:Snogo"), mod = WDM)
t0_sim <- simulate_DDM(param_vector = t0_vec, param_name = c("t0"), mod = WDM)
Z_sim <- simulate_DDM(param_vector = Z_vec, param_name = c("Z"), mod = WDM)

# Produce plots that show the parameter specific effects to data descriptives
plot_results(a_sim, transform = "exp", title = "Param: a")
plot_results(v_sim, title = "Param: v")
plot_results(t0_sim, transform = "exp", title = "Param: t0")
plot_results(Z_sim, transform = "pnorm", title = "Param: Z")

# Describe results for accuracy and reaction time
describe_results(a_sim, transform = "exp")
describe_results(v_sim)
describe_results(t0_sim, transform = "exp")
describe_results(Z_sim, transform = "pnorm")


###
####### Part 3: Between trial variability
###

# Create between trial variability models
sv_DDM  <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, sv~1))
st0_DDM <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, st0~1))
SZ_DDM  <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, SZ~1))

# Generate simulated data as a function of changing parameters
sv_vec <- log(seq(.1, 9.5, .1))
st0_vec <- log(seq(.05, .50, .05))
SZ_vec <-  qnorm(seq(.01, .99, .02)) 

# Simulate data for the drift rate
sv_sim <- simulate_DDM(param_vector = sv_vec, param_name = c("sv"), mod = sv_DDM)
st0_sim <- simulate_DDM(param_vector = st0_vec, param_name = c("st0"), mod = st0_DDM)
SZ_sim <- simulate_DDM(param_vector = SZ_vec, param_name = c("SZ"), mod = SZ_DDM)

# Produce plots that show the parameter specific effects to data descriptives
plot_results(sv_sim, transform = "exp", title = "Param: sv")
plot_results(st0_sim, transform = "exp", title = "Param: st0")
plot_results(SZ_sim, transform = "pnorm", title = "Param: SZ")

# Describe results for accuracy and reaction time
describe_results(sv_sim, transform = "exp")
describe_results(st0_sim, transform = "exp")
describe_results(SZ_sim, transform = "pnorm")

####
####### Part 4: Multiple between-trial variability
####

# Create the multiple between trial variability models
sv_st0_DDM <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, sv~1, st0 ~1))
SZ_sv_DDM  <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, sv~1, SZ ~1))
SZ_st0_DDM <- create_design(list(v~0 + match:S, Z~1, a~1, t0~1, s~1, st0~1, SZ~1))

# Generate simulated data as a function of changing parameters
sv_vec <- log(seq(.1, 9.5, .1))
st0_vec <- log(seq(.05, .50, .05))
SZ_vec <-  qnorm(seq(.01, .99, .02)) 

# Simulate the data
sv_st0_sim <- simulate_DDM(param_vector = sv_vec, param_name = c("sv"), mod = sv_st0_DDM,
                       param_vector2 = st0_vec, param_name2 = c("st0"))

SZ_sv_sim  <- simulate_DDM(param_vector = SZ_vec, param_name = c("SZ"), mod = SZ_sv_DDM,
                           param_vector2 = sv_vec, param_name2 = c("sv"))

SZ_st0_sim <- simulate_DDM(param_vector = SZ_vec, param_name = c("SZ"), mod = SZ_st0_DDM,
                           param_vector2 = st0_vec, param_name2 = c("st0"))


# Describe results for accuracy and reaction time
describe_results(SZ_st0_sim, transform = "pnorm", 
                 parameter2 = TRUE, transform2 = "exp")


plot_results2(SZ_st0_sim, transform = "exp", transform2 = "SZ")
plot_results2(SZ_sv_sim, transform = "exp", transform2 = "SZ")


st0_SZ_sim$accuracy %>%
  mutate(
    param = exp(param),
    param2 = exp(param2)
  ) %>%
  ggplot(aes(x = param, y = param2, fill = `S == R`)) +
  geom_tile() +
  facet_wrap(~S) +
  scale_fill_viridis_c() +
  labs(x = "Parameter 1", y = "Parameter 2", fill = "Accuracy") +
  theme_minimal()

st0_SZ_sim$accuracy %>%
  mutate(
    param = exp(param),
    param2 = factor(round(exp(param2), 2))
  ) %>%
  ggplot(aes(x = param, y = `S == R`,
             color = param2, group = param2)) +
  geom_line() +
  facet_wrap(~S, nrow = 2) +
  labs(x = "Parameter 1", y = "Accuracy",
       color = "Parameter 2") +
  theme_minimal()


