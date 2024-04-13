# Model 1

### Parameters

- h = 0.2
- nu.upper.bound = 1.5
- nu to be estimated
- all data

### Call

```
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
>   #sample_n(1000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> 
> # -----------------------------------------------------------------------------
> 
> # WM
> 
> # -----------------------------------------------------------------------------
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
inla.mkl: tabulate-Qfunc.c:147: GMRFLib_tabulate_Qfunc_core: Assertion `arg->Q->a[0] >= 0.0' failed.
Segmentation fault
inla.mkl: tabulate-Qfunc.c:147: GMRFLib_tabulate_Qfunc_core: Assertion `arg->Q->a[0] >= 0.0' failed.
Segmentation fault
Error: C stack usage  106187851 is too close to the limit
```


# Model 2

### Parameters

- h = 0.2
- nu.upper.bound = 0.5
- nu to be estimated
- slice_sample(n = 100000, replace = FALSE)

### Call

```
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) %>%
+   slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> 
> # -----------------------------------------------------------------------------
> 
> # WM
> 
> # -----------------------------------------------------------------------------
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
inla.mkl: tabulate-Qfunc.c:147: GMRFLib_tabulate_Qfunc_core: Assertion `arg->Q->a[0] >= 0.0' failed.
Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern
	
Segmentation fault
Error: C stack usage  102439892 is too close to the limit

```



# Model 3

### Parameters

- h = 0.2
- nu.upper.bound = 1.5
- nu to be estimated
- all data
- initial values for non stationary from stationary

### Call

```
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
>   #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> # -----------------------------------------------------------------------------------
> 
> # WM
> 
> # -----------------------------------------------------------------------------------
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  53393671 is too close to the limit
```


### Model 4

```
# Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
>   #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "spde", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.tau = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.kappa = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.tau$mode, fit.rspde$summary.log.kappa$mode, 0, 0),
+                                             B.tau = B.tau,
+                                             B.kappa = B.kappa,
+                                             parameterization = "spde", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error messsage

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  52465122 is too close to the limit
```


### Model 5

```
# Open a connection to a text file
Warning message:
R graphics engine version 16 is not supported by this version of RStudio. The Plots tab will be disabled until a newer version of RStudio is installed. 
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun.txt")
> 
> library(MetricGraph)

Attaching package: ‘MetricGraph’

The following object is masked from ‘package:stats’:

    filter

> library(rSPDE)
Loading required package: Matrix
> library(plotly)
Loading required package: ggplot2

Attaching package: ‘plotly’

The following object is masked from ‘package:ggplot2’:

    last_plot

The following object is masked from ‘package:stats’:

    filter

The following object is masked from ‘package:graphics’:

    layout

> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> library(MASS)

Attaching package: ‘MASS’

The following object is masked from ‘package:dplyr’:

    select

The following object is masked from ‘package:plotly’:

    select

The following object is masked from ‘package:MetricGraph’:

    select

> library(glmnet)
Loaded glmnet 4.1-8
> library(car)
Loading required package: carData

Attaching package: ‘car’

The following object is masked from ‘package:dplyr’:

    recode

> library(inlabru)
Loading required package: fmesher

Attaching package: ‘inlabru’

The following object is masked from ‘package:MASS’:

    shrimp

> library(INLA)
Loading required package: sp
This is INLA_24.03.02 built 2024-03-02 17:13:00 UTC.
 - See www.r-inla.org/contact-us for how to get help.
 - List available models/likelihoods/etc with inla.list.models()
 - Use inla.doc(<NAME>) to access documentation
Warning message:
package ‘INLA’ was built under R version 4.3.3 
> library(tidyr)

Attaching package: ‘tidyr’

The following objects are masked from ‘package:Matrix’:

    expand, pack, unpack

> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
>   #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "spde", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.tau = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.kappa = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.tau$mode, fit.rspde$summary.log.kappa$mode, 0, 0),
+                                             B.tau = B.tau,
+                                             B.kappa = B.kappa,
+                                             parameterization = "spde", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  52463938 is too close to the limit
```


### Model 6

```
# Close the connection
> sink()
> # Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
>   #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> # rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
> #                                             start.theta = c(fit.rspde$summary.log.tau$mode, fit.rspde$summary.log.kappa$mode, 0, 0),
> #                                             B.tau = B.tau,
> #                                             B.kappa = B.kappa,
> #                                             parameterization = "spde", nu.upper.bound = 1.5)
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  53397848 is too close to the limit
```

### Model 7

```
# Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2rerun2.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
>   #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> # rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
> #                                             start.theta = c(fit.rspde$summary.log.tau$mode, fit.rspde$summary.log.kappa$mode, 0, 0),
> #                                             B.tau = B.tau,
> #                                             B.kappa = B.kappa,
> #                                             parameterization = "spde", nu.upper.bound = 1.5)
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  49864082 is too close to the limit
```

### Model 8

```
# Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.2
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
> #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu.upper.bound = 1.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  42872162 is too close to the limit
```

### Model 9
```
# Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check2.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.05
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
> #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu = 0.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu = 0.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  53916142 is too close to the limit
```

### Model 10

```
# Open a connection to a text file
> sink("~/Desktop/Spring 2024/Model_runs/stat_vs_nonstat_gaussian_all_data_h0.2_no_consecutuve_zeros_nu.upper.bound1.5.with.initial.val.from.stat2.check2.txt")
> 
> library(MetricGraph)
> library(rSPDE)
> library(plotly)
> library(dplyr)
> library(MASS)
> library(glmnet)
> library(car)
> library(inlabru)
> library(INLA)
> library(tidyr)
> 
> # loading the data
> load("Data_files/data_on_graph_with_covariates_no_consecutive_zeros.RData")
> load("Graph_objects/graph_construction_11_03_2024.RData")
> 
> h = 0.1
> 
> source("Covariates/6Creates_covariates_on_mesh.R")
> 
> creates_covariates_on_mesh(h)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> 
> Sys.sleep(60)
> 
> load("Data_files/data_on_mesh_with_covariates.RData")
> 
> start_time <- Sys.time()
> #set.seed(2024)
> 
> data = data_on_graph_with_covariates %>% 
+   dplyr::select(speed, 
+                 day,
+                 SpeedLimit,
+                 density_per_hour_normalized,
+                 bus,
+                 signal,
+                 stop,
+                 crossing) %>%
+   mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5)) #%>%
> #slice_sample(n = 100000, replace = FALSE)
> 
> 
> sf_graph$add_observations(data = data, group = "day", clear_obs = TRUE)
Adding observations...
The unit for edge lengths is km
The current tolerance for removing distant observations is (in km): 1.61153172520888
> sf_graph$build_mesh(h = h)
> 
> 
> mesh = data_on_mesh_with_covariates %>% mutate(bus = round(bus, 5), signal = round(signal, 5), stop = round(stop, 5), crossing = round(crossing, 5))
> 
> 
> 
> ################################################################################
> ################################# STATIONARY MODEL #############################
> ################################################################################
> 
> 
> rspde_model_statWM <- rspde.metric_graph(sf_graph, 
+                                          parameterization = "matern", nu = 0.5)
> 
> data_rspde_bru_s_WM <- graph_data_rspde(rspde_model_statWM, loc_name = "loc")
> 
> cmp_statWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_statWM)
> 
> rspde_fit_stat_WM <-
+   bru(cmp_statWM,
+       data = data_rspde_bru_s_WM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
> 
> summary(rspde_fit_stat_WM)
> 
> fit.rspde = rspde.result(rspde_fit_stat_WM, "field", rspde_model_statWM)
> summary(fit.rspde)
> 
> 
> 
> ################################################################################
> ############################# NON STATIONARY MODEL #############################
> ################################################################################
> 
> B.sigma = cbind(0, 1, 0, 
+                 mesh$SpeedLimit, 0
+                 #scale(mesh$density_per_hour), 0
+                 #mesh$bus, 0
+                 #mesh$signal, 0,
+                 #mesh$stop, 0,
+                 #mesh$crossing, 0
+ ) 
> 
> B.range = cbind(0, 0, 1, 
+                 0, mesh$SpeedLimit
+                 #0, scale(mesh$density_per_hour)
+                 #0, mesh$bus
+                 #0, mesh$signal,
+                 #0, mesh$stop,
+                 #0, mesh$crossing
+ ) 
> 
> rspde_model_nonstatWM <- rspde.metric_graph(sf_graph,
+                                             start.theta = c(fit.rspde$summary.log.std.dev$mode, fit.rspde$summary.log.range$mode, 0, 0),
+                                             B.sigma = B.sigma,
+                                             B.range = B.range,
+                                             parameterization = "matern", nu = 0.5)
> 
> data_rspde_bru_nsWM <- graph_data_rspde(rspde_model_nonstatWM, loc_name = "loc")
> 
> cmp_nonstatWM = speed ~ -1 +
+   Intercept(1) +
+   #SpeedLimit + 
+   #density_per_hour_normalized +
+   #bus +
+   #signal +
+   #stop +
+   #crossing +
+   field(loc, model = rspde_model_nonstatWM)
> 
> rspde_fit_nonstat_WM <-
+   bru(cmp_nonstatWM,
+       data = data_rspde_bru_nsWM[["data"]],
+       family = "gaussian",
+       options = list(verbose = FALSE)
+   )
```

### Error message

```
	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault


	GitId: df68e3d1323332b4ea60a93984545692ba14f07f - Sat Mar 2 19:55:18 2024 +0300
	Error:12 Reason: The Newton-Raphson optimizer did not converge
	Message: Condition `lambda < 1.0 / lambda_lim' is not TRUE
	Line:1043 Function: GMRFLib_init_GMRF_approximation_store__intern

Segmentation fault
Error: C stack usage  35186880 is too close to the limit
```

### Model 11