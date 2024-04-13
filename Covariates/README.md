# `Covariates` folder description

Do not forget to fixe some of the files here. In the second big if part, it should be

- `center_going = -(original_distance - center_sign)` instead of `center_going = -center_sign`
- `center_coming = as.numeric(edge_length[coming_edges[i]]) + (original_distance - center_sign)` instead of `center_coming = as.numeric(edge_length[coming_edges[i]]) + center_sign`
- Notice also that the if structure was changed

Files `Covariates/6Creates_covariates_on_mesh.R` and `Covariates/7Creates_covariates_on_graph_without_consec_zeros.R` are already fixed.


## `1Creates_covariates.R`

This file creates covariates taking into account the effect of the signs in the same edge.

## `2Creates_covariates_on_mesh.R` 

This file creates covariates on mesh using the same strategy as in `1Creates_covariates.R`.

## `3Creates_covariates.R` and `4Creates_covariates.R` 

This files are not important. They were used to test some variations of the algorithm that computes the covariates.

## `5Creates_covariates.R` 

This file creates covariates taking into account the effect of signs in neighboring edges.
