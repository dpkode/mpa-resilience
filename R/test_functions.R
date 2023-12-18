# 
# sim_num = num_sims_dt
# reserve_frac = reserve_fracs_dt
# flep_ratio = flep_ratios_dt
# year = year_vals_dt
# age = age_vals_dt
# patch_num = patch_num_dt
# number = NA_real_
# yield = NA_real_

#### USED
run_sims_sm_write <- function(expmts,
                              spec_name,
                              alpha_bh,
                              beta_bh,
                              recr_sd,
                              num_sims,
                              frac_reserves,
                              fleps,
                              n_years,
                              noise_vec,
                              noise_dat,
                              num_patches,
                              les_mats,
                              conn_mats,
                              in_out_fracs,
                              sim_results,
                              sim_spec_dervars,
                              f_vals,
                              spec_flep_f_ind,
                              sim_spec_pars) {
  
  con <-
    dbConnect(duckdb::duckdb(),
              dbdir = "data/sim_out.duckdb",
              read_only = FALSE)
  
  for (i in 1:length(expmts)) {
    for (j in 1:length(spec_name)) {
      for (k in noise_vec) {
        for (l in 1:length(recr_sd)) {
          for (m in 1:num_sims) {
            print(paste0("num_sims: ", m))
            for (n in 1:length(frac_reserves)) {
              # print(paste0("frac_res: ", frac_reserves[n]))
              for (o in 1:length(fleps)) {
                # print(paste0("fleps: ", fleps[o]))
                dt_one <- data.table(sim_num = m, 
                                     reserve_frac=as.character(frac_reserves[n]), 
                                     flep_ratio=as.character(fleps[o]), 
                                     number = list(), 
                                     yield = list())
                
                # dt_one <- dt[i = .(
                #   expmts[i],
                #   spec_name[j],
                #   alpha_bh[j],
                #   recr_sd[m],
                #   frac_reserves[o],
                #   fleps[n],
                #   p,
                #   q
                # ), ]
                # dt_one <- dt[i = sim_num == m & near(reserve_frac, frac_reserves[n]) & near(flep_ratio, fleps[o]),]
                
                out <- run_patch_sims2(
                  t_steps = n_years,
                  num_patches = num_patches,
                  les_mat = les_mats[[j]][[o]],
                  con_mat = conn_mats[[n]],
                  frac_in_out = in_out_fracs[[n]],
                  N = sim_results[[j]][[o]],
                  # this needs the values from a unfished_N1 for each species
                  selectivity = as.integer(sim_spec_dervars[[j]][["F_select"]]),
                  fishing_mortality = c(0, f_vals[spec_flep_f_ind[[j]][[o]]]),
                  natural_mortality = sim_spec_pars[[j]][["M"]],
                  weight_at_age = sim_spec_pars[[j]][["biom_const"]] * sim_spec_dervars[[j]][["length_at_age"]] ^
                    sim_spec_pars[[j]][["biom_exp"]],
                  alpha = alpha_bh[[j]],
                  beta = beta_bh,
                  noise_series = noise_dat[,m] ########
                )
                
                out_Ns <-
                  setnames(
                    as.data.table(out[["Ns"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "number")
                  )
                setcolorder(out_Ns, c("year", "age", "patch_num"))
                setkeyv(out_Ns, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Ns[j = patch_num]) == num_patches)
                
                out_Y <-
                  setnames(
                    as.data.table(out[["Y"]]),
                    old = c("V1", "V2", "V3", "value"),
                    new = c("age", "year", "patch_num", "yield")
                  )
                setcolorder(out_Y, c("year", "age", "patch_num"))
                setkeyv(out_Y, c("year", "age", "patch_num"))
                
                stopifnot(max(out_Y[j = patch_num]) == num_patches)
                
                dt_one[, j =  `:=` (number = list(out_Ns),
                                    yield = list(out_Y))] 
                
                rm(out_Ns, out_Y, out) 
                
                dt_one <- dt_one %>%
                  unnest(c(number, yield), names_repair = "minimal") 
                
                dt_one <- dt_one[, c(1,2,3,4,5,6,7,11)]
                
                if (n == 1 & o == 1) {
                  dt_ddb <- dt_one
                  # print("dt_ddb")
                  # print(dim(dt_ddb))
                  rm(dt_one)
                } else {
                  # print("dt_ddb before append")
                  # print(dim(dt_ddb))
                  # print("dt_one before append")
                  # print(dim(dt_one))
                  dt_ddb <- rbind(dt_ddb, dt_one)
                  # print("dt_ddb after append")
                  # print(dim(dt_ddb))
                  rm(dt_one)
                }
              }
            }
            print("Before ddb write")
            
            tab_name <- paste0(expmts[i], "_", tolower(sub(
              " ", "_", spec_name[j]
            )), "_", k, "_", substring(as.character(recr_sd[l]), 3))
            
            print(tab_name)
            
            print(tail(dt_ddb))
            duckdb::dbWriteTable(con,
                                 tab_name,
                                 dt_ddb,
                                 append = TRUE) # FALSE
            print("After ddb write")
            print(tail(dt_ddb))
            rm(dt_ddb)
            
          }
        }
      }
      print(paste0("dispersal: ", expmts[i], " and species: ", spec_name[j],
                   " and ", k, " noise ", " and recruit sd of ", recr_sd[l]))
    }
  }
  DBI::dbDisconnect(con, shutdown = TRUE)
  return(dt)
}

