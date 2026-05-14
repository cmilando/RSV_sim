#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================
library(data.table)
library(ggplot2)

get_contact_plot <- function(GET_COMMUNITY) {

  x_l <- readRDS(paste0("x_l_", GET_COMMUNITY, ".RDS"))

  x_df <- do.call(rbind, x_l)

  head(x_df)

  group_cols = c(
    'ref_age', 'contact_age'
  )


  # how many people you are interacting with
  # what if you interact with the same person in multiple environments?

  # so at the person level is c(1,0)
  # and then you average up

  # volume version vs a probability version

  # (1) network analysis approach
  # (2) age-stratified approach without networks

  x_df_agg <- x_df[, .(
    sum_pt_sum = sum(pt_sum)), by = group_cols
  ]

  x_df_agg <- x_df[, .(
    mean_pt_sum = mean(pt_sum)), by = group_cols
  ]

  x_df_agg_true <- x_df_agg

  summary(x_df_agg$sum_pt_sum)

  # reset 0 to NA
  rr <- which(x_df_agg$mean_pt_sum == 0)
  x_df_agg$mean_pt_sum[rr] <- NA

  return(x_df_agg)

}

total_contact <- get_contact_plot(GET_COMMUNITY = T)
total_contact_no_comm <- get_contact_plot(GET_COMMUNITY = F)

ggplot(total_contact) +
  geom_tile(aes(x = contact_age, y = ref_age, fill = mean_pt_sum),
            color = 'white', linewidth = 0.005) +
  scale_fill_viridis_c() + ggtitle("Full Contact Matrix")

ggplot(total_contact_no_comm) +
  geom_tile(aes(x = contact_age, y = ref_age, fill = mean_pt_sum),
            color = 'white', linewidth = 0.005) +
  scale_fill_viridis_c() + ggtitle("Contact Matrix - No Community Contact")

# who acquires infection from whom
# you have interacted with this person
# not necessarily scaled by volume
# when you later on have a transmission probability
# and then you add the volume post-hoc, there is greater volume
# based on what network each person is in

#' ============================================================================
#' ////////////////////////////////////////////////////////////////////////////
#' ============================================================================

# since community is everyone can we just take a random population sample
# then you don't have to do the individual accounting

# basically you could get the average contact matrix without community size
# and then you could get the final contact matrix from the literature presumably

# (1) assume community size lambda
# (2) then sample 100 draws of that size from the age distribution
# (3) then create a contact matrix addition
# (4) then add it to the x without community size distribution
# (5) then compare it to the target and adjust lambda

# well actually, you might not need STAN right, because
# its just one parameters

# why don't you start with NLOPTR and then you can do others later

# you know lambda_true is 50

lambda_guess  = 100

N_draws <- 100
sampled_pois <- rpois(N_draws, lambda_guess)

sample_i = 1
age_dist <- round(runif(1e5, min = 0, max = 100)) # same as before
oo <- vector("list", N_draws)
for(sample_i in 1:N_draws) {
  x1 <- sample(age_dist, sampled_pois[sample_i], replace = T)
  # xdf <- data.table(sample_i = sample_i, age = x1, interact = 1)
  # xdf <- xdf[, .(interact_sum = sum(interact)), .(sample_i, age)]
  oo[[sample_i]] <- x1
}
oo[[1]]
oo[[2]]

# now get xgrid
xgrid <- as.data.table(tidyr::expand_grid(age1 = as.numeric(0:100),
                                          age2 = as.numeric(0:100)))

# so turn a vector into a matrix
get_xgrid2 <- function(i) {
  v1 <- apply(combn(oo[[i]],2),2,paste,collapse='_')
  v1_mat <- data.table(do.call( rbind, strsplit(v1, "_")))
  v1_mat$interact = 1
  v1_mat_b <- v1_mat[, .(sum_interact = sum(interact)), .(V1, V2)]
  v1_mat_b$age1 <- as.numeric(v1_mat_b$V1)
  v1_mat_b$age2 <- as.numeric(v1_mat_b$V2)
  v1_mat_b <- v1_mat_b[, .(age1, age2, sum_interact)]
  v1_mat_b

  xgrid2 <- v1_mat_b[
    xgrid, on = c('age1', 'age2')
  ]

  # library(ggplot2)
  # ggplot(xgrid2) +
  #   geom_tile(aes(x = age1, y = age2, fill = sum_interact))

  rr <- which(is.na(xgrid2$sum_interact))
  xgrid2$sum_interact[rr] <- 0
  xgrid2
}

xgrid2_l <- lapply(1:length(oo), get_xgrid2)
xgrid2_df <- do.call(rbind, xgrid2_l)
xgrid2_sum <- xgrid2_df[, .(avg_interaction = mean(sum_interact)), .(age1, age2)]

# library(ggplot2)
ggplot(xgrid2_sum) +
  geom_tile(aes(x = age1, y = age2, fill = avg_interaction))

# ok so now, addt t
setnames(xgrid2_sum, 'age1', 'ref_age')
setnames(xgrid2_sum, 'age2', 'contact_age')
setnames(xgrid2_sum, 'avg_interaction', 'avg_comm_interaction')

xgrid3 <- xgrid2_sum[
  total_contact_no_comm, on = c('ref_age', 'contact_age')
]
dim(xgrid3)
head(xgrid3)

xgrid3$test_contact <- xgrid3$avg_comm_interaction + xgrid3$mean_pt_sum

setnames(total_contact, "mean_pt_sum", "true_mean_pt_sum")
head(total_contact)

xgrid3 <- xgrid3[
  total_contact, on = c('ref_age', 'contact_age')
]
head(xgrid3)

ggplot(xgrid3) +
  geom_tile(aes(x = ref_age, y = contact_age,
                fill = true_mean_pt_sum - test_contact))

plot(xgrid3$true_mean_pt_sum, xgrid3$test_contact)
abline(a = 0, b = 1)
