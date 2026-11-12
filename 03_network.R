# network size
# clicque size
# and contact matrix
# -- should give us the chain we want

# ****
# do this for each environment then join individuals together
# ****

# ****
# maybe what you do is
# - do one person at a time
# - when one person selects another
# - mark that connection in the other persons clique as well
# - and reduce their probability by 1 so p_new = max(0, p_curr - 1)
# - and reduce their clique size total by 1 using the same function
# ****

# ****
# how do you keep it from being a really long chain
# rather than small pods
# -- there must be something that you can quantify, where its
# -- the thing is, the clique is across all age groups
# so when you know the network size
# and you have the contact matrix
# how to enforce that little networks emerge
# maybe the best thing to do is to start with doing it completely randomly
# and then decide?
# ****

# ****
# there is also an angle where the duration of contact needs to come into play
# right because there is a time-dimension
# where the duration of contact needs to be a function of
# the amoung of time so it amounts to the same level of contact
# ****

# ****
# also think about whether this needs to be triangular or not
# think about educator vs student
# ****

# so whats a really simple example here for this
#

library(data.table)
library(raster)

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Simulated Data
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

# *** SIMULATED DATA *****
set.seed(123)
agebreaks <- 1:3

# setup
simple_cm <- setDT(expand.grid(resp_age = agebreaks,
                         contact_age = agebreaks))

# get a contact matrix
simple_cm[, ncont := runif(nrow(simple_cm))]
simple_cm

# convert to a matrix
simple_cm_mat <- matrix(simple_cm$ncont,
          nrow = length(agebreaks),
          byrow = F)

# add 1 to the diag to make it look like a contact matrix
for(i in agebreaks) {
  for(j in agebreaks) {
    if(j + i == (agebreaks[1] + agebreaks[length(agebreaks)])) {
      simple_cm_mat[i, j] = simple_cm_mat[i, j] + 1
    }
  }
}

# *** END SIMULATED DATA *****

# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# MAKE CM triangular
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

simple_cm_mat

# enforce that it needs to be triangular
nn = length(agebreaks)
for(i in agebreaks) { # row
  for(j in agebreaks) { # column
    if(j <= (nn - i + 1)) {
      # print(paste0(i, '-', j))
      rr1 <- simple_cm_mat[i, j]
      rr2 <- simple_cm_mat[(nn - j + 1), (nn - i + 1)]
      # print(paste(round(rr1,2), round(rr2, 2)))
      rravg = mean(rr1, rr2)
      simple_cm_mat[i, j] = rravg
      simple_cm_mat[(nn - j + 1), (nn - i + 1)] = rravg
    }
  }
}

# convert to something you can plot or plot as a raster
plot(raster(simple_cm_mat),
     xlim = c(0, 1),
     ylim = c(0, 1))

# now make back to a df
simple_cm_v2 <- setDT(expand.grid(resp_age = agebreaks,
                                  contact_age = agebreaks))
simple_cm_v2$ncont = NA

simple_cm_v2

for(i in agebreaks) { # row
  for(j in agebreaks) { # column
    jact = (nn - j + 1)
    rr <- which(simple_cm_v2$resp_age == i &
                  simple_cm_v2$contact_age == jact)
    print(rr)
    simple_cm_v2$ncont[rr] = simple_cm_mat[i, j]
  }
}

simple_cm_v2

ggplot(simple_cm_v2) +
  geom_tile(aes(x = resp_age, y = contact_age, fill = ncont)) +
  scale_fill_viridis_c()


# /////////////////////////////////////////////////////////////////////////////
# =============================================================================
# Ok so now make your population
# doesn't matter if its slow, you only have to do this once
# =============================================================================
# /////////////////////////////////////////////////////////////////////////////

Ntotal = 100   # the total number of people in the population
Nnetwork = 20  # the total size of the network
# whats unknown is the clique size

#
pop_df <- data.table(pid = 1:Ntotal)

# randomly assign agebreaks
pop_df$age_fct = sample(agebreaks, Ntotal, replace = T)

# ok now how do you
# so each person gets a clique
# so i think its a matrix of 1s and 0s maybe?

# or maybe its a simulated annealing problem
# right its finding which subset of 5 stations gives you the most coverage

# no i think there is a way to just build it from a pool moving up
clique_list <- vector("list", length = Ntotal)
for(i in 1:Ntotal) {
  clique_list[[i]]$pid = pop_df$pid[i]
  clique_list[[i]]$age_fct = pop_df$age_fct[i]
  clique_list[[i]]$dist_to_sample =
    subset(simple_cm_v2, resp_age == pop_df$age_fct[i])
}

xdf <- clique_list[[1]]$dist_to_sample
xdf

xsize = clique_list[[1]]$dist_to_sample$ncont[1]
xsize_floor = floor(xsize)
xbern = xsize - xsize_floor
xrand = 1 * (runif(1) < xbern)

sample(subset(pop_df, age_fct == 1)$pid,
       size = xsize_floor + xrand,
       replace = F)

# maybe what you do is say groups of X and then assess and then just
# use that as your brute force function?



