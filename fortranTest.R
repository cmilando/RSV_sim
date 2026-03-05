
# remember that the most common cause of failure and abort
# is misconfiguration of inputs in this call
# either you:
# -- are missing one
# -- or its real when it should be an integer
# -- or the order isn't correct

# the best way to work with this is to open these files in Rstudio but
# run from the terminal so if it crashes you don't have to start over


system("rm *.o")
system("rm *.so")
system("R CMD SHLIB rsv.f90")

# load the library and create a tmp directory
# dyn.unload("rsv.so")
dyn.load("rsv.so")

# lets test ransam
set.seed(123)
oo <- .Fortran("ransam",
               x = as.integer(as.numeric(1:100)),
               subx = as.integer(rep(0, 20)),
               n = as.integer(100),
               k = as.integer(20))

# have to pass in the thing you want to get out
# df, nrows, ncols, age_col, age_lb, age_ub, rr
set.seed(123)
nrows = as.integer(10)
ncols = as.integer(4)
df <- matrix(rnorm(nrows * ncols), nrow = nrows, ncol = ncols)

# set id
df[, 1] <- 100 + 1:nrows
df

# set age
age_col = as.integer(2)
df[, 2] <- c(rep(20, 5), rep(50, 5))
df
age_lb = 10
age_ub = 25
df

# set zerocol
zero_col = as.integer(3)
df[, 3] <- 0
df

# df[sample(1:nrows, 20), 3] = 0
rr = rep(as.integer(0), times = nrows)
n = as.integer(5)
xcontinue = 1
ids <- rnorm(nrows)
ids_size <- as.integer(nrows)

df
ids

# remember that the most common cause of failure and abort
# is misconfiguration of inputs in this call
# either you:
# -- are missing one
# -- or its real when it should be an integer
# -- or the order isn't correct
oo <- .Fortran("get_ids",
               df        = df,
               nrows     = nrows,
               ncols     = ncols,
               age_col   = age_col,
               age_lb    = age_lb,
               age_ub    = age_ub,
               zero_col  = zero_col,
               rr        = rr,
               n         = n,
               xcontinue = xcontinue,
               ids       = ids,
               ids_size  = ids_size)

oo$rr  # things that match
oo$ids # this group
oo$ids_size
table(oo$rr)
df[which(oo$rr == 1),]


## OK FINALLY
# df, nrows, ncols,  age_col, vec, zero_col, &
#   & p1, p2, age0, age1, age2
oo <- .Fortran("set_ids",
               df = df,
               nrows = nrows,
               ncols = ncols,
               age_col = age_col,
               vec = as.integer(rep(5, nrows)),
               zero_col = as.integer(3),
               p1 = 0.2,
               p2 = 0.8,
               age0 = 0,
               age1 = 30,
               age2 = 125)

oo$df
