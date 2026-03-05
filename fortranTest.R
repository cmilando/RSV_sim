
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
nrows = as.integer(100)
ncols = as.integer(4)
df <- matrix(rnorm(100 * 4), nrow = nrows, ncol = ncols)
df[sample(1:nrows, 20), 3] = 0
df[, 1] <- 100 + 1:nrows
age_col = as.integer(2)
age_lb = 0
age_ub = 1
rr = rep(as.integer(0), times = nrows)
n = as.integer(20)
xcontinue = 1
ids <- rnorm(100)
ids_size <- as.integer(100)

# remember that the most common cause of failure and abort
# is misconfiguration of inputs in this call
# either you:
# -- are missing one
# -- or its real when it should be an integer
# -- or the order isn't correct
oo <- .Fortran("get_ids",
               df = df,
               nrows = nrows,
               ncols = ncols,
               age_col = age_col,
               age_lb = age_lb,
               age_ub = age_ub,
               zero_col = as.integer(3),
               rr = rr,
               n = n,
               xcontinue = xcontinue,
               ids = ids,
               ids_size = ids_size)

cat("IDS\n")
oo$ids

cat("IDsize\n")
oo$ids_size

cat("rr\n")
oo$rr

table(oo$rr)
df[which(oo$rr == 1),]
