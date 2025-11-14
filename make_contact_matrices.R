library(ggplot2)

set.seed(123)

n <- 20
i <- 1:n
j <- 1:n

## 1. Base diagonal band (correlation decays with distance from diagonal)
decay_scale <- 6
band_mat <- outer(i, j, function(a, b) exp(-abs(a - b) / decay_scale))

## 2. Trend so bottom-left is strongest
trend_mat <- outer(i, j, function(a, b) {
  ((n + 1 - a) * (n + 1 - b)) / n^2  # big at small (a, b)
})

signal_mat <- band_mat * trend_mat

## 3. 2-D correlated noise (smooth random field)
noise_raw <- matrix(rnorm(n * n), n, n)

# simple 3x3 Gaussian-ish kernel
kernel <- matrix(c(1, 2, 1,
                   2, 4, 2,
                   1, 2, 1), nrow = 3, byrow = TRUE)
kernel <- kernel / sum(kernel)

smooth2d <- function(mat, kern) {
  pad <- floor(nrow(kern) / 2)
  nr  <- nrow(mat); nc <- ncol(mat)

  # zero-padded extension
  ext <- matrix(0, nr + 2 * pad, nc + 2 * pad)
  ext[(pad + 1):(pad + nr), (pad + 1):(pad + nc)] <- mat

  out <- matrix(0, nr, nc)
  for (r in 1:nr) {
    for (c in 1:nc) {
      sub <- ext[r:(r + 2 * pad), c:(c + 2 * pad)]
      out[r, c] <- sum(sub * kern)
    }
  }
  out
}

noise_smooth <- smooth2d(noise_raw, kernel)

## 4. Let noise be a bit different in each half around the diagonal
lower_mask <- row(noise_smooth) > col(noise_smooth)
upper_mask <- !lower_mask

noise_smooth[lower_mask] <- noise_smooth[lower_mask] * 0.8
noise_smooth[upper_mask] <- noise_smooth[upper_mask] * 1.2

## 5. Final matrix
mat <- signal_mat + 0.2 * noise_smooth
mat <- mat - min(mat)
mat <- mat / max(mat)  # scale to [0, 1]

## 6. Plot with ggplot
dnn <- levels(cut(0:100, breaks = n, include.lowest = T))
row.names(mat) <- dnn
colnames(mat) <- dnn
df <- as.data.frame(as.table(mat))
names(df) <- c("i", "j", "z")

ggplot(df, aes(i, j, fill = z)) +
  geom_tile() +
  scale_fill_gradientn(colors = blues9) +
  coord_fixed() +
  theme_minimal()
