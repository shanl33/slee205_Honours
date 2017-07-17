X_sphere <- sphere(crabs[,4:8])

# 5 guided holes 2D tours w crabs data. See help for path_dist()-----------------------------
tries <- replicate(5, save_history(X_sphere, guided_tour(holes, d=2, max.tries = 50), max = 50),
                   simplify = FALSE)
tries_proper <- lapply(tries, interpolate, 0.2)
bases_proper <- unlist(lapply(tries_proper, as.list), recursive = FALSE)
class(bases_proper) <- "history_list"
index_values <- paths_index(tries_proper, holes) # calculates index value
d_proper <- path_dist(bases_proper)
ord_proper <- as.data.frame(cmdscale(d_proper, 2))
info_proper <- cbind(ord_proper, index_values)

qplot(V1, V2, data = info_proper, geom="path", group = try) +
  #geom_point(aes(size = value)) +
  coord_equal() +
  scale_x_continuous(limits = c(-1, 1.5)) +
  scale_y_continuous(limits = c(-1.2, 1)) 
# Index plot
qplot(step, value, data = info, geom="line", group = try)

# tries1 has initial orthogonal proj retained
orthog_init <- function(t) {
  t0 <- t[,,1]
  t0[,,1] <- matrix(c(1, rep(0, 5), 1, rep(0, 3)), ncol = 1)
  t1 <- abind(t, t0, along = 3)
  class(t1) <- "history_array"
  t1
}
tries1 <- lapply(tries, orthog_init)
attr(tries1, "data") <- X_sphere
tries2 <- lapply(tries1, interpolate, 0.2)
bases2 <- unlist(lapply(tries2, as.list), recursive = FALSE)
class(bases2) <- "history_list"

# No proj pursuit fn index calculated, only "try" 1 to 5 (path #)
try_index <- c()
for (i in 1:length(tries2)) {
  try_index <- c(try_index, rep(i, dim(tries2[[i]])[3]))
}
d2 <- path_dist(bases2)
ord2 <- as.data.frame(cmdscale(d2, 2)) #V1 and V2 values
info2 <- cbind(ord2, try=try_index)

qplot(V1, V2, data = info2, geom="path", group = try) +
  coord_equal() +
  scale_x_continuous(limits = c(-1, 1.5)) +
  scale_y_continuous(limits = c(-1.2, 1)) 

# The paths are represented on a 2D plot using multi-dimensional scaling (cmdscale()) of the "distances" between two planes.
# The "principal angles" between two planes is used to represent the distance between two projections.

# ggobi tour path trace visualisation (not tried yet)
x <- path1d_ggobi(tries[[1]]) # trace of first tour
ggobi(x)
