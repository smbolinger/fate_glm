# Sample matrix
mat <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("row1", "row2"), c("colA", "colB", "colC")))

# Named vector with names matching matrix columns
vec_row <- c(7, 8, 9)
# names(vec_row) <- c("colB", "colA", "colC")
names(vec_row) <- c("colA", "colB", "colC")

# Combine the vector as a new row
combined_mat_row <- rbind(mat, new_row = vec_row)
print(combined_mat_row)
