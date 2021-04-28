# Purpose of this feature branch

The purpose of this feature branch is to improve performance by plotting projections (overview tab and gene expression tab) using the `scattermore` R package.
Especially with large data sets (100k), the performance gain is significant.
However, the projection is less interactive and cells can only be selected using a rectangle, not a lasso.
