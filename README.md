OsteoSort:

A statistical osteological sorting R package for commingled human remains using metric, 2D, and 3D data.

Changes for OsteoSort version 1.2.2:




Modules:

Updated 2D to include fragmentary shape registration and pair-matching.


Added match.3d() function (currently broken) for 3D pair-matching.




Minor changes:

Momocs dependency removed: replaced EFA from Momocs with modified version for 2D and 3D EFA.

Pracma dependency removed: replaced distmat from pracma with modified version.

Shapes dependency removed: removed Procrustes distance function call as an option from 2D settings.

Moved code for matching matrices names and alpha color for plots into two internal functions.




Moderate changes:

Renamed segmented_hausdorff_dist() function to hausdorff_dist().

Renamed match.2d.invariant() function to match.2d().

Re-wrote the base code for match.2d() to be more efficient with looping for distance calculations.

Re-wrote the base code for write.TPS to use lists with unequal landmarks and 3D matrices with equal landmarks.

Re-wrote all file output into output_options() including TPS, excel, and plots.

Added pca_align() function to return rotated PCA scores for alignment prior to 3D analysis

Re-wrote the backend for Hausdorff with RcppParallel to use parallel processing for large point clouds.
max_directional_hausdorff_rcpp(), mean_directional_hausdorff_rcpp(), dilated_directional_hausdorff_rcpp()

Added new functions, fragment_margins(), remove_fragment_margins(), and minimum_euclidean_deistances_ind() , which are ussed to identifyssss the indices of fragment margins along a orderly 2D outline and remove any non-overlapping landmark from the calculation of Hausdorff distances.

The regression model used for antemortem stature association has been switched to calculate the point estimate of the measurement rather than vice versa. 

Added statistical option to change from 1 to 2 tail t-tests.

Switched terminology from cores to threads for functions.



Bug fixes:


