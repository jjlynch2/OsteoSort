OsteoSort:

A statistical osteological sorting R package for commingled human remains using metric, 2D, and 3D data.

Changes for OsteoSort version 1.2.2:



Modules:

Updated 2D to include fragmentary shape registration and pair-matching

Added pca_align() function to return rotated PCA scores for alignment prior to 3D analysis

Added match.3d() function (currently broken) for 3D pair-matching



Minor changes:

Momocs dependency removed: replaced EFA from Momocs with modified version for 2D and 3D EFA.

Pracma dependency removed: replaced distmat from pracma with modified version.

Shapes dependency removed: removed Procrustes distance function call as an option from 2D settings.

Moved code for matching matrices names and alpha color for plots into two internal functions.



Moderate changes:

Renamed segmented_hausdorff_dist() function to hausdorff_dist()

Renamed match.2d.invariant() function to match.2d()

Re-wrote the base code for match.2d() to be more efficient with looping for distance calculations.

Re-wrote the base code for write.TPS to use lists with unequal landmarks and 3D matrices with equal landmarks.

Re-wrote all file output into output_options() including TPS, excel, and plots.



Bug fixes:

Swapped axes for the antemortem plot (does not change result)
