OsteoSort:

A statistical osteological sorting R package for commingled human remains using metric, 2D, and 3D data.

Changes for OsteoSort version : 1.2.3




Modules:




Minor changes:
Adjusted print messages for start/finish of functions

Moderate changes:
parLapply implemented on Windows to replace Lapply



Bug fixes:

Fixed directory creation if not present for sessiontempdir
Fixed testagainstzero argument mislabeled as testagainst
Fixed measurement argument being NULL
Fixed typo with Fem_17 mislabeled as Fem_18
Fixed typo with Tib_11 mislabeled as Tib_10
Fixed Factors being introduced by forcing stringsAsFactors = FALSE within each function excplicity. Value returns to TRUE when complete This solves the memory issue assocaited with articulation and association.