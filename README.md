OsteoSort:

A statistical osteological sorting R package for commingled human remains using metric, 2D, and 3D data.

Changes for OsteoSort version 1.1.0:

API chhanges:
1. Renamed lengthsort function to metric sort. This reflects the use of more than maximum length measurements.
2. Merged plot arguments into output_options.
3. Removed plotme calls from OsteoSort and crated plots for all iterations of analytics if specified in output_options.
4. Removed stdout argument in favor of using output_options.
5. Added tolower cleanup of string arguments.
6. Added new function for pair, articulation, and association output files to .csv.

Bug fixes:
1. Fixed null n_regions and dist arguments in match.2d.invariant
2. Fixed typo for excluded to exclude.
3. Changed variable name ID from template and argument input to lowercase id to avoid excel bug with csv sylk file error.

Methods:
1. Stable 2D analytical functions are now released.

Dependency changes:
1. Removed doSnow and foreach dependencies

Document changes:
Updated all man pages for argument changes.
