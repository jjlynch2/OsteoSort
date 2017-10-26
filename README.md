OsteoSort:

A statistical osteological sorting R package for commingled human remains using metric, 2D, and 3D data.

Changes for OsteoSort version 1.2.1:


Modules:

Added a new module to test the strength of evidence for an antemortem stature to postmortem measurement


Minor changes:

Added ability to switch output metric between inches, millimeters, and centimeters for the statsort function.

Adjusted resolution of graphs to 400x400.

Re-ran roxygenize for man pages.

Adjusted reference to previous code by Julien Claude (Morphometrics with R 2008). 


Bug fixes:

Fixed Trotter data in outlier analysis for the over correction by 10 mm

Standard error was estimated slightly wrong for the t-statistic in regression association. This has been fixed.
