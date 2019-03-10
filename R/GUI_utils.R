## Input init function
#
# CSV
#   either basic csv (no current loader)
#   loadParamCSV (with all the uROI... and checks)
#
# -> either new fuction
#       double read when passing to loadParamCSV
#       secret pass the data to loadParamCSV
# -> extend LoadParamCSV
#       recreate csv with new columns (berk)
#       detect which format at file read, treat it all inside
#
# filepath
#   get a list of path from the UI
#
# metadata (no checks, just length, allowed not to be here)
#   cpd metadata
#   spl metadata
#
