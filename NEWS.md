# eiExpand 1.0.5
* updated split_precinct_analysis() to work when there are no split precincts
* edits for submitting to cran

# eiExpand 1.0.4
* updated split_precinct_analysis() to append plot_list with a map of district
with all vtds and include/exclude blocks. 
* added mean difference calculation in performance plot 
* fixed error in split_precinct_analysis when no blocks are in precinct
* added upper_thresh and lower_thresh args to split_precinct_analysis()
* fixed !covered error in percent_intersect()

# eiExpand 1.0.3
* added MT data to cover full example district
* updated coeff_plot larger text and points and includes mean labels

# eiExpand 1.0.2
* added wa bisg example data

# eiExpand 1.0.1
* added authors to functions
* updated examples using ci_cvap_full() to use rpv_normalize()

# eiExpand 1.0.0
Transfer CRinternal to new name eiExpand
* added rpv_coef_plot() function
* added examples to split_precinct_analysis()
* changed package data names 


# CRInternal 0.2.0
* change function name RPV_toDF to rpv_toDF for package naming consistency 
* updated package internal data
* update examples in ci_cvap_full() and rpv_toDF() using new package data
* change output of ci_cvap_full() to match eiCompare rpv output. This simplifies
rpv_toDF()
* Update rpv_toDF() to work with new ci_cvap_full naming. 
* Update rpv_toDF() argument 'model' to allow any character string. Before this
argument was used to determine which column names to use but with the update of ci_cvap_full() this argument can be anything the user wants to describe the rpv 
model used for the rpv_results argument (Ex "ei cvap").
* adjusted how rpv_toDF() creates initial dataframe from ei/rxc results.
* Added split_precinct_analysis() and supporting function percent_intersect()
to replace previous split prec anaysis functions. Now, entire analysis can be done
using only split_precinct_analysis. In doing so, added additional qc measures 
in output


# CRInternal 0.1.0
Startup
initial functions
