# gen_cloglog
A class of link functions that generalize the cloglog link function. This leads to better pure fit to the data than the cloglog link and it can be tested for significance simply through a likelihood ratio test. See the vignette for details.

link_fun.R contains the function to use the link and examples of its application on data.

fitplots.R contains plots of the cloglog vs. generalized cloglog fitted predicted probabilities on the menarche and bacteria datasets.

link graphs.R plots the generalized link function for different values of a and b.
