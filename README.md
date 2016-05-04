# ArchaeologicalFloors

This R-package contains data and code to the to reproduce the results in the
[*Archaeological Floors* paper](http://dx.doi.org/10.1016/j.jas.2016.04.016)
[1]. If you don't have access to the journal paper, a preprint is available at 
[Zenodo](http://dx.doi.org/10.5281/zenodo.34521).

In the paper we used [geoRcb](https://github.com/famuvie/geoRcb) to perform a 
cost-based kriging prediction of chemical residues in the soil.

This package contains a vignette with all the code necessary to reproduce the 
results from the paper. You can browse the code 
[online](https://rawgit.com/famuvie/ArchaeologicalFloors/master/inst/doc/Results_paper.html).

To browse offline or to play with the 
data yourself, install the package locally as follows.

```r
devtools:::install_github('famuvie/ArchaeologicalFloors')
library(ArchaeologicalFloors)
vignette('Results_paper')
data(Jandhala)
```

Note that the package includes in separate files some helper functions for
[plotting](R/ggplot_helpers.R) and for performing the
[Cross-Validation](R/loocv_helper.R) excercise.


[1] J. Negre, F. Muñoz, C. Lancelotti (2016). Geostatistical modelling of 
chemical residues on archaeological floors in the presence of barriers. Journal 
of Archaeological Science 70 (2016) 
91--101
