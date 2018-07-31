
# mlisi
R package with my frequently used functions  and helpers

Set of miscellaneous functions to analyze psychophysical data, basic mixture-models and signal processing, etc. etc. 

If you want to use it, to install type in R terminal:
```
library(devtools)
install_github("mattelisi/mlisi")
```
Documentation is not exaustive...

To get started and see a list of the functions included, use
```
help(package=mlisi)
```
For example, the function `lnorm_3par_multi()` compute the loglikelihood of a psychometric function with symmetric lapse rate constrained to be constant across conditions. This makes sense if the conditions are interleaved. There are even some examples (!), see `?lnorm_3par_multi`.