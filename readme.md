# *s*patial *seg*regation, or interaction, summaries for multitype point patterns

Emphasis on **multi**-type, more than 2.

This is a reboot of the old `spatialsegregation` package originally conceived 2008. It doesn't have all the functionality yet, at the moment we have

* (inhomogeneous) cross-type nearest neighbour cdf
* (inhomogeneous) ISAR
* Intensity estimation with automatic bandwidth selection
* Dixon's test

To install the vignette that demonstrates how to compute ISAR's install the package using

Planned features:

* Spatial versions of Simpson and Shannon indices
* Mingling index
* Mean composite information
* Models for hypothesis testing:
    * inhomogeneous random superposition
    * inhomogeneous random marking
    * Product shot noise Cox process


```
library(devtools)
install_github('antiphon/sseg', build_vignettes = TRUE)
```

and then

```
library(sseg)
vignette("ISAR")
```
