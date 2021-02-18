[![Travis-CI Build Status](https://travis-ci.org/sdray/adegraphics.svg?branch=master)](https://travis-ci.org/sdray/adegraphics)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/y3771xk9a4obepas/branch/master?svg=true)](https://ci.appveyor.com/project/sdray/adegraphics/branch/master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/adegraphics)](http://cran.r-project.org/package=adegraphics)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/adegraphics)](https://cran.r-project.org/package=adegraphics)
[![R-CMD-check](https://github.com/sdray/adegraphics/workflows/R-CMD-check/badge.svg)](https://github.com/sdray/adegraphics/actions)


# [adegraphics](http://pbil.univ-lyon1.fr/ADE-4/)
An S4 Lattice-Based Package for the Representation of Multivariate Data


Read the [wiki](https://github.com/sdray/adegraphics/wiki) for more information

Installing *adegraphics*
-------------
To install the development version from github:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
    * **Mac**: Install Xcode from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).
    
Then:

```r
library(devtools)
install_github("sdray/adegraphics")
```

The stable version can be installed from CRAN using:

```r
install.packages("adegraphics")
```

Once installed, the package can be loaded using:

```r
library("adegraphics")
```

If you do not wish to install the development environments Rtools (Windows) / XCode (Mac), you can get the binary packages here:

* [**Windows**](http://pbil.univ-lyon1.fr/members/thioulouse/bin/windows/)

* [**macOS**](http://pbil.univ-lyon1.fr/members/thioulouse/bin/macosx/)
