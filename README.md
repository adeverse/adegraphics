# [adegraphics](http://pbil.univ-lyon1.fr/ADE-4/)

[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/adegraphics)](http://cran.r-project.org/package=adegraphics)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/adegraphics)](https://cran.r-project.org/package=adegraphics)
[![R-CMD-check](https://github.com/adeverse/adegraphics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adeverse/adegraphics/actions/workflows/R-CMD-check.yaml)

An S4 Lattice-Based Package for the Representation of Multivariate Data

---------------------------

**Please note! Since January 2024, this repository has belonged to the *adeverse* organization.**
To avoid confusion, we strongly recommend updating any existing local clones to point to the new 
repository URL. You can do this by using `git remote` on the command line:

`git remote set-url origin git@github.com:adeverse/adegraphics.git`

or 

`git remote set-url origin https://github.com/adeverse/adegraphics.git`

---------------------------


Read the [wiki](https://github.com/adeverse/adegraphics/wiki) for more information


Installing the development version of `adegraphics`
-------------

- Install the release version of `remotes` from CRAN with `install.packages("remotes")`.

- Make sure you have a working development environment.
    * **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
    * **Mac**: Install Xcode from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).
    
Then:

```r
remotes::install_github("adeverse/adegraphics")
```


If you do not wish to install the development environments Rtools (Windows) / XCode (Mac), we can supply binary packages of the development version of `adegraphics` on request. 



Installing the stable version of `adegraphics`
-------------

```r
install.packages("adegraphics")
```


Loading `adegraphics`
-------------

```r
library("adegraphics")
```
