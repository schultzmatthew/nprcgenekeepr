
[![Rdoc](http://www.rdocumentation.org/badges/version/roxygen2)](http://www.rdocumentation.org/packages/roxygen2)
[![Build
Status](https://travis-ci.org/rmsharp/nprcmanager.svg?branch=master)](https://travis-ci.org/rmsharp/nprcmanager)
[![codecov](https://codecov.io/gh/rmsharp/nprcmanager/branch/master/graph/badge.svg)](https://codecov.io/gh/rmsharp/nprcmanager)
[![Rdoc](http://www.rdocumentation.org/badges/version/RDocumentation)](http://www.rdocumentation.org/packages/RDocumentation)
[![Rdoc](http://www.rdocumentation.org/badges/version/nprcmanager)](http://www.rdocumentation.org/packages/gh/rmsharp/nprcmanager)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nprcmanager – Version 0.4.22 (20181224)

## Introduction

The goal of **nprcmanager** is to implement Genetic Tools for Colony
Management. It was initially conceived and developed as a Shiny web
application at the Oregon National Primate Research Center (ONPRC) to
facilitate some of the analyses they perform regularly. It is currently
being enhanced to have more capability as a Shiny application and to
expose the functions so they can be used either interactively or in R
scripts.

<!--It is now managed and maintained as a joint effort between ONPRC-->

<!--and Southwest National Primate Research Center (SNPRC) with the -->

<!--coding being done by R. Mark Sharp, Ph.D.-->

At present, the application supports 5 functions:

``` 
1. Quality control of uploaded studbooks  
2. Creation of pedigrees from a list of potential breeders and LabKey EHR integration  
3. Generation of Genetic Value Analysis Reports  
4. Creation of potential breeding groups  
5. Display of an age by sex pyramid plot  
```

**For more information see:**  
A Practical Approach for Designing Breeding Groups to Maximize Genetic
Diversity in a Large Colony of Captive Rhesus Macaques (*Macaca
mulatto*) Vinson, A ; Raboin, Mj *Journal Of The American Association
For Laboratory Animal Science*, 2015 Nov, Vol.54(6), pp.700-707 \[Peer
Reviewed Journal\]

## Installation

You can install **nprcmanager** from github with:

``` r
install.packages("devtools")
devtools::install_github("rmsharp/nprcmanager")
```

All missing dependencies should be automatically installed.

## Summary of Major Functions

### Quality Control

Studbooks maintained by breeding colonies generally contain information
of varying quality. The quality control functions of the toolkit check
to ensure all animals listed as parents have their own line entries, all
parents have the appropriate sex listed, no animals are listed as both a
sire and a dam, duplicate entries are removed, pedigree generation
numbers are added, and all dates are valid dates. In addition, exit
dates are added if possible and are consistent with other information
such as departure dates and death dates. Current ages of animals that
are still alive are added if a database connection is provided via a
configuration file and the user has read permission on a LabKey server
with the demographic data in an **EHR** (Electronic Health Record)
module. See

Parents with ages below a user selected threshold are identified. A
minimum parent age in years is set by the user and is used to ensure
each parent is at least that age on the birth date of an offspring. The
minimum parent age defaults to 2 years. This check is not performed for
animals with missing birth
dates.

### Creation of Pedigree From a List of Potential Breeders and LabKey Integration

The user can enter a list of breeders in a CSV file that will be used to
create a pedigree containing all direct relative (ancestors and
descendants) via the **labkey.selectRows** function within the
**Rlabkey** package if a database connection is provided via a
configuration file and the user has read permission on a LabKey server
with the demographic data in an **EHR** (Electronic Health Record)
module.

Two configuration files are needed to use the database features of
nprcmanager with LabKey. The first file is named **\_netrc** on
Microsoft Windows operating systems and **.netrc** otherwise, allows the
user to authenticate with LabKey through the LabKey API and is fully
described by [LabKey
documentation](https://www.labkey.org/Documentation/wiki-page.view?name=netrc)

The second file is named **\_nprcmanager\_config** on Microsoft Windows
operating systems and **.nprcmanager\_config** otherwise and is the
nprcmanager [configuration
file](https://github.com/rmsharp/nprcmanager/blob/master/inst/extdata/example_nprcmanager_config)
An image of this example configuration file is included as a data object
and can be loaded and viewed with the following lines of R code in the R
console.

``` r
data("exampleNprcmanagerConfig")
View(exampleNprcmanagerConfig)
```

### Genetic Value Analysis Reports

The Genetic Value Analysis is a ranking scheme developed at ONPRC to
indicate the relative breeding value of animals in the colony. The
scheme uses the mean kinship for each animal to indicate how
inter-related it is with the rest of the current breeding colony
members. Genome uniqueness is used to provide an indication of whether
or not an animal is likely to possess alleles at risk of being lost from
the colony. Under the scheme, animals with low mean kinship or high
genome uniqueness are ranked more highly.

### Breeding Group Formation

One of the goals in breeding group formation is to avoid the potential
for mating of closely related animals. Since behavioral concerns and
housing constraints will also be taken into account in the group
formation process, it is our goal to provide the largest number of
animals possible from a list of candidates that can be housed together
without risk of consanguineous mating. To that end, this function uses
information from the Genetic Value Analysis to search for the largest
combinations of animals that can be produced from a list of candidates.

## Running Shiny Application

The toolset available within nprcmanager can be used inside standard R
scripts. However, it was orginally designed to be used within a Shiny
application that can be started with:

``` r
library(nprcmanager)
runManager()
```

Find online documentation at <https://rmsharp.github.io/nprcmanager/>.

**For more information see:**  
A Practical Approach for Designing Breeding Groups to Maximize Genetic
Diversity in a Large Colony of Captive Rhesus Macaques (*Macaca
mulatto*) Vinson, A ; Raboin, Mj *Journal Of The American Association
For Laboratory Animal Science*, 2015 Nov, Vol.54(6), pp.700-707 \[Peer
Reviewed Journal\]
