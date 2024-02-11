# TREX <img src="man/figures/trex_logo.png" align="right" width = "150"/>


<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![R-CMD-check](https://github.com/the-Hull/TREX/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/the-Hull/TREX/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


`TREX` allows to assimilate, process and analyse sap flow data obtained with the thermal dissipation method (TDM). 
The package includes functions for gap filling time-series data, detecting outliers, calculating data-processing uncertainties and generating uniform data output and visualisation.
The package is designed to deal with large quantities of data and apply commonly used data-processing methods. 
The functions have been validated on data collected from different tree species across the northern hemisphere [(Peters et al. 2018 <doi: 10.1111/nph.15241>)](https://doi.org/10.1111/nph.15241), and
an accompanying manuscript has been published in *Methods in Ecology and Evolution* as [(Peters et al. 2020 <doi: 10.1111/2041-210X.13524>)](https://doi.org/10.1111/2041-210X.13524)

---

## 1. Installation

The latest version of `TREX` can be installed and used via

```r
remotes::install_github("the-Hull/TREX")

library(TREX)

```
If you want to use `CRAN`, we have a stable release version used for the *MEE* manuscript available named `TREXr`:

```r
install.packages("TREXr")
```

## 2. Basic use and workflow


### Load data

```r
# load raw data
raw   <- is.trex(example.data(type="doy"),
                 tz="GMT",
                 time.format="%H:%M",
                 solar.time=TRUE,
                 long.deg=7.7459,
                 ref.add=FALSE)
                 
# adjust time steps
input <- dt.steps(input=raw, 
                start="2013-05-01 00:00",
                end="2013-11-01 00:00",
                time.int=15,
                max.gap=60,
                decimals=10,
                df=FALSE)
                
# remove obvious outliers
input[which(input<0.2)]<- NA


```

### Calculate maximum &Delta;T-Values

Three methods can be applied to calculate &Delta;T (or &Delta;V for voltage differences between TDM probes):  

- `pd`: pre-dawn
- `mw`: moving-window
- `dr`: double-regression

```r
input <- tdm_dt.max(input,
                    methods = c("pd", "mw", "dr"),
                    det.pd = TRUE,
                    interpolate = FALSE,
                    max.days = 10,
                    df = FALSE)
                    
plot(input$input, ylab = expression(Delta*italic("V")))

lines(input$max.pd, col = "green")
lines(input$max.mw, col = "blue")
lines(input$max.dr, col = "orange")

```
![](man/figures/dtmax.png)

### Calculate Sap Flux Density


```r

output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=FALSE,wood="Coniferous")

plot(output.data$sfd.pd$sfd[1:1000, ], ylim=c(0,10))
# see estimated uncertainty
lines(output.data$sfd.pd$q025[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$q975[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$sfd[1:1000, ])

sfd_data <- output.data$sfd.dr$sfd


```
![](man/figures/sfd.png)


### Generate Outputs 

Here we generate outputs based on environmental filters and calculate crown conductance (G<sub>c</sub>) values.


```r
output<- out.data(input=sfd_data,
                  vpd.input=vpd, 
                  sr.input=sr,
                  prec.input=preci,
                  low.sr = 150,
                  peak.sr=300, 
                  vpd.cutoff= 0.5, 
                  prec.lim=1,
                  method="env.filt", 
                  max.quant=0.99, 
                  make.plot=TRUE)

```

![](man/figures/output.png)

## 3. More on `TREX`

### Workshops using `TREX`

- **ESA 2020**: `TREX` was introduced and demonstrated in detail in a workshop during the Ecological Society of America's 2020 AGM.
The workshop description can be found [here](https://deep-tools.netlify.app/talk/esa-2020-rpeters-cpappas/), and all materials on the [dedicated page](https://deep-tools.netlify.app/docs-workshops/esa-workshop2020/02_trex/).

## 4. Citing this work

Please cite `TREX` when you apply it in your own work as:

>  Peters, RL, Pappas, C, Hurley, AG, et al. Assimilate, process and analyse thermal dissipation sap flow data using the TREX r package. Methods Ecol Evol. 2021; 12: 342– 350. https://doi.org/10.1111/2041-210X.13524 

A reference is available in `R` using:

```r

citation("TREX")

#or

citation("TREXr")
```


