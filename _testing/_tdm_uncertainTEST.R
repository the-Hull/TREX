# tdm_uncertain -----------------------------------------------------------

library(TREX)



raw   <- example.data(type="doy")
input <- is.trex(raw, tz="GMT", time.format="%H:%M",
                 solar.time=TRUE, long.deg=7.7459, ref.add=FALSE, df=FALSE)
input<-dt.steps(input,time.int=15,start="2013-04-01 00:00",
                end="2013-11-01 00:00",max.gap=180,decimals=15)
output<- tdm_uncertain(input, probe.length=20, method="pd",
                       n=2000,sw.cor=32.28,sw.sd=16,log.a_mu=3.792436,
                       log.a_sd=0.4448937,b_mu=1.177099,b_sd=0.3083603,
                       make.plot=TRUE)


